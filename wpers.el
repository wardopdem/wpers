;;; wpersmode.el --- minor mode for stopping cursor jumping in emacs

;; Copyright (C) 2015 <wardopdem@gmail.com>

;; Authors:         wardopdem@gmail.com
;; Version:         2.0.0
;; URL:             <https://github.com/wardopdem/wpers>
;; Keywords:        persistent, cursor

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This minor mode for people hating cursor's jumping.
;; Just turn `wpers-mode` and cursor will be moving
;; with saving his position at the line (column).

;;; Installation & using:

;; Just add wpers.el folder location to load-path and call require (f.e. in your init)

;;         (add-to-list 'load-path <path-to-wpers>)
;;         (require 'wpers)

;; Then call command `wpers-mode` for toggle mode

;;         M-x wpers-mode

;;; Code:

(defconst wpers-overloaded-funs
  [next-line previous-line
   left-char right-char backward-delete-char-untabify
   move-end-of-line move-beginning-of-line
   scroll-up scroll-down] 
  "Functions overloaded by the mode")

(defconst wpers-fun-prefix "wpers-" "Prefix for new functions")

(defconst wpers-pspace 183 "Pseudo-space - char for showing in the overlay instead of real spaces") ; try 183 for explicitness

(defun wpers--intern (x &optional priv) (intern (concat wpers-fun-prefix (when priv "-")(symbol-name x))))

(defconst wpers-funs-alist
  (mapcar '(lambda (x) (cons x (wpers--intern x))) wpers-overloaded-funs)
  "alist (old . new) functions")

(defconst wpers-mode-map
  (reduce '(lambda (s x) (define-key s (vector 'remap (car x)) (cdr x)) s)
          wpers-funs-alist :initial-value (make-sparse-keymap))
  "Mode map for `wpers'")

(defconst wreps-hooks-alist
  '((pre-command-hook  . wpers--pre-command-hook)
    (post-command-hook . wpers--post-command-hook)
    (auto-save-hook    . wpers-kill-ovr)
    (before-save-hook  . wpers-kill-ovr))
  "alist (hook-var . hook-function)")

(define-minor-mode wpers-mode
  "Toggle persistent cursor mode."
  :init-value nil
  :lighter " wpers"
  :group 'wpers
  :keymap wpers-mode-map
  (if wpers-mode
      (progn
        (message "Wpers enabled")
        (setq-local wpers-overlay nil)
        (mapc '(lambda (x) (add-hook (car x) (cdr x) nil t)) wreps-hooks-alist))
      (progn
        (message "Wpers disabled")         
        (wpers-kill-ovr)
        (mapc '(lambda (x) (remove-hook (car x) (cdr x) t)) wreps-hooks-alist))))

(defun wpers--propz-ovr-txt (txt)
  (if (or hl-line-mode global-hl-line-mode)
      (propertize txt 'face (list :background (face-attribute 'highlight :background)))
      txt))

(defun wpers-make-ovr (&optional prop val)
  (wpers-kill-ovr)
  (setq wpers-overlay (make-overlay (point) (point)))
  (overlay-put wpers-overlay 'wpers t)
  (if (and prop val) (overlay-put wpers-overlay prop val)))

(defun wpers-ovr-at-point-p () "Return t if wpers-overlay was placed at the point"
  (eq (point) (overlay-start wpers-overlay)))

(defun wpers-txt-after-ovr-p ()
  (when wpers-overlay
    (let ((ch (char-after (overlay-start wpers-overlay))))
      (and ch (not (eq ch 10))))))

(defun wpers--ovr-to-spcs ()
  (let ((ovr-size (when (wpers-ovr-at-point-p) (length (wpers-ovr-get)))))
    (save-excursion
     (goto-char ov-pos)
     (insert (make-string (length (wpers-ovr-get)) 32)))
    (when ovr-size (right-char ovr-size))))
  
(defun wpers-kill-ovr ()
  (when wpers-overlay
    (let* ((ov-pos (overlay-start wpers-overlay))
           (ch (char-after ov-pos)))
      (when (and ch (not (eq ch 10))) (wpers--ovr-to-spcs)))
    (delete-overlay wpers-overlay)
    (setq wpers-overlay nil)))

(defmacro wpers-ovr-get (&optional prop)
  "Get wpers-overlay property"
  (let* ((prop (or prop 'before-string)))
    `(overlay-get wpers-overlay ',prop)))

(defmacro wpers-ovr-put (prop val)
  "Set wpers-overlay property PROP to VAL within context where variable _ is set to (wpers-ovr-get PROP)"
  `(let ((_ (wpers-ovr-get ,prop)))
    (overlay-put wpers-overlay ',prop ,val)))

(defun wpers-current-column ()
  "Same as `current-column' but it takes account size of overlay if it present"
  (let ((res (current-column)))
    (if (and wpers-overlay (wpers-ovr-at-point-p))
        (+ res (length (wpers-ovr-get)))
        res)))

(defun wpers-move-to-column (col)
  "Same as `move-to-column' but adds overlay for correction cursor position in the line (column)"
  (move-to-column col)
  (let* ((last-column (- (line-end-position) (line-beginning-position)))
         (spcs-needed (- col last-column)))
    (when (plusp spcs-needed)
      (wpers-make-ovr 'before-string (wpers--propz-ovr-txt (make-string spcs-needed wpers-pspace))))))

(defmacro wpers-save-vpos (form) "Eval form with saving current cursor's position in the line (column)"
  (let ((old-col (make-symbol "old-col")))
    `(let ((,old-col (wpers-current-column))) ,form (wpers-move-to-column ,old-col))))

(defmacro wpers--def-remap-fun (org-fun doc-str &rest body)
  `(defun ,(wpers--intern org-fun) () ,doc-str ,@body))

(defmacro wpers--def-vert (name doc-str expr) "Auxiliary macro for defining commands that do vertical cursor movement"
  `(wpers--def-remap-fun ,name ,doc-str (interactive) (message "!") (wpers-save-vpos ,expr)))

(wpers--def-vert next-line "Same as `new-line' but adds the overlay if it's needed
for saving cursor's position in the line (column)"
  (next-line))

(wpers--def-vert previous-line "Same as `previous-line' but adds the overlay if it's needed
for saving cursor's position in the line (column)"
  (previous-line))

(wpers--def-vert scroll-up "" (cua-scroll-up))

(wpers--def-vert scroll-down "" (cua-scroll-down))

(wpers--def-remap-fun right-char "Same as `right-char' but adds the overlay if cursor at end of line (column)"
  (interactive)
  (let ((ca (char-after)))
    (if (or (null ca) (eq ca 10))
        (if (null wpers-overlay)
            (wpers-make-ovr 'before-string (wpers--propz-ovr-txt (string wpers-pspace)))
            (if (wpers-ovr-at-point-p)
                (wpers-ovr-put before-string (wpers--propz-ovr-txt (concat _ (string wpers-pspace))))
                (wpers-kill-ovr) (wpers-make-ovr 'before-string (wpers--propz-ovr-txt (string wpers-pspace)))))
        (wpers-kill-ovr) (right-char))))

(defmacro wpers--def-left (name doc-str expr)
  "Auxiliary macro for defining commands that do left cursor movement"
  `(wpers--def-remap-fun ,name 
     ,doc-str
     (interactive)
     (if wpers-overlay
         (if (and (wpers-ovr-at-point-p) (eq (char-after) 10))
             (if (plusp (length (wpers-ovr-get)))
                 (wpers-ovr-put before-string (wpers--propz-ovr-txt (substring _ 1)))
                 (wpers-kill-ovr) ,expr)
             (wpers-kill-ovr) ,expr)
         ,expr)))

(wpers--def-left left-char
                 "Same as `left-char' but performs correcting or deleting the overlay if it's needed" 
                 (left-char))

(wpers--def-left backward-delete-char-untabify
                 "Same as `backward-delete-char-untabify' but performs correcting or deleting the overlay if it's needed"
                 (backward-delete-char-untabify 1))

(defun wpers-move-end-of-line ()
  "Function `move-end-of-line' is called
and then removes overlay and all trailing spaces"
  (interactive)                                    
  (move-end-of-line nil)
  (when wpers-overlay (wpers-kill-ovr)))

(defun wpers-move-beginning-of-line ()
  "Function `move-beginning-of-line' is called
and then removes overlay if it's present and line is empty"
  (interactive)
  (move-beginning-of-line nil)
  (when (and wpers-overlay (zerop (current-column))) (wpers-kill-ovr)))

(defun wpers--pre-command-hook () "Disabling functionality when visual-line-mode is non-nil or marking is active"
  (if (or this-command-keys-shift-translated mark-active visual-line-mode)
      (let ((fn-pair (rassoc this-command wpers-funs-alist)))
        (when fn-pair (setq this-command (car fn-pair))))))

(defun wpers--post-command-hook () "Killing wpers-overlay when it is not at the point"
  (when (and wpers-overlay
             (or (not (wpers-ovr-at-point-p))
                 (wpers-txt-after-ovr-p)))
    (wpers-kill-ovr)))

(provide 'wpers)
;;; buffer-move.el ends here
