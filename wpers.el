;;; wpers.el --- minor mode for stopping cursor jumping in emacs

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

;; Call command `wpers-overlay-visible` (or customise `wpers-pspace`) for toggle visibility of overlay

;;         M-x wpers-overlay-visible

;; Customize the `wpers-ovr-killing-funs` to define which functions
;; reset the vertical position of the cursor (column).

;;; Code:

;;; Import

(require 'cl)
(require 'hl-line)

;;; Mode constants, variables 

(defconst wpers--prefix "wpers-" "Package prefix")

(defconst wpers--pspace-def 183 "Default char for overlay displaying")

(defvar-local wpers--overlay nil)

(defun wpers--intern (x &optional public) "Make symbol with package prefix (wpers)"
  (intern (concat wpers--prefix (when (not public) "-") (symbol-name x))))

(defgroup wpers nil
  "Wpers mode"
  :group 'editing
  :prefix "wpers-")

(defun wpers--get-pspace (sym)
  (let ((val (symbol-value sym)))
    (cond 
      ((eq val 32) nil)
      ((eq val wpers--pspace-def) t)
      (t val))))

(defun wpers--set-pspace (var val)
  (setq wpers-pspace 
        (cond ((null val)  32)
              ((eq val t) wpers--pspace-def)
              (t val)))
  (mapc #'(lambda (b)
            (with-current-buffer b
              (when (and (local-variable-p 'wpers-mode) wpers-mode wpers--overlay)
                (wpers--ovr-put (make-string (length (wpers--ovr-get)) wpers-pspace)))))
        (buffer-list)))

(defvar wpers--funs-alist nil "alist of remaped functions")

(defconst wpers--mode-map
  (reduce #'(lambda (s x) (define-key s (vector 'remap (car x)) (cdr x)) s)
          wpers--funs-alist :initial-value (make-sparse-keymap))
  "Mode map for `wpers'")

(defconst wreps--hooks-alist
  '((pre-command-hook  . wpers--pre-command-hook)
    (post-command-hook . wpers--post-command-hook))
  "alist (hook-var . hook-function)")

;;; Mode public interface

(define-minor-mode wpers-mode
  "Toggle persistent cursor mode."
  :init-value nil
  :lighter " wpers"
  :group 'wpers
  :keymap wpers--mode-map
  (if wpers-mode
      (progn
        (message "Wpers enabled")
        (wpers--set-remaps)
        (setq-local wpers--overlay nil)
        (mapc #'(lambda (x) (add-hook (car x) (cdr x) nil t)) wreps--hooks-alist))
      (progn
        (message "Wpers disabled")         
        (wpers--ovr-kill)
        (mapc #'(lambda (x) (remove-hook (car x) (cdr x) t)) wreps--hooks-alist))))

(defcustom wpers-pspace 32
  "Pseudo-space - char for displaying in the overlay instead of real spaces"
  :type `(choice (const :tag "Standard visible" t)
                 (const :tag "Invisible" nil)
                 (character :tag "Custom visible"))
  :get 'wpers--get-pspace
  :set 'wpers--set-pspace
  :set-after '(wpers--pspace-def))

(defun wpers-overlay-visible (val) "Toggle overlay visibility if VAL is nil, swtich on if t else set to VAL"
  (interactive "P")
  (wpers--set-pspace nil
    (cond
      ((null val) t)
      ((member val  '(- (4))) nil)
      (t val))))

(defcustom wpers-ovr-killing-funs '(undo move-end-of-line move-beginning-of-line) 
  "Functions killing overlay"
  :group 'wpers
  :type '(repeat function))

;;; Utils

(defmacro wpers--at-end (&optional expr)
  (let ((expr (or expr '(overlay-start wpers--overlay)))
        (ch (make-symbol "ch")))
    `(let ((,ch (char-after ,expr)))
       (or (null ,ch) (eq ,ch 10)))))

;;; Overlay managing functions

(defun wpers--ovr-propz-txt (txt) "Propertize TXT for overlay displaying"
  (if (or hl-line-mode global-hl-line-mode)
      (propertize txt 'face (list :background (face-attribute 'highlight :background)))
      txt))

(defun wpers--ovr-make (&optional str) "Creating overlay with optional setting before-string to STR"
  (wpers--ovr-kill)
  (setq wpers--overlay (make-overlay (point) (point)))
  (overlay-put wpers--overlay 'wpers t)
  (if str (overlay-put wpers--overlay 'before-string (wpers--ovr-propz-txt str))))

(defun wpers--ovr-at-point-p () "Return t if wpers--overlay was placed at the point"
  (eq (point) (overlay-start wpers--overlay)))

(defun wpers--ovr-txt-after-p () "Return t if exists something after overlay"
  (and wpers--overlay (not (wpers--at-end)))) 

(defun wpers--ovr-to-spcs () "Replacing overlay (wpers--overlay) with space chars (32)"
  (let ((ovr-size (when (wpers--ovr-at-point-p) (length (wpers--ovr-get)))))
    (save-excursion
     (goto-char (overlay-start wpers--overlay))       
     (insert (make-string (length (wpers--ovr-get)) 32)))
    (when ovr-size (right-char ovr-size))))      
  
(defun wpers--ovr-kill () "Killing of the wpers--overlay with the replacement of spacess, if necessary"
  (when wpers--overlay
    (when (not (wpers--at-end)) (wpers--ovr-to-spcs))
    (delete-overlay wpers--overlay)
    (setq wpers--overlay nil)))           

(defun wpers--ovr-get () "Get wpers--overlay before-string property"
   (overlay-get wpers--overlay 'before-string))

(defmacro wpers--ovr-put (val) 
  "Set wpers--overlay property before-string to VAL within context where variable _ is set to (wpers--ovr-get 'before-string)"
  `(let ((_ (wpers--ovr-get)))
    (overlay-put wpers--overlay 'before-string (wpers--ovr-propz-txt ,val))))

;;; Column managing functions

(defun wpers--current-column () "Same as `current-column' but it takes account size of overlay if it present"
  (let ((res (current-column)))
    (if (and wpers--overlay (wpers--ovr-at-point-p))
        (+ res (length (wpers--ovr-get)))
        res)))

(defun wpers--move-to-column (col) "Same as `move-to-column' but adds overlay for correction cursor position in the line (column)"
  (move-to-column col)
  (let* ((last-column (- (line-end-position) (line-beginning-position)))
         (spcs-needed (- col last-column)))
    (when (plusp spcs-needed)
      (wpers--ovr-make (make-string spcs-needed wpers-pspace)))))

(defmacro wpers--save-vpos (form) "Eval form with saving current cursor's position in the line (column)"
  (let ((old-col (make-symbol "old-col")))
    `(let ((,old-col (wpers--current-column))) ,form (wpers--move-to-column ,old-col))))

;;; Mode remap handlers

(defun wpers--remap (key body &optional params)
  (let ((old (when (and (vectorp key)
                        (eq (elt key 0) 'remap)) (elt key 1)))
        (fun (if (functionp (car body))
                 (car body)
                 `(lambda ,params "WPERS handler: perform operation with saving current cursor's position in the line (column)" ,@body))))
    (when old (add-to-list 'wpers--funs-alist (cons old fun)))
    (define-key wpers--mode-map key fun)))

(defun wpers--remap-vert (command &optional key)
  (wpers--remap (or key (vector 'remap command)) 
                `((interactive) (wpers--save-vpos (call-interactively ',command)))))
 
(defun wpers--remap-right (command &optional key)
  (let ((key (or key (vector 'remap command)))
        (expr `(call-interactively ',command)))
    (wpers--remap key
       `((interactive)
         (if (wpers--at-end (point))
             (if (null wpers--overlay)
                 (wpers--ovr-make (string wpers-pspace))
                 (if (wpers--ovr-at-point-p)
                     (wpers--ovr-put (concat _ (string wpers-pspace)))
                     (wpers--ovr-kill) (wpers--ovr-make (string wpers-pspace))))
             (wpers--ovr-kill) ,expr)))))

(defun wpers--remap-left (command &optional key)
  (let ((key (or key (vector 'remap command)))
        (expr `(call-interactively ',command)))
    (wpers--remap key
       `((interactive)
         (if wpers--overlay
             (if (and (wpers--ovr-at-point-p) (wpers--at-end (point)))
                 (if (plusp (length (wpers--ovr-get)))
                     (wpers--ovr-put (substring _ 1))
                     (wpers--ovr-kill) ,expr)
                 (wpers--ovr-kill) ,expr)
             ,expr)))))

(defun wpers--set-remaps (&optional var val)
  (mapc #'(lambda (x)
            (let ((remaper (car x))
                  (funs (cdr x)))
              (mapc #'(lambda (f) (funcall remaper f)) funs)))
        (or val wpers-remaps)))

(wpers--remap [remap mouse-set-point] `(
  (interactive "e")
  (mouse-set-point event)
  (let ((col (car (posn-col-row (cadr event)))))
    (wpers--move-to-column col))) '(event))

(defcustom wpers-remaps
  '((wpers--remap-vert  next-line previous-line scroll-up scroll-down)
    (wpers--remap-left  left-char backward-char backward-delete-char backward-delete-char-untabify)
    (wpers--remap-right right-char forward-char))
  ""
  :options '(wpers--remap-vert wpers--remap-left wpers--remap-right)
  :type '(alist :key-type symbol :value-type (repeat function)))

;;; Hooks

(defun wpers--pre-command-hook () "Disabling functionality when visual-line-mode is non-nil or marking is active"
  (if (member this-command wpers-ovr-killing-funs)
      (wpers--ovr-kill)
      (if (or this-command-keys-shift-translated mark-active visual-line-mode (null truncate-lines))
          (let ((fn-pair (rassoc this-command wpers--funs-alist)))
            (when fn-pair (setq this-command (car fn-pair)))))))

(defun wpers--post-command-hook () "Killing wpers--overlay when it is not at the point"
  (when (and wpers--overlay
             (or (not (wpers--ovr-at-point-p))
                 (wpers--ovr-txt-after-p)))
    (wpers--ovr-kill)))

(provide 'wpers)
;;; wpers.el ends here
