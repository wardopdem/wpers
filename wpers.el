;;; wpersmode.el --- minor mode for stopping cursor jumping in emacs

;; Copyright (C) 2015 <wardopdem@gmail.com>

;; Authors:         wardopdem@gmail.com
;; Keywords:        persistent, cursor
;; Package-Version: 20150825.001
;; Version:         1.0.0
;; URL:             <https://github.com/wardopdem/wpers>

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

;; This would require adding extra spaces at the end of lines.
;; When you disable mode or buffer saving all the extra spaces
;; will be removed.

;;; Installation & using:

;; Just add wpers.el folder location to load-path and call require (f.e. in your init)

;;         (add-to-list 'load-path <path-to-wpers>)
;;         (require 'wpers)

;; Then call command `wpers-mode` for toggle mode

;;         M-x wpers-mode

;;; Code:

(defconst wpers-overloaded-funs [next-line previous-line left-char right-char move-end-of-line move-beginning-of-line ] "Functions overloaded by the mode")

(defconst wpers-fun-prefix "wpers-" "Prefix for new functions")

(defconst wpers-funs-alist
  (mapcar '(lambda (x) (cons x (intern (concat wpers-fun-prefix (symbol-name x)))))
          wpers-overloaded-funs)
  "alist (old . new) functions")

(defconst wpers-mode-map
  (reduce '(lambda (s x) (define-key s (vector 'remap (car x)) (cdr x)) s)
          wpers-funs-alist :initial-value (make-sparse-keymap))
  "Mode map for `wpers'")

(defconst wreps-hooks-alist
  '((pre-command-hook . wpers--pre-command-hook)
    (post-command-hook . wpers--post-command-hook)
    (auto-save-hook   . wpers-kill-final-spaces)
    (before-save-hook . wpers-kill-final-spaces))
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
        (wpers-kill-final-spaces)
        (wpers-kill-ovr)
        (mapc '(lambda (x) (remove-hook (car x) (cdr x) t)) wreps-hooks-alist))))


(defun wpers-make-ovr (&optional prop val)
  (wpers-kill-ovr)
  (setq wpers-overlay (make-overlay (point) (point)))
  (overlay-put wpers-overlay 'wpers t)
;  (overlay-put wpers-overlay 'evaporate t)
  (if (and prop val) (overlay-put wpers-overlay prop val)))

(defun wpers-kill-ovr ()
  (when wpers-overlay
    (let* ((ov-pos (overlay-start wpers-overlay))
          (ch (char-after ov-pos)))
      (when (and ch (not (eq ch 10)))
        (save-excursion (goto-char ov-pos) (insert (make-string (length (wpers-ovr-get)) 32)))))
    (delete-overlay wpers-overlay)
    (setq wpers-overlay nil)))

(defmacro wpers-ovr-get (&optional prop)
  (let* ((prop (or prop 'before-string)))
    `(overlay-get wpers-overlay ',prop)))

(defmacro wpers-ovr-put (prop val)
  `(let ((_ (wpers-ovr-get ,prop)))
    (overlay-put wpers-overlay ',prop ,val)))

;(macroexpand '(wpers-ovr-put before-string (concat _ " ")))

(defun wpers-current-column ()
  (let ((res (current-column)))
    (if (and wpers-overlay (eq (overlay-start wpers-overlay) (point)))
        (+ res (length (wpers-ovr-get)))
        res)))

(defun wpers-move-to-column (col)
  (move-to-column col)
  (let* ((last-column (- (line-end-position) (line-beginning-position)))
         (spcs-needed (- col last-column)))
    (when (plusp spcs-needed) (wpers-make-ovr 'before-string (make-string spcs-needed 32)))))

(defmacro wpers-save-vpos (form) "Eval form with saving current cursor's position in the line (column)"
  (let ((old-col (make-symbol "old-col")))
    `(let ((,old-col (wpers-current-column))) ,form (wpers-move-to-column ,old-col))))

(defun wpers-next-line () "Same as `new-line' but adds the spaces if it's needed
for saving cursor's position in the line (column)"
  (interactive) (wpers-save-vpos (next-line)))

(defun wpers-previous-line () "Same as `previous-line' but adds the spaces if it's needed
for saving cursor's position in the line (column)"
  (interactive) (wpers-save-vpos (previous-line)))

(defun wpers-right-char () "Same as `right-char' but adds the spaces if cursor at end of line (column)"
  (interactive)
  (let ((ca (char-after)))
    (if (or (null ca) (eq ca 10))
        (if (null wpers-overlay)
            (wpers-make-ovr 'before-string " ")
            (if (eq (overlay-start wpers-overlay) (point))
                (wpers-ovr-put before-string (concat _ " "))
                (wpers-kill-ovr) (wpers-make-ovr 'before-string " ")))
        (right-char))))

        ;; (if (and wpers-overlay (eq (point) (overlay-start wpers-overlay)))
        ;;     (overlay-put wpers-overlay 'before-string (concat (overlay-get wpers-overlay 'before-string) " "))
        ;;     (right-char)))

  ;; (let ((ca (char-after)))
  ;;   (if (or (null ca) (eq ca 10))
  ;;       (insert 32)
  ;;       (right-char))))

(defun wpers-left-char ()
  (interactive)
  (if wpers-overlay
      (if (eq (point) (overlay-start wpers-overlay))
          (if (plusp (length (wpers-ovr-get)))
              (wpers-ovr-put before-string (substring _ 1))
              (wpers-kill-ovr) (left-char))
          (wpers-kill-ovr))
      (left-char)))

;; (if (and wpers-overlay (eq (point) (overlay-start wpers-overlay)))
;;       (overlay-put wpers-overlay 'before-string (substring (overlay-get wpers-overlay 'before-string) 1))
;;       (left-char)))

(defun wpers-move-end-of-line () "Function `move-end-of-line' is called and then removes all trailing spaces"
  (interactive)
  (move-end-of-line nil)
  (when wpers-overlay (wpers-kill-ovr))
  (while (eq (char-before) 32) (delete-char -1)))

(defun wpers-move-beginning-of-line ()
  (interactive)
  (move-beginning-of-line nil)
  (when (and wpers-overlay (zerop (current-column))) (wpers-kill-ovr)))

(defun wpers-kill-final-spaces () "Deleting all trailing spaces for all lines in the buffer"
  (save-excursion
   (goto-char (point-min))
   (while (search-forward-regexp " +$" nil t) (replace-match ""))))

(defun wpers--pre-command-hook () "Disabling functionality when buffer is read only, visual-line-mode is non-nil or marking is active"
  (if (or buffer-read-only this-command-keys-shift-translated mark-active visual-line-mode)
      (let ((fn-pair (rassoc this-command wpers-funs-alist)))
        (when fn-pair (setq this-command (car fn-pair))))))

(defun wpers--post-command-hook ()
  (when wpers-overlay
    (let* ((ovr-pos (overlay-start wpers-overlay))
           (ovr-in-cur-line (eq (line-number-at-pos) (line-number-at-pos ovr-pos)))
           (ovr-at-end (eq (char-after ovr-pos) 10)))
      (when (or (and ovr-in-cur-line (not ovr-at-end))
                (and ovr-at-end (not ovr-in-cur-line)))
        (wpers-kill-ovr)
        (setq wpers-overlay nil)))))

(provide 'wpers)
;;; buffer-move.el ends here
;(remove-overlays)
;(length (remove-if-not '(lambda (x)(overlay-get x 'wpers)) (overlays-in (point-min) (point-max))))
