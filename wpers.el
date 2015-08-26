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

(defconst wpers-overloaded-funs [next-line previous-line right-char move-end-of-line] "Functions overloaded by the mode")

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
        (mapc '(lambda (x) (add-hook (car x) (cdr x) nil t)) wreps-hooks-alist))
      (progn
        (message "Wpers disabled")
        (wpers-kill-final-spaces)
        (mapc '(lambda (x) (remove-hook (car x) (cdr x) t)) wreps-hooks-alist))))

(defmacro wpers-save-vpos (form) "Eval form with saving current cursor's position in the line (column)"
  `(let ((old-col (current-column)) last-col) ,form (move-to-column old-col t)))

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
        (insert 32)
        (right-char))))

(defun wpers-move-end-of-line () "Function `move-end-of-line' is called and then removes all trailing spaces"
  (interactive)
  (move-end-of-line nil)
  (while (eq (char-before) 32) (delete-char -1)))

(defun wpers-kill-final-spaces () "Deleting all trailing spaces for all lines in the buffer"
  (save-excursion
   (goto-char (point-min))
   (while (search-forward-regexp " +$" nil t) (replace-match ""))))

(defun wpers--pre-command-hook () "Disabling functionality when buffer is read only, visual-line-mode is non-nil or marking is active"
  (if (or buffer-read-only this-command-keys-shift-translated mark-active visual-line-mode)
      (let ((fn-pair (rassoc this-command wpers-funs-alist)))
        (when fn-pair (setq this-command (car fn-pair))))))

(provide 'wpers)
;;; buffer-move.el ends here

