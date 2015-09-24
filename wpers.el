;;; wpers.el --- minor mode for stopping cursor jumping in emacs

;; Copyright (C) 2015 <wardopdem@gmail.com>

;; Authors:         wardopdem@gmail.com
;; Version:         2.2
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

;; Just add wpers.el folder location to load-path and call `require`

;;         (add-to-list 'load-path <path-to-wpers>)
;;         (require 'wpers)

;; Then call command `wpers-mode` for toggle mode

;;         M-x wpers-mode

;;; Сustomization:

;; Call command `wpers-overlay-visible` or customise `wpers-pspace`
;; for toggle visibility of overlay.

;; Customize `wpers-ovr-killing-funs` to define which functions
;; reset the vertical position of the cursor (column).

;; Set the `wpers--remaps` to define the list of functions saving
;; the vertical position of the cursor (column).

;;; Code:

;;;;;;;;;;
;;; Import

(require 'cl)
(require 'hl-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode constants, variables 

(defconst wpers--prefix "wpers-" "Package prefix")

(defconst wpers--pspace-def ?\xB7 "Default char for overlay displaying.")

(defvar-local wpers--overlay nil)

(defgroup wpers nil
  "Persistent cursor"
  :group 'editing
  :prefix "wpers-")

(defvar wpers--funs-alist nil "alist of remaped functions.")

(defconst wpers--mode-map (make-sparse-keymap) "Wpers-mode keymap.")

(defconst wreps--hooks-alist
  '((pre-command-hook  . wpers--pre-command)
    (post-command-hook . wpers--post-command))
  "alist (hook-var . hook-function) for wpers-mode.")

;;;;;;;;;
;;; Utils

(defmacro wpers--at-end (&optional expr)
  "Return non-nil if end of line or buffer be found at EXPR location."
  (let ((expr (or expr '(overlay-start wpers--overlay)))
        (ch (make-symbol "ch")))
    `(let ((,ch (char-after ,expr)))
       (or (null ,ch) (eq ,ch ?\n)))))

(defun wpers--remap-p (key) "Return non-nil if KEY is remapping."
  (and (vectorp key)
       (eq (elt key 0) 'remap)))

(defun wpers--key-handler (key) "Return the handler for a KEY."
  (if (wpers--remap-p key) (elt key 1) (key-binding key)))

(defun wpers--mk-key (command &optional key)
  "Return KEY or remapping for a COMMAND."
  (or key (vector 'remap command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay managing functions

(defun wpers--ovr-propz-txt (txt) "Propertize TXT for overlay displaying."
  (if (or (and (default-boundp 'global-hl-line-mode) 
                global-hl-line-mode)
          (and (default-boundp 'hl-line-mode) 
               hl-line-mode 
               (buffer-local-value 'hl-line-overlay)))
      (propertize txt 'face (list :background (face-attribute 'hl-line :background nil t)))
      txt))

(defun wpers--ovr-str (len)
  (wpers--ovr-propz-txt (make-string len wpers-pspace)))

(defun wpers--ovr-make (&optional len) "Creating overlay with optional setting before-string to STR."
  (wpers--ovr-kill)
  (setq wpers--overlay (make-overlay (point) (point)))
  (overlay-put wpers--overlay 'wpers t)
  (overlay-put wpers--overlay 'window (selected-window))
  (when len (wpers--ovr-put len)))

(defun wpers--ovr-at-point-p () "Return t if wpers--overlay was placed at the point."
  (eq (point) (overlay-start wpers--overlay)))

(defun wpers--ovr-txt-after-p () "Return t if exists something after overlay."
  (and wpers--overlay (not (wpers--at-end)))) 

(defun wpers--ovr-to-spcs () "Replacing overlay (wpers--overlay) with space chars."
  (let ((ovr-size (when (wpers--ovr-at-point-p) (wpers--ovr-len))))
    (save-excursion
     (goto-char (overlay-start wpers--overlay))       
     (insert (make-string (wpers--ovr-len) ?\s)))
    (when ovr-size (right-char ovr-size))))      
  
(defun wpers--ovr-kill (&optional buffer)
  "Killing of the wpers--overlay with the replacement of spacess, if necessary."
  (with-current-buffer (or buffer (current-buffer))
    (when wpers--overlay
      (when (not (wpers--at-end)) (wpers--ovr-to-spcs))
      (delete-overlay wpers--overlay)
      (setq wpers--overlay nil))))

(defun wpers--clean-up-ovrs ()
  "Destroying of overlays in the inactive buffers."
  (mapc #'(lambda (b)
              (when (and (local-variable-p 'wpers-mode b)
                         (buffer-local-value 'wpers-mode b)
                         ;(buffer-local-value 'wpers--overlay b)
                         (not (eq b (current-buffer))))
                (wpers--ovr-kill b)))
        (buffer-list)))

(defun wpers--ovr-len () "Get wpers--overlay before-string property."
   (length (overlay-get wpers--overlay 'before-string)))

(defun wpers--ovr-put (len) 
  "Set wpers--overlay property before-string to (make-string LEN wpers-pspace)."
  (overlay-put wpers--overlay 'before-string (wpers--ovr-propz-txt (wpers--ovr-str len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Column managing functions

(defun wpers--current-column ()
  "Same as `current-column' but it takes account size of overlay if it present."
  (let ((res (current-column)))
    (if (and wpers--overlay (wpers--ovr-at-point-p))
        (+ res (wpers--ovr-len))
        res)))

(defun wpers--move-to-column (col)
  "Same as `move-to-column' but adds overlay for correction cursor position in the line (column)."
  (move-to-column col)
  (let* ((last-column (- (line-end-position) (line-beginning-position)))
         (spcs-needed (- col last-column)))
    (when (plusp spcs-needed) (wpers--ovr-make spcs-needed))))

(defmacro wpers--save-column (form) "Eval form with saving current cursor's position in the line (column)."
  (let ((old-col (make-symbol "old-col")))
    `(let ((,old-col (wpers--current-column))) ,form (wpers--move-to-column ,old-col))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode remap handlers

(defun wpers--remap (key body &optional params)
  "The basic function of creating wrappers for cursor positioning commands."
  (let ((old (wpers--key-handler key))
        (fun `(lambda ,params 
                "WPERS handler: perform operation with saving current cursor's position in the line (column)."
                ,@body)))
    (when old (add-to-list 'wpers--funs-alist (cons old fun)))
    (define-key wpers--mode-map key fun)))

(defun wpers--remap-vert (command &optional key)
  "Define to KEY or remap vertical motion COMMAND with saving the position of the cursor in the row (column)."
  (wpers--remap (wpers--mk-key command key) 
                `((interactive)(wpers--save-column (call-interactively ',command)))))

(defun wpers--remap-left (command &optional key)
  "Define to KEY or remap right motion COMMAND with shrinking or deleting of overlay if needed."
  (let ((key (wpers--mk-key command key))
        (expr `(call-interactively ',command)))
    (wpers--remap key
       `((interactive)
         (if wpers--overlay
             (if (and (wpers--ovr-at-point-p) (wpers--at-end (point)))
                 (if (plusp (wpers--ovr-len))
                     (wpers--ovr-put (1- (wpers--ovr-len)))
                     (wpers--ovr-kill) ,expr)
                 (wpers--ovr-kill) ,expr)
             ,expr)))))
 
(defun wpers--remap-right (command &optional key)
  "Define to KEY or remap left motion COMMAND with making or enlarging of overlay if needed."
  (let ((key (wpers--mk-key command key))
        (expr `(call-interactively ',command)))
    (wpers--remap key
       `((interactive)
         (if (wpers--at-end (point))
             (if (null wpers--overlay)
                 (wpers--ovr-make 1)
                 (if (wpers--ovr-at-point-p)
                     (wpers--ovr-put (1+ (wpers--ovr-len)))
                     (wpers--ovr-kill) (wpers--ovr-make 1)))
             (wpers--ovr-kill) ,expr)))))

(defun wpers--remap-mouse (command)
  "Remap mouse-related COMMAND with positioning cursor at mouse pointer."
  (wpers--remap (vector 'remap command) `(
    (interactive)
    (call-interactively ',command)
    (let ((col (car (posn-col-row (cadr last-input-event)))))
      (wpers--move-to-column col)))))

;;;;;;;;;
;;; Hooks

(defun wpers--pre-command () "Disabling functionality when visual-line-mode is non-nil,
marking is active, truncate-lines is nil or command in `wpers-ovr-killing-funs'."
  (if (member this-command wpers-ovr-killing-funs)
      (wpers--ovr-kill)
      (if (or this-command-keys-shift-translated mark-active visual-line-mode (null truncate-lines))
          (let ((fn-pair (rassoc this-command wpers--funs-alist)))
            (when fn-pair (setq this-command (car fn-pair)))))))

(defun wpers--post-command () "Killing wpers--overlay when it is not at the point or text happens after it."
  (when wpers--overlay
    (overlay-put wpers--overlay 'window (selected-window))
    (when  (or (not (wpers--ovr-at-point-p))
               (wpers--ovr-txt-after-p))
      (wpers--ovr-kill))))

;;;;;;;;;;;;;;;;;;;;
;;; Custom accessors

(defun wpers--get-pspace (sym)
  "Getter for custom `wpers-pspace'"
  (let ((val (symbol-value sym)))
    (cond 
      ((eq val ?\s) nil)
      ((eq val wpers--pspace-def) t)
      (t val))))

(defun wpers--set-pspace (var val)
  "Setter for custom `wpers-pspace'"
  (setq wpers-pspace 
        (cond ((null val)  ?\s)
              ((eq val t) wpers--pspace-def)
              (t val)))
  (mapc #'(lambda (b)
            (with-current-buffer b
              (when (and (local-variable-p 'wpers-mode) wpers-mode wpers--overlay)
                (wpers--ovr-put (wpers--ovr-len)))))
        (buffer-list)))

(defun wpers--set-remaps (var val)
  "Setter for custom `wpers-remaps'"
  (set-default var val)
  (mapc #'(lambda (x) (define-key wpers--mode-map (vector 'remap (car x)) nil))
        wpers--funs-alist)
  (mapc #'(lambda (x)
            (let ((remaper (car x))
                  (funs (cdr x)))
              (mapc #'(lambda (f)
                        (if (listp f)
                            (funcall remaper (car f) (kbd (cadr f)))
                            (funcall remaper f))) funs)))
        (or val wpers-remaps)))

;;;;;;;;;;;;;;;;;;;;;;;;;
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
        (setq-local wpers--overlay nil)
        (mapc #'(lambda (x) (add-hook (car x) (cdr x) nil t)) wreps--hooks-alist))
      (progn
        (message "Wpers disabled")         
        (wpers--ovr-kill)
        (mapc #'(lambda (x) (remove-hook (car x) (cdr x) t)) wreps--hooks-alist))))

(defcustom wpers-pspace ?\s
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

(defcustom wpers-remaps
  '((wpers--remap-vert  next-line previous-line scroll-up-command scroll-down-command
                        (scroll-down-command "<prior>") (scroll-up-command "<next>")) ; for CUA mode
    (wpers--remap-left  left-char backward-char backward-delete-char backward-delete-char-untabify)
    (wpers--remap-right right-char forward-char)
    (wpers--remap-mouse mouse-set-point))
  "alist which is linking general remap-functions with commands.
Each element looks like (HANDLER . LIST-OF-COMMANDS) where
  HANDLER          - one of wpers--remap-xxx functions, 
  LIST-OF-COMMANDS - list of commands (like `next-line') or looks like (COMMAND KEY)
                       where KEY is a string intended for `kbd' processing"
  :options '(wpers--remap-vert wpers--remap-left wpers--remap-right wpers--remap-mouse)
  :type '(alist :key-type symbol :value-type (repeat (choice function (list symbol string))))
  :set 'wpers--set-remaps)

;; Destroying ot the overlays in all inactive buffers
(add-hook 'post-command-hook 'wpers--clean-up-ovrs)

(provide 'wpers)
;;; wpers.el ends here
