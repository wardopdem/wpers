;; wpers.el --- minor mode for stopping cursor jumping in emacs

;; Copyright (C) 2015 <wardopdem@gmail.com>

;; Authors:         wardopdem@gmail.com
;; Version:         2.3
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

(defconst wpers--prefix "wpers-"
  "Package prefix")

(defconst wpers--pspace-def ?\xB7 "Default char for overlay displaying.")

(defconst wpers--ovr-txt-prop 'before-string)

(defvar-local wpers--overlay nil
  "Overlay to shift the cursor to the right.")

(defvar-local wpers--shadow-overlays nil
  "alist of inactive the overlays with they windows 
each element of which has the form (window . overlay).")

(defgroup wpers nil
  "Persistent cursor"
  :group 'editing
  :prefix "wpers-")

(defconst wreps--hooks-alist
  '((pre-command-hook  . wpers--pre-command)
    (post-command-hook . wpers--post-command))
  "alist (hook-var . hook-function) for wpers-mode.")

;;;;;;;;;
;;; Utils

(defun wpers--NOP-command ()
  "NOP interactive function"
  (interactive))

(defun wpers--at-end (&optional ovr)
  "Return non-nil if end of line or buffer be found at overlay 
(ovr is non-nil) or point (default) location."
  (let ((ch (char-after (if ovr (overlay-start wpers--overlay) (point)))))
       (or (null ch) (eq ch ?\n))))

(defun wpers--hll-p (&optional w)
  "Return t if highlighting by global-hl-line-mode/hl-line-mode 
is active in the window W (selected window by default)"
  (let ((w (or w (selected-window))))
    (or (and (default-boundp 'global-hl-line-mode) 
             global-hl-line-mode
             (or global-hl-line-sticky-flag (eq w (selected-window))))
        (and (local-variable-if-set-p 'hl-line-mode) 
             hl-line-mode 
             (or hl-line-sticky-flag (eq w (selected-window)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay managing functions

(defun wpers--ovr-propz-txt (txt &optional w)
  "Propertize TXT for overlay displaying in window W (selected window by default)."
  (if (wpers--hll-p w)
      (propertize txt 'face (list :background (face-attribute 'hl-line :background nil t)))
      txt))

(defun wpers--ovr-str (len &optional w)
  "Make string for overlay of length LEN"
  (wpers--ovr-propz-txt (make-string len wpers-pspace) w))

(defun wpers--ovr-make (&optional len &optional w)
  "Creating overlay with optional setting before-string to STR."
  (wpers--ovr-kill)
  (setq wpers--overlay (make-overlay (point) (point)))
  (overlay-put wpers--overlay 'wpers t)
  (overlay-put wpers--overlay 'window (selected-window))
  (when len (wpers--ovr-put len w)))

(defun wpers--ovr-at-point-p ()
  "Return t if wpers--overlay was placed at the point."
  (eq (point) (overlay-start wpers--overlay)))

(defun wpers--ovr-txt-after-p ()
  "Return t if exists something after overlay."
  (and wpers--overlay (not (wpers--at-end t)))) 

(defun wpers--ovr-to-spcs ()
  "Replacing overlay (wpers--overlay) with space chars."
  (let ((ovr-size (when (wpers--ovr-at-point-p) (wpers--ovr-len))))
    (save-excursion
     (goto-char (overlay-start wpers--overlay))
     (insert (make-string (wpers--ovr-len) ?\s)))
    (when ovr-size (right-char ovr-size))))
  
(defun wpers--ovr-kill (&optional buffer)
  "Killing of the wpers--overlay with the replacement of spacess, if necessary."
  (with-current-buffer (or buffer (current-buffer))
    (when wpers--overlay
      (when (not (wpers--at-end t)) (wpers--ovr-to-spcs))
      (delete-overlay wpers--overlay)
      (setq wpers--overlay nil))))

(defun wpers--ovr-len ()
  "Get wpers--overlay before-string property."
   (length (overlay-get wpers--overlay wpers--ovr-txt-prop)))

(defun wpers--ovr-put (&optional len w) 
  "Set wpers--overlay property before-string to (make-string LEN wpers-pspace)."
  (let ((len (or len (wpers--ovr-len))))
    (if (zerop len)
        (wpers--ovr-kill)
        (overlay-put wpers--overlay wpers--ovr-txt-prop (wpers--ovr-str len w)))))

(defun wpers--ovr-update ()
  (dolist (w (window-list))
    (with-current-buffer (window-buffer w)
      (when wpers-mode
        (dolist (ovr (remove-if-not #'(lambda (x) (overlay-get x 'wpers))
                                (overlays-in (point-min) (point-max))))
          (overlay-put ovr wpers--ovr-txt-prop
                       (wpers--ovr-str (length (overlay-get ovr wpers--ovr-txt-prop))
                                       (overlay-get ovr 'window))))))))

;;; Shadow overlays

(defun wpers--ovr-to-shadow (w)
  "Make overlay in the window W inactive (but visible)."
  (push (cons w wpers--overlay) wpers--shadow-overlays))
  
(defun wpers--ovr-from-shadow (w)
  "Restore active overlay in the window W from previously saved state."
  (let ((sh-ovr (find-if #'(lambda (x) (eq (car x) w)) wpers--shadow-overlays)))
    (if sh-ovr
        (setq wpers--overlay (cdr sh-ovr)
              wpers--shadow-overlays (remove sh-ovr wpers--shadow-overlays))
        (setq wpers--overlay nil))))

(defun wpers--ovr-kill-dangling ()
  "Killing of all \"dangling\" (owned by nothing) overlays"
  (let ((wl (window-list)) dl-ovs)
    (dolist (ovr wpers--shadow-overlays)
      (let* ((w (car ovr))
             (o (cdr ovr))
             (ob (and o (overlay-buffer o)))
             (ow (and o (overlay-get o 'window))))
        (unless (and w o ob ow (member w wl) (member ow wl) (buffer-local-value 'wpers-mode ob))
          (delete-overlay o)
          (push ovr dl-ovs))))
    (setq wpers--shadow-overlays (remove-if #'(lambda (x) (member x dl-ovs)) wpers--shadow-overlays))))

(defun wpers--adapt-ovrs ()
  "Adapt overlays for current windows/buffers disposition."
  (when wpers-mode
    (let ((sw (selected-window)))
      (if wpers--overlay
          (let ((ow (overlay-get wpers--overlay 'window)))
            (unless (eq sw ow)
              (wpers--ovr-to-shadow ow)
              (wpers--ovr-from-shadow sw)))
          (wpers--ovr-from-shadow sw))))
  (wpers--ovr-kill-dangling)
  (wpers--ovr-update))

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

;;;;;;;;;;;;;;;;;;;;
;;; Command handlers

(defun wpers--vert-handler (command)
  "The handler for commands that move the cursor vertically."
  (let ((old-col (wpers--current-column)))
    (call-interactively command)
    (wpers--move-to-column old-col)))

(defun wpers--left-handler (command)
  "The handler for commands that move the cursor to the left."
  (if wpers--overlay
      (if (and (wpers--ovr-at-point-p) (wpers--at-end))
          (if (plusp (wpers--ovr-len))
              (wpers--ovr-put (1- (wpers--ovr-len)))
              (wpers--ovr-kill) (call-interactively command))
          (wpers--ovr-kill) (call-interactively command))
      (call-interactively command)))

(defun wpers--right-handler (command)
  "The handler for commands that move the cursor to the right."
  (if (wpers--at-end)
      (if (null wpers--overlay)
          (wpers--ovr-make 1)
          (if (wpers--ovr-at-point-p)
              (wpers--ovr-put (1+ (wpers--ovr-len)))
              (wpers--ovr-kill) (wpers--ovr-make 1)))
      (wpers--ovr-kill) (call-interactively command)))

(defun wpers--mouse-handler (command)
  "The handler for commands that move mouse cursor."
  (call-interactively command)
  (wpers--move-to-column (car (posn-col-row (cadr last-input-event)))))

;;;;;;;;;
;;; Hooks

(defun wpers--command-handler (&optional command)
  "Returns handler for a command or nil if handler absent"
  (let* ((command (or command this-command))
         (rmp (find-if #'(lambda (x) (member command (cdr x))) wpers-remaps)))
    (when rmp (car rmp))))

(defun wpers--pre-command () "Disabling functionality when one of folowing truely: 
 - visual-line-mode is non-nil,
 - marking is active, 
 - truncate-lines is nil 
 - command in `wpers-ovr-killing-funs'."
  (if (member this-command wpers-ovr-killing-funs)
      (wpers--ovr-kill)
      (let ((type (wpers--command-handler)))
        (when type
          (unless (or this-command-keys-shift-translated mark-active visual-line-mode (null truncate-lines))
            (condition-case err (funcall type this-command)
              (error (message (error-message-string err)) (beep)))
            (setq this-command 'wpers--NOP-command))))))

(defun wpers--post-command ()
  "Killing wpers--overlay when it is not at the point or text happens after it."
  (when wpers--overlay
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
                (wpers--ovr-put))))
        (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode public interface

(define-minor-mode wpers-mode
  "Toggle persistent cursor mode."
  :init-value nil
  :lighter " wpers"
  :group 'wpers
  (if wpers-mode
      (progn
        (message "Wpers enabled")
        (setq wpers--overlay nil)
        (add-hook 'buffer-list-update-hook #'wpers--adapt-ovrs)
        (mapc #'(lambda (x) (add-hook (car x) (cdr x) nil t)) wreps--hooks-alist))
      (progn
        (message "Wpers disabled")
        (wpers--ovr-kill)
        (wpers--ovr-kill-dangling)
        (remove-hook 'buffer-list-update-hook #'wpers--adapt-ovrs)
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
  '((wpers--vert-handler  next-line previous-line
                          scroll-up scroll-down
                          scroll-up-command scroll-down-command
                          cua-scroll-down cua-scroll-up
                          scroll-up-line scroll-down-line)
    (wpers--left-handler  left-char backward-char backward-delete-char backward-delete-char-untabify)
    (wpers--right-handler right-char forward-char)
    (wpers--mouse-handler mouse-set-point))
  "alist which is linking general remap-functions with commands.
Each element looks like (HANDLER . LIST-OF-COMMANDS) where
  HANDLER          - one of wpers--remap-xxx functions, 
  LIST-OF-COMMANDS - list of commands"
  :options '(wpers--vert-handler wpers--left-handler wpers--right-handler wpers--mouse-handler)
  :type '(alist :key-type symbol :value-type (repeat (choice function (list symbol string)))))

(provide 'wpers)
;;; wpers.el ends here
