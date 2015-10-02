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
;; Just turn `wpers-mode` or `global-wpers-mode` and cursor
;; will be moving with saving his position at the line (column).

;;; Installation & using:

;; Just add wpers.el folder location to load-path and call `require`

;;         (add-to-list 'load-path <path-to-wpers>)
;;         (require 'wpers)

;; Then call command `wpers-mode` for toggle mode in current buffer

;;         M-x wpers-mode

;; or `global-wpers-mode` for toggle mode in all buffers

;;         M-x global-wpers-mode

;;; Сustomization:

;; Call command `wpers-overlay-visible` or customise `wpers-pspace`
;; for adjusting visibility of overlay.

;; Customize `wpers-ovr-killing-funs` to define which functions
;; reset the vertical position of the cursor (column).

;; Set the `wpers-remaps` to define the list of functions saving
;; the vertical position of the cursor (column).

;;; Note

;; Mode is compatible with build-in line hilight `hl-line-mode`
;; but may have visualization problems with other similar modes.

;;; Code:

;;;;;;;;;;
;;; Import

(require 'cl)
(require 'hl-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode constants, variables 

(defconst wpers--prefix "wpers"
  "Package prefix")

(defconst wpers--pspace-def ?\xB7 "Default char for overlay displaying.")

(defconst wpers--ovr-txt-prop 'before-string)

(defvar-local wpers--overlay nil
  "Overlay to shift the cursor to the right.")

(defvar-local wpers--shadow-overlays nil
  "List of the overlays in inactive windows.")

(defvar wpers--command nil "Wrapped command")

(defgroup wpers nil
  "Persistent cursor"
  :group 'convenience
  :prefix "wpers-")

(defconst wreps--hooks-alist
  '((pre-command-hook wpers--pre-command t)
    (post-command-hook wpers--post-command t)
    (buffer-list-update-hook wpers--adapt-ovrs))
  "alist (hook-var . hook-function) for wpers-mode.")

;;;;;;;;;
;;; Utils

(defun wpers--intern (&rest xs)
  (intern (apply 'concat wpers--prefix "--" 
                 (mapcar #'(lambda (x) (if (stringp x) x (symbol-name x))) xs))))

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

(defun wpers--group-by-kwd (xs)
  (let (res)
    (dolist (x xs (reverse (mapcar #'reverse res)))
      (if (keywordp x)
          (push (list x) res)
          (push x (car res))))))

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
  (push wpers--overlay wpers--shadow-overlays))
  
(defun wpers--ovr-from-shadow (w)
  "Restore active overlay in the window W from previously saved state."
  (let ((sh-ovr (find-if #'(lambda (x) (eq (overlay-get x 'window) w)) wpers--shadow-overlays)))
    (if sh-ovr
        (setq wpers--overlay sh-ovr
              wpers--shadow-overlays (remove sh-ovr wpers--shadow-overlays))
        (setq wpers--overlay nil))))

(defun wpers--ovr-kill-dangling ()
  "Killing of all \"dangling\" (owned by nothing) overlays"
  (let ((wl (window-list)) new-sh-ovs)
    (dolist (ovr wpers--shadow-overlays)
      (let* ((w (overlay-get ovr 'window))
             (b (and ovr (overlay-buffer ovr))))
        (if (not (and ovr w b (member w wl) (buffer-local-value 'wpers-mode b)))
            (delete-overlay ovr)
            (push ovr new-sh-ovs))))
    (setq wpers--shadow-overlays new-sh-ovs)))

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
  (let* ((last-column (string-width (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
         (spcs-needed (- col last-column)))
    (when (plusp spcs-needed)
      (wpers--ovr-make spcs-needed))))

;;;;;;;;;;;;;;;;;;;;
;;; Command handlers

(defun wpers--vert-handler (&optional command)
  "The handler for commands that move the cursor vertically."
  (interactive)
  (let ((command (if (called-interactively-p 'any) wpers--command command))
        (old-col (wpers--current-column)))
    (call-interactively command)
    (unless goal-column
      (wpers--move-to-column old-col))))

(defun wpers--hor-worker (&rest task-stream)
  (dolist (task (wpers--group-by-kwd task-stream))
    (pcase (car task)
           (:exec (let ((current-prefix-arg (if (cddr task) (caddr task) current-prefix-arg))) 
                    (call-interactively (cadr task))))
           (:make (wpers--ovr-make (cadr task)))
           (:kill (wpers--ovr-kill))
           (:put  (wpers--ovr-put (cadr task))))))

(defun wpers--left-handler (&optional command)
  "The handler for commands that move the cursor to the left."
  (interactive)
  (let ((command (if (called-interactively-p 'any) wpers--command command)))
    (if wpers--overlay
        (if (and (wpers--ovr-at-point-p) (wpers--at-end))
            (let* ((arg (prefix-numeric-value current-prefix-arg))
                   (ovr-len (wpers--ovr-len))
                   (rest-len (- ovr-len arg)))
              (if (plusp ovr-len)
                  (if (>= rest-len 0)
                      (wpers--hor-worker :put rest-len)
                      (wpers--hor-worker :kill :exec command (- rest-len)))
                  (wpers--hor-worker :kill :exec command)))
            (wpers--hor-worker :kill :exec command))   
        (wpers--hor-worker :exec command))))

(defun wpers--right-handler (&optional command)
  "The handler for commands that move the cursor to the right."
  (interactive)
  (let ((command (if (called-interactively-p 'any) wpers--command command))
        (arg (prefix-numeric-value current-prefix-arg)))
    (if (wpers--at-end)
        (if (null wpers--overlay)
            (wpers--hor-worker :make arg)
            (if (wpers--ovr-at-point-p)
                (wpers--hor-worker :put (+ (wpers--ovr-len) arg))
                (wpers--hor-worker :make arg)))
        (let* ((to-eol-len (string-width (buffer-substring-no-properties (point) (line-end-position))))
               (rest-len (- to-eol-len arg)))
          (wpers--hor-worker :exec command (if (minusp rest-len) to-eol-len arg))
          (when (minusp rest-len) (wpers--hor-worker :make (- rest-len)))))))

(defun wpers--mouse-handler (&optional command)
  "The handler for commands that move mouse cursor."
  (interactive)
  (let ((command (if (called-interactively-p 'any) wpers--command command)))
    (call-interactively command)
    (wpers--move-to-column (car (posn-col-row (cadr last-input-event))))))

;;;;;;;;;;;;;;;;;;;
;;; Command advices

(defun wpers--add-advice (h fs)
  (dolist (f (if (listp fs) fs (list fs))) 
    (let ((old-fn (wpers--intern "old-" f))
          (ard-fn (wpers--intern f)))
      (unless (advice-function-member-p ard-fn (symbol-function f))
        (fset old-fn (symbol-function f))
        (fset ard-fn
              `(lambda (org-fun &rest r)
                 (if (and wpers-mode (called-interactively-p 'any))
                     (condition-case err (funcall ',h ',old-fn)
                       (error (message (error-message-string err)) (beep)))
                     (apply ',old-fn r))))
        (advice-add f :around ard-fn)))))

(defun wpers--add-advices ()
  (dolist (r wpers-remaps) (wpers--add-advice (car r) (cdr r))))

(defun wpers--remove-advices ()
  (dolist (f (apply 'append (mapcar 'cdr wpers-remaps))) 
    (advice-remove f (wpers--intern f))
    (fset (wpers--intern "old-" f) nil)))

;;;;;;;;;
;;; Hooks

(defun wpers--get-command-handler (&optional command)
  "Returns handler for a command or nil if handler absent"
  (let* ((command (or command this-command))
         (rmp (find-if #'(lambda (x) (member command (cdr x))) wpers-remaps)))
    (when rmp (car rmp))))

(defun wpers--pre-command () "Disabling functionality when one of folowing truely: 
 - visual-line-mode is non-nil,
 - marking is active, 
 - truncate-lines is nil 
 - command in `wpers-ovr-killing-funs'."
  (if (or (and (not wpers-adviced) (eq this-command 'execute-extended-command))
          (member this-command wpers-ovr-killing-funs))
      (wpers--ovr-kill)
      (let ((handler (wpers--get-command-handler)))
        (when (and handler (not wpers-adviced))
          (unless (or this-command-keys-shift-translated mark-active visual-line-mode (null truncate-lines))
            (setq wpers--command this-command  
                  this-command handler))))))

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
  (wpers--ovr-update))

(defun wpers--set-adviced (var val)
  "Setter for custom `wpers-adviced'"
  (set var val)
  (if val (wpers--add-advices) (wpers--remove-advices)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode public interface

(define-minor-mode wpers-mode
  "Toggle persistent cursor mode."
  :init-value nil
  :lighter " wpers"
  :group 'wper
  (if wpers-mode
      (progn
        (message "Wpers enabled")
        (setq wpers--overlay nil)
        (mapc #'(lambda (x) (add-hook (car x) (cadr x) nil (caddr x))) wreps--hooks-alist)
        (when wpers-adviced (wpers--add-advices)))
      (progn
        (message "Wpers disabled")
        (wpers--ovr-kill)
        (wpers--ovr-kill-dangling)
        (mapc #'(lambda (x) (remove-hook (car x) (cadr x) (caddr x))) wreps--hooks-alist)
        (unless (remove-if-not #'(lambda (x) (buffer-local-value 'wpers-mode x)) (buffer-list))
          (wpers--remove-advices)))))

(defun wpers-mode-maybe ()
  "What buffer `wpers-mode' prefers."
  (unless (or wpers-mode (minibufferp (current-buffer)))
    (wpers-mode 1)))

(define-global-minor-mode global-wpers-mode
  wpers-mode wpers-mode-maybe 
  :group 'wpers)

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

(defcustom wpers-adviced nil
  "If non-nil then uses advice for providing interactive function calls."
  :type 'boolean
  :set 'wpers--set-adviced)

(provide 'wpers)
;;; wpers.el ends here
