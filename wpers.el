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

(defconst wpers--symbol 'wpers
  "Package signature name")

(defconst wpers--pspace-def ?\xB7
  "Default char for overlay displaying (if it is visible).")

(defconst wpers--ovr-txt-prop 'before-string
  "Overlay property used for positioning cursor at the screen.")

(defvar-local wpers--overlay nil
  "Overlay used for positioning cursor at the screen in the seected window.")

(defvar wpers--command nil
  "Stored original command for further processing in one of wpers--xxx-handler.")

(defgroup wpers nil
  "Persistent cursor (see 'cursor beyond end of line' option at FAR editor (Windows) 
or 'virtual space' option at Midnight Commander (Unix))"
  :group 'convenience
  :prefix "wpers-")

(defconst wreps--hooks-alist
  '((pre-command-hook wpers--pre-command t)
    (post-command-hook wpers--post-command t)
    (buffer-list-update-hook wpers--adapt-ovrs)
    (window-configuration-change-hook wpers--adapt-ovrs))
  "alist (hook-var hook-function [local]) for wpers-mode.")

;;;;;;;;;
;;; Utils

(defun wpers--intern (&rest xs)
  "Make symbol with name `wpers--symbol' + \"--\" + XS elements (casted to string) concatenation."
  (intern (apply 'concat (symbol-name wpers--symbol) "--" 
                 (mapcar #'(lambda (x) (if (stringp x) x (symbol-name x))) xs))))

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
  "Groups by keywords, f.e. (:q 1 2 :w 3 :e) => ((:q 1 2) (:w 3) (:e))"
  (let (res)
    (dolist (x xs (reverse (mapcar #'reverse res)))
      (if (keywordp x)
          (push (list x) res)
          (push x (car res))))))

(defun wpers--buf-p (b)
  "Return non-nil when `wpers-mode' is active for B."
  (buffer-local-value 'wpers-mode b))

(defun wpers--ovr-p (ovr)
  "Return non-nil when OVR has non-nil 'wpers attribute."
  (overlay-get ovr wpers--symbol))

(defun wpers--exists-p ()
  "Return non-nil when any buffer with active `wpers-mode' exists."
  (find-if #'wpers--buf-p (buffer-list)))

(defun wpers--non-nil (x)
  (cond
    ((symbolp x) (or (and (local-variable-p x) (buffer-local-value x (current-buffer)))
                  (and (default-boundp x) (default-value x))))
    ((consp x) (or (wpers--non-nil (car x))
                   (wpers--non-nil (intern (concat "global-" (symbol-name (car x)))))))
    ((vectorp x) (not (wpers--non-nil (elt x 0))))))

(defun wpers--replace (str &rest repls)
  (if (null repls)
      str
      (apply #'wpers--replace
             (replace-regexp-in-string (car repls) (format "%s" (cadr repls)) str)
             (cddr repls))))

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

(defun wpers--ovr-find (w &optional b)
  (let ((w (or w (selected-window)))
        (b (or b (current-buffer))))
    (with-current-buffer b
      (find-if #'(lambda (x) (and (wpers--ovr-p x) (eq (overlay-get x 'window) w)))
               (overlays-in (point-min) (point-max))))))

(defun wpers--ovr-make (&optional len w)
  "Creating overlay with optional setting before-string to STR."
  (wpers--ovr-kill)
  (let* ((w (or w (selected-window)))
         (ovr (wpers--ovr-find w)))
    (if (null ovr)
      (setq wpers--overlay (make-overlay (point) (point)))
      (move-overlay ovr (point) (point))
      (setq wpers--overlay ovr))
    (overlay-put wpers--overlay wpers--symbol t)
    (overlay-put wpers--overlay 'window w)
    (when len (wpers--ovr-put len w))))

(defun wpers--ovr-at-point-p ()
  "Return t if wpers--overlay was placed at the point."
  (eq (point) (overlay-start wpers--overlay)))

(defun wpers--ovr-txt-after-p ()
  "Return t if exists something after overlay."
  (and wpers--overlay (not (wpers--at-end t)))) 

(defun wpers--ovr-to-spcs ()
  "Replacing overlay (wpers--overlay) with space chars."
  (when wpers--overlay
    (let ((ovr-size (when (wpers--ovr-at-point-p) (wpers--ovr-len))))
      (save-excursion
       (goto-char (overlay-start wpers--overlay))
       (insert (make-string (wpers--ovr-len) ?\s)))
      (when ovr-size (right-char ovr-size)))))
  
(defun wpers--ovr-kill (&optional buffer force-legz)
  "Killing of the wpers--overlay with the replacement of spacess, if necessary."
  (with-current-buffer (or buffer (current-buffer))
    (when wpers--overlay
      (when (or force-legz (not (wpers--at-end t))) (wpers--ovr-to-spcs))
      (overlay-put wpers--overlay wpers--ovr-txt-prop nil)
      (setq wpers--overlay nil))))

(defun wpers--ovr-destroy-all (&optional b)
  "Removes all overlays in thw buffer B that have non-nil attribute 'wpers"
  (with-current-buffer (or b (current-buffer))
    (remove-overlays (point-min) (point-max) wpers--symbol t)))

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
        (dolist (ovr (remove-if-not #'wpers--ovr-p (overlays-in (point-min) (point-max))))
          (overlay-put ovr wpers--ovr-txt-prop
                       (wpers--ovr-str (length (overlay-get ovr wpers--ovr-txt-prop))
                                       (overlay-get ovr 'window))))))))

(defun wpers--ovr-dup (w)
  (with-current-buffer (window-buffer w)
    (when wpers--overlay
      (let ((ovr (copy-overlay wpers--overlay)))
        (overlay-put ovr 'window w)))))

(defun wpers--ovr-kill-dangling ()
  "Killing of all \"dangling\" (owned by nothing) overlays"
  (let ((wl (window-list)))
    (dolist (b (buffer-list));(b (remove-if-not #'wpers--buf-p (buffer-list)))
      (with-current-buffer b
        (dolist (ovr (remove-if-not #'wpers--ovr-p
                                    (overlays-in (point-min) (point-max))))
          (unless (and wpers-mode (member (overlay-get ovr 'window) wl))
            (delete-overlay ovr)))))))
               
(defun wpers--adapt-ovrs ()
  "Adapt overlays for current windows/buffers disposition."
  (let ((sw (selected-window)))
    (setq wpers--overlay
          (find-if #'(lambda (x) (and (wpers--ovr-p x) (eq (overlay-get x 'window) sw)))
                   (overlays-in (point-min) (point-max)))))
  (dolist (w (remove-if-not #'wpers--buf-p
                            (window-list)
                            :key #'window-buffer))
    (unless (wpers--ovr-find w (window-buffer w))
      (wpers--ovr-dup w)))
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

(defun wpers--processor (&rest task-stream)
  "Simple processor for operations with overlays and  interactive command execution."
  (let ((cnd t) col)
    (dolist (task (wpers--group-by-kwd task-stream))
      (let ((% (car task)) (%1 (cadr task)) (%2 (caddr task)))
        (if (eq % :when)
            (setq cnd %1)
            (when cnd
              (pcase %
                     (:exec (let ((current-prefix-arg (or %2 current-prefix-arg))) 
                              (call-interactively %1)))
                     (:make (wpers--ovr-make %1))
                     (:move (wpers--move-to-column %1))
                     (:kill (wpers--ovr-kill))
                     (:save (setq col (wpers--current-column)))
                     (:rest (wpers--move-to-column col))
                     (:put  (wpers--ovr-put %1))
                     (:legz (wpers--ovr-kill nil t))
                     (_     (error "wpers--processor: unknown command %s" %))))
            (setq cnd t))))))

(defmacro wpers--with-comnarg (&rest body)
  "Execute BODY in the context where exists binding: 
  COMMAND = wpers--command (in interactive) or current variable COMMAND;
  ARG = numeric value of the current prefix arg." 
  `(let ((command (if (called-interactively-p 'any) wpers--command command))
         (arg (prefix-numeric-value current-prefix-arg)))
     ,@body))

(defun wpers--vert-handler (&optional command)
  "The handler for commands that move the cursor vertically."
  (interactive)
  (wpers--with-comnarg
   (if goal-column
       (wpers--processor :exec command :move goal-column)
       (wpers--processor :save :exec command :rest))))

(defun wpers--left-handler (&optional command)
  "The handler for commands that move the cursor to the left."
  (interactive)
  (wpers--with-comnarg
   (catch 'done
     (when (and wpers--overlay (wpers--ovr-at-point-p) (wpers--at-end))
       (let* ((ovr-len (wpers--ovr-len))
              (rest-len (- ovr-len arg)))
         (when (plusp ovr-len)
           (if (>= rest-len 0)
               (wpers--processor :put rest-len)
               (wpers--processor :kill :exec command (- rest-len)))
           (throw 'done nil))))
     (wpers--processor :kill :exec command))))

(defun wpers--right-handler (&optional command)
  "The handler for commands that move the cursor to the right."
  (interactive)
  (wpers--with-comnarg
   (if (wpers--at-end)
       (if (null wpers--overlay)
           (wpers--processor :make arg)
           (if (wpers--ovr-at-point-p)
               (wpers--processor :put (+ (wpers--ovr-len) arg))
               (wpers--processor :make arg)))
       (let* ((to-eol-len (string-width (buffer-substring-no-properties (point) (line-end-position))))
              (rest-len (- to-eol-len arg)))
         (wpers--processor :exec command (if (minusp rest-len) to-eol-len arg)
                           :when (minusp rest-len) :make (- rest-len))))))

(defun wpers--mouse-handler (&optional command)
  "The handler for commands that move mouse cursor."
  (interactive)
  (wpers--with-comnarg
    (wpers--processor :exec command :move (car (posn-col-row (cadr last-input-event))))))

(defun wpers--with-legalize-handler (&optional command)
  (interactive)                                              
  (wpers--with-comnarg
   (wpers--processor :when (eq arg 1) :legz :exec command)))

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
          (unless (some #'wpers--non-nil wpers-ovr-killing-vars)
            (setq wpers--command this-command  
                  this-command handler))))))

(defun wpers--post-command ()
  "Killing wpers--overlay when it is not at the point or text happens after it."
  (unless (wpers--ovr-find (selected-window))
    (wpers--ovr-make))
  (when (and wpers--overlay
             (or (not (wpers--ovr-at-point-p))
                 (wpers--ovr-txt-after-p)))
    (wpers--ovr-kill)))

;;;;;;;;;;;;;;;;;;;;
;;; Custom accessors etc

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

(defun wpers--indicator ()
  (let* ((c (current-column))
         (wc (wpers--current-column))
         (d (- wc c)))
    (wpers--replace wpers-mode-line-format
                    "%r"  (line-number-at-pos)
                    "%c"  c
                    "%wc" wc
                    "\\?\\(.*\\)%d\\(.*\\)\\?" (if (plusp d) (format "\\1%d\\2" d) "")
                    "%d" d)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode public interface

(define-minor-mode wpers-mode
  "Toggle persistent cursor mode."
  :init-value nil
  :lighter (:eval (wpers--indicator))
  :group 'wper
  (if wpers-mode
      (progn
        (message "Wpers enabled")
        (setq wpers--overlay nil)
        (mapc #'(lambda (x) (add-hook (car x) (cadr x) nil (caddr x))) wreps--hooks-alist)
        (when wpers-adviced (wpers--add-advices)))
      (progn
        (message "Wpers disabled")
        (wpers--ovr-destroy-all)
        (wpers--ovr-kill-dangling)
        (let ((any-alive (wpers--exists-p)))
          (mapc #'(lambda (x)
                    (when (or (caddr x) (not any-alive))
                      (remove-hook (car x) (cadr x) (caddr x))))
                wreps--hooks-alist)
          (unless any-alive (wpers--remove-advices))))))

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

(defun wpers-overlay-visible (val) "Switch on overlay visibility if VAL is nil, 
swtich off if - or (4) else set to VAL."
  (interactive "P")
  (wpers--set-pspace nil
    (cond
      ((null val) t)
      ((member val  '(- (4))) nil)
      ((eq val :toggle) (when (eq wpers-pspace ?\s) t))
      (t val))))

(defun wpers-toggle-overlay-visibility () "Toggle overlay visibility."
  (interactive)
  (wpers-overlay-visible :toggle))

(defcustom wpers-ovr-killing-funs '(undo move-end-of-line move-beginning-of-line)
  "Functions killing overlay"
  :type '(repeat function))

(defcustom wpers-ovr-killing-vars '(this-command-keys-shift-translated mark-active (visual-line-mode) (whitespace-mode) [truncate-lines])
  "Variables killing overlay"
  :type '(repeat (choice symbol (list symbol) (vector symbol))))

(defcustom wpers-remaps
  '((wpers--vert-handler  next-line previous-line
                          scroll-up scroll-down
                          scroll-up-command scroll-down-command                      
                          cua-scroll-down cua-scroll-up
                          scroll-up-line scroll-down-line)
    (wpers--left-handler  left-char backward-char backward-delete-char backward-delete-char-untabify)
    (wpers--right-handler right-char forward-char)
    (wpers--mouse-handler mouse-set-point)
    (wpers--with-legalize-handler set-goal-column))
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

(defcustom wpers-mode-line-format " ?[%wc=%c+%d]?" ; " {%r:%c?+%d?}"
  "Wpers-mode line format."
  :type 'string)

(provide 'wpers)
;;; wpers.el ends here
