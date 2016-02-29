;;; switch-window.el --- A *visual* way to choose a window to switch to
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: https://github.com/dimitri/switch-window
;;      http://tapoueh.org/emacs/switch-window.html
;; Git-URL: https://github.com/dimitri/switch-window.git
;; Version: 0.11
;; Created: 2010-04-30
;; Keywords: window navigation
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;; Package-Requires: ((cl-lib "0.5"))
;;
;; This file is NOT part of GNU Emacs.
;;

;;; Commentary:
;;
;; * Usage
;;
;;    (require 'switch-window)
;;    (global-set-key (kbd "C-x o") 'switch-window)
;;
;; * Changelog
;;
;; 0.11 - 2013-09-14
;;
;;  - restore point to end-of-buffer for windows where it was the case after
;;    switching, fixing an anoying bug.
;;
;; 0.10 - 2011-06-19
;;
;;  - implement M-x delete-other-window (thanks developernotes on github)
;;
;; 0.9 - 2010-11-11 - emacs22 called, it wants some support
;;
;;  - implement a propertize based hack to support emacs22
;;
;; 0.8 - 2010-09-13 - 999
;;
;;  - Suport more than 9 windows (with a single key to type)
;;  - Use quail-keyboard-layout to choose single key labels for windows
;;
;; 0.7 - 2010-08-23 - window-dedicated-p
;;
;;  - temporarily unset the window dedicated flag for displaying the
;;    numbers, patch from René Kyllingstad <Rene@Kyllingstad.com>
;;  - fix timeout and RET handling wrt to not changing window selection
;;
;; 0.6 - 2010-08-12 - *Minibuf-1*
;;
;;  - add support for selecting the minibuffer when it's active
;;  - some try at a better horizontal centering
;;  - assorted cleanup
;;
;; 0.5 - 2010-08-08 - Polishing
;;
;;  - dim:switch-window-increase is now a maximum value
;;

;;; Code:
(require 'cl-lib) ; We use cl-loop and cl-subseq
(require 'quail)

(defgroup switch-window nil
  "switch-window customization group"
  :group 'convenience)

(defcustom switch-window-increase 12
  "How much to increase text size in the window numbering, maximum"
  :type 'integer
  :group 'switch-window)

(defcustom switch-window-timeout 5
  "After this many seconds, cancel the window switching"
  :type 'integer
  :group 'switch-window)

(defcustom switch-window-threshold 2
  "Only active switch-window after this many windows open"
  :type 'integer
  :group 'switch-window)

(defcustom switch-window-relative nil
  "Control the ordering of windows, when true this depends on current-window"
  :type 'boolean
  :group 'switch-window)

(defcustom switch-window-shortcut-style 'quail
  "Use either keyboard layout or alphabet shortcut style"
  :type '(choice (const :tag "Alphabet" 'alphabet)
                 (const :tag "Keyboard Layout" 'quail)
                 (const :tag "Qwerty Homekeys Layout" 'qwerty))
  :group 'switch-window)

(defcustom switch-window-qwerty-shortcuts
  '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")
  "The list of characters used when switch-window-shortcut-style is 'qwerty'"
  :type 'list
  :group 'switch-window)

(defcustom switch-window-minibuffer-shortcut nil
  "Whether to customize the minibuffer shortcut.
Default to no customisation (nil), which will make the minibuffer take whatever the last short is.
If a character is specified it will always use that key for the minibuffer shortcut."
  :type '(choice (const :tag "Off" nil)
                 (character "m"))
  :group 'switch-window)

(defcustom switch-window-configuration-change-hook-inhibit nil
  "Whether inhibit `window-configuration-change-hook' during switch-window."
  :type 'boolean
  :group 'switch-window)

(defun switch-window--list-keyboard-keys ()
  "Return a list of current keyboard layout keys"
  (cl-loop with layout = (split-string quail-keyboard-layout "")
           for row from 1 to 4
           nconc (cl-loop for col from 1 to 10
                          collect (nth (+ 1 (* 2 col) (* 30 row)) layout))))

(defun switch-window--list-keys ()
  "Return a list of keys to use depending on `switch-window-shortcut-style'"
  (remove
   (when switch-window-minibuffer-shortcut
     (char-to-string switch-window-minibuffer-shortcut))
   (cond ((eq switch-window-shortcut-style 'qwerty)
          switch-window-qwerty-shortcuts)
         ((eq switch-window-shortcut-style 'alphabet)
          (cl-loop for i from 0 to 25
                   collect (byte-to-string (+ (string-to-char "a") i))))
         (t (switch-window--list-keyboard-keys)))))

(defun switch-window--enumerate ()
  "Return a list of one-letter strings to label current windows"
  (cl-loop for w in (switch-window--list)
           for x in (switch-window--list-keys)
           collect (if (and switch-window-minibuffer-shortcut
                            (minibuffer-window-active-p w))
                       (char-to-string switch-window-minibuffer-shortcut)
                     x)))

(defun switch-window--label (num)
  "Return the label to use for a given window number"
  (nth (- num 1) (switch-window--enumerate)))

(defun switch-window--list (&optional from-current-window)
  "list windows for current frame, starting at top left unless
from-current-window is not nil"
  (if (or from-current-window switch-window-relative)
      (window-list nil nil)
    (window-list nil nil (frame-first-window))))

(defun switch-window--display-number (win num)
  "prepare a temp buffer to diplay in the window while choosing"
  (let* ((label (switch-window--label num))
         (buf (get-buffer-create
               (format " *%s: %s*" label (buffer-name (window-buffer win))))))
    (with-current-buffer buf
      (let ((w (window-width win))
            (h (window-body-height win)))
        ;; increase to maximum switch-window-increase
        (when (fboundp 'text-scale-increase)
          (text-scale-increase switch-window-increase))
        ;; insert the label, with a hack to support ancient emacs
        (if (fboundp 'text-scale-increase)
            (insert label)
          (insert (propertize label 'face
                              (list :height (* (* h switch-window-increase)
                                               (if (> w h) 2 1)))))))
      (set-window-buffer win buf)
      buf)))

(defun switch-window--jump-to-window (index)
  "Jump to the window which index is `index'."
  (cl-loop for c from 1
           for win in (switch-window--list)
           until (= c index)
           finally (select-window win)))

(defun switch-window--list-eobp ()
  "Return a list of all the windows where `eobp' is currently
   true so that we can restore that important property (think
   auto scrolling) after switching."
  (cl-loop for win in (switch-window--list)
           when (with-current-buffer (window-buffer win) (eobp))
           collect win))

(defun switch-window--restore-eobp (eobp-window-list)
  "For each window in EOBP-WINDOW-LIST move the point to end of buffer."
  (cl-loop for win in eobp-window-list
           do (let ((buffer (window-buffer win)))
                (when buffer
                  (with-current-buffer buffer
                    (goto-char (point-max)))))))

;;;###autoload
(defun switch-window-then-delete ()
  "Display an overlay in each window showing a unique key, then
ask user which window to delete"
  (interactive)
  (switch-window--then
   "Delete window: "
   #'delete-window
   #'delete-window t))

(defalias 'delete-other-window 'switch-window-then-delete)
(make-obsolete 'delete-other-window 'switch-window-then-delete
               "switch-window version 0.2")

;;;###autoload
(defun switch-window-then-maximize ()
  "Display an overlay in each window showing a unique key, then
ask user which window to maximize"
  (interactive)
  (switch-window--then
   "Maximize window: "
   #'delete-other-windows
   #'delete-other-windows t))

;;;###autoload
(defun switch-window ()
  "Display an overlay in each window showing a unique key, then
ask user for the window where move to"
  (interactive)
  (switch-window--then
   "Move to window: "
   #'(lambda () (other-window 1))))

;;;###autoload
(defun switch-window-then-split-horizontally (arg)
  "Select a window then split it horizontally."
  (interactive "P")
  (switch-window--then
   "Horiz-split window: "
   #'split-window-horizontally
   #'split-window-horizontally arg 1))

;;;###autoload
(defun switch-window-then-split-vertically (arg)
  "Select a window then split it vertically."
  (interactive "P")
  (switch-window--then
   "Verti-split window: "
   #'split-window-vertically
   #'split-window-vertically arg 1))

;;;###autoload
(defun switch-window-then-split-below (arg)
  "Select a window then split it with split-window-below's mode."
  (interactive "P")
  (switch-window--then
   "Below-split window: "
   #'split-window-below
   #'split-window-below arg 1))

;;;###autoload
(defun switch-window-then-split-right (arg)
  "Select a window then split it with split-window-right's mode."
  (interactive "P")
  (switch-window--then
   "Right-split window: "
   #'split-window-right
   #'split-window-right arg 1))

;;;###autoload
(defun switch-window-then-swap-buffer (arg)
  "Select a window then swap it buffer with current window's buffer."
  (interactive "P")
  (let ((buffer1 (window-buffer))
        (window1 (get-buffer-window))
        buffer2 window2)
    (switch-window)
    (setq buffer2 (current-buffer))
    (setq window2 (get-buffer-window))
    (set-window-buffer window2 buffer1)
    (set-window-buffer window1 buffer2)
    (if arg
        (select-window window1)
      (select-window window2))))

(defun switch-window--then (prompt function1 &optional function2
                                   return-original-window threshold)
  "If the number of opened window is less than `threshold', call `function1'
in current window, otherwise, switch to the window assocated with the typed key,
then call `function2'.

1. `function1' and `function2' are functions with no arguments.
2. When `return-original-window' is t, switch to original window
   after `function2' is called.
3. When `threshold' is not a number, use the value of
   `switch-window-threshold' instead."
  (if (<= (length (window-list))
          (if (numberp threshold)
              threshold
            switch-window-threshold))
      (when (functionp function1)
        (funcall function1))
    (let ((orig-window (selected-window))
          (index (switch-window--prompt prompt))
          (eobps (switch-window--list-eobp)))
      (switch-window--jump-to-window index)
      (when (functionp function2)
        (funcall function2))
      (when (and return-original-window
                 (window-live-p orig-window))
        (select-window orig-window))
      (switch-window--restore-eobp eobps))))

(defun switch-window--prompt (prompt-message)
  "Display an overlay in each window showing a unique key, then
ask user for the window to select"
  (let ((config (current-window-configuration))
        (num 1)
        (minibuffer-num nil)
        (original-cursor (default-value 'cursor-type))
        (eobps (switch-window--list-eobp))
        (window-configuration-change-hook
         (unless switch-window-configuration-change-hook-inhibit
           window-configuration-change-hook))
        key buffers
        window-points
        dedicated-windows)

    ;; arrange so that C-g will get back to previous window configuration
    (unwind-protect
        (progn
          ;; hide cursor during window selection process
          (setq-default cursor-type nil)
          ;; display big numbers to ease window selection
          (dolist (win (switch-window--list))
            (push (cons win (window-point win)) window-points)
            (when (window-dedicated-p win)
              (push (cons win (window-dedicated-p win)) dedicated-windows)
              (set-window-dedicated-p win nil))
            (if (minibuffer-window-active-p win)
                (setq minibuffer-num num)
              (push (switch-window--display-number win num) buffers))
            (setq num (1+ num)))

          (while (not key)
            (let ((input
                   (event-basic-type
                    (read-event
                     (if minibuffer-num
                         (format "Move to window [minibuffer is %s]: "
                                 (if switch-window-minibuffer-shortcut
                                     (char-to-string switch-window-minibuffer-shortcut)
                                   (switch-window--label minibuffer-num)))
                       prompt-message)
                     nil switch-window-timeout))))

              (if (or (null input) (eq input 'return))
                  (progn
                    (switch-window--restore-eobp eobps)
                    (keyboard-quit))	; timeout or RET
                (unless (symbolp input)
                  (let* ((wchars (mapcar 'string-to-char
                                         (switch-window--enumerate)))
                         (pos (cl-position input wchars)))
                    (if pos
                        (setq key (1+ pos))
                      (progn
                        (switch-window--restore-eobp eobps)
                        (keyboard-quit)))))))))
      ;; clean input-method-previous-message
      (setq input-method-previous-message nil)
      ;; restore original cursor
      (setq-default cursor-type original-cursor)
      ;; get those huge numbers away
      (mapc 'kill-buffer buffers)
      (set-window-configuration config)
      (dolist (w window-points)
        (set-window-point (car w) (cdr w)))
      (dolist (w dedicated-windows)
        (set-window-dedicated-p (car w) (cdr w))))
    key))

(provide 'switch-window)
;;; switch-window.el ends here
