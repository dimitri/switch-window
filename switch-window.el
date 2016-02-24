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

(defcustom switchw-increase 12
  "How much to increase text size in the window numbering, maximum"
  :type 'integer
  :group 'switch-window)

(defcustom switchw-timeout 5
  "After this many seconds, cancel the window switching"
  :type 'integer
  :group 'switch-window)

(defcustom switchw-threshold 2
  "Only active switch-window after this many windows open"
  :type 'integer
  :group 'switch-window)


(defcustom switchw-relative nil
  "Control the ordering of windows, when true this depends on current-window"
  :type 'boolean
  :group 'switch-window)

(defcustom switchw-shortcut-style 'quail
  "Use either keyboard layout or alphabet shortcut style"
  :type '(choice (const :tag "Alphabet" 'alphabet)
                 (const :tag "Keyboard Layout" 'quail)
                 (const :tag "Qwerty Homekeys Layout" 'qwerty))
  :group 'switch-window)

(defcustom switchw-qwerty-shortcuts
  '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")
  "The list of characters used when switchw-shortcut-style is 'qwerty'"
  :type 'list
  :group 'switch-window)

(define-obsolete-variable-alias 'switch-window-increase 'switchw-increase)
(define-obsolete-variable-alias 'switch-window-timeout 'switchw-timeout)
(define-obsolete-variable-alias 'switch-window-threshold 'switchw-threshold)
(define-obsolete-variable-alias 'switch-window-relative 'switchw-relative)
(define-obsolete-variable-alias 'switch-window-shortcut-style 'switchw-shortcut-style)
(define-obsolete-variable-alias 'switch-window-qwerty-shortcuts 'switchw-qwerty-shortcuts)

(defun switchw--list-keyboard-keys ()
  "Return a list of current keyboard layout keys"
  (cl-loop with layout = (split-string quail-keyboard-layout "")
           for row from 1 to 4
           nconc (cl-loop for col from 1 to 10
                          collect (nth (+ 1 (* 2 col) (* 30 row)) layout))))

(defun switchw--list-keys ()
  "Return a list of keys to use depending on `switchw-shortcut-style'"
  (cond ((eq switchw-shortcut-style 'qwerty)
         switchw-qwerty-shortcuts)
        ((eq switchw-shortcut-style 'alphabet)
         (cl-loop for i from 0 to 25
                  collect (byte-to-string (+ (string-to-char "a") i))))
        (t (switchw--list-keyboard-keys))))

(defun switchw--enumerate ()
  "Return a list of one-letter strings to label current windows"
  (cl-loop for w being the windows for x in (switchw--list-keys) collect x))

(defun switchw--label (num)
  "Return the label to use for a given window number"
  (nth (- num 1) (switchw--enumerate)))

(defun switchw--list (&optional from-current-window)
  "list windows for current frame, starting at top left unless
from-current-window is not nil"
  (if (or from-current-window switchw-relative)
      (window-list nil nil)
    (window-list nil nil (frame-first-window))))

(defun switchw--display-number (win num)
  "prepare a temp buffer to diplay in the window while choosing"
  (let* ((label (switchw--label num))
         (buf (get-buffer-create
               (format " *%s: %s*" label (buffer-name (window-buffer win))))))
    (with-current-buffer buf
      ;; increase to maximum switchw-increase
      (when (fboundp 'text-scale-increase)
        (text-scale-increase switchw-increase))
      ;; insert the label, with a hack to support ancient emacs
      (if (fboundp 'text-scale-increase)
          (insert label)
        (insert (propertize label 'face
                            (list :height (* (* h switchw-increase)
                                             (if (> w h) 2 1)))))))
    (set-window-buffer win buf)
    buf))

(defun switchw--apply-to-window-index (action n message-format)
  "apply action to given window index, target is the place of the
   window in (switchw--list)"
  (cl-loop for c from 1
           for win in (switchw--list)
           until (= c n)
           finally (funcall action win))
  ;; be verbose about it
  (unless (minibuffer-window-active-p (selected-window))
    (message message-format
             (substring-no-properties
              (buffer-name (window-buffer (selected-window)))))))

(defun switchw--list-eobp ()
  "Return a list of all the windows where `eobp' is currently
   true so that we can restore that important property (think
   auto scrolling) after switching."
  (cl-loop for win in (switchw--list)
           when (with-current-buffer (window-buffer win) (eobp))
           collect win))

(defun switchw--restore-eobp (eobp-window-list)
  "For each window in EOBP-WINDOW-LIST move the point to end of buffer."
  (cl-loop for win in eobp-window-list
           do (with-current-buffer (window-buffer win) (end-of-buffer))))

;;;###autoload
(defun switchw-delete-window ()
  "Display an overlay in each window showing a unique key, then
ask user which window to delete"
  (interactive)
  (if (> (length (window-list)) 1)
      (progn
        (let ((index (switchw--prompt "Delete window: ")))
          (switchw--apply-to-window-index 'delete-window index "")))))

(defalias 'switch-to-window 'switchw-delete-window)

;;;###autoload
(defun switchw-maximize-window ()
  "Display an overlay in each window showing a unique key, then
ask user which window to maximize"
  (interactive)
  (if (<= (length (window-list)) switchw-threshold)
      (call-interactively 'delete-other-windows)
    (progn
      (let ((index (switchw--prompt "Maximize window: "))
            (eobps (switchw--list-eobp)))
        (switchw--apply-to-window-index
         'select-window index "Maximize %S")
        (switchw--restore-eobp eobps))
      (delete-other-windows))))

;;;###autoload
(defun switchw ()
  "Display an overlay in each window showing a unique key, then
ask user for the window where move to"
  (interactive)
  (if (<= (length (window-list)) switchw-threshold)
      (call-interactively 'other-window)
    (progn
      (let ((index (switchw--prompt "Move to window: "))
            (eobps (switchw--list-eobp)))
        (switchw--apply-to-window-index 'select-window index "Moved to %S")
        (switchw--restore-eobp eobps)))))

(defalias 'switch-window 'switchw)

(defun switchw--prompt (prompt-message)
  "Display an overlay in each window showing a unique key, then
ask user for the window to select"
  (let ((config (current-window-configuration))
        (num 1)
        (minibuffer-num nil)
        (original-cursor (default-value 'cursor-type))
        (eobps (switchw--list-eobp))
        key buffers
        window-points
        dedicated-windows)

    ;; arrange so that C-g will get back to previous window configuration
    (unwind-protect
        (progn
          ;; hide cursor during window selection process
          (setq-default cursor-type nil)
          ;; display big numbers to ease window selection
          (dolist (win (switchw--list))
            (push (cons win (window-point win)) window-points)
            (when (window-dedicated-p win)
              (push (cons win (window-dedicated-p win)) dedicated-windows)
              (set-window-dedicated-p win nil))
            (if (minibuffer-window-active-p win)
                (setq minibuffer-num num)
              (push (switchw--display-number win num) buffers))
            (setq num (1+ num)))

          (while (not key)
            (let ((input
                   (event-basic-type
                    (read-event
                     (if minibuffer-num
                         (format "Move to window [minibuffer is %s]: "
                                 (switchw--label minibuffer-num))
                       prompt-message)
                     nil switchw-timeout))))

              (if (or (null input) (eq input 'return))
                  (progn
                    (switchw--restore-eobp eobps)
                    (keyboard-quit))	; timeout or RET
                (unless (symbolp input)
                  (let* ((wchars (mapcar 'string-to-char
                                         (switchw--enumerate)))
                         (pos (position input wchars)))
                    (if pos
                        (setq key (1+ pos))
                      (progn
                        (switchw--restore-eobp eobps)
                        (keyboard-quit)))))))))

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
