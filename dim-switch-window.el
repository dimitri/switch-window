;;; dim-switch-window.el
;;
;; Offer a *visual* way to choose a window to switch to
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/switch-window.el
;; Version: 0.6
;; Created: 2010-04-30
;; Keywords: window navigation
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install:
;;  (require 'dim-switch-window)
;;
;; It'll take over your C-x o binding.
;;
;; Changelog
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

(defgroup dim:switch-window nil "dim:switch-window customization group"
  :group 'convenience)

(defcustom dim:switch-window-increase 12
  "How much to increase text size in the window numbering, maximum"
  :type 'integer
  :group 'dim:switch-window)

(defcustom dim:switch-window-timeout 5
  "After this many seconds, cancel the window switching"
  :type 'integer
  :group 'dim:switch-window)

(defcustom dim:switch-window-relative nil
  "Control the ordering of windows, when true this depends on current-window"
  :type 'boolean
  :group 'dim:switch-window)

(defun dim:switch-window-list (&optional from-current-window)
  "list windows for current frame, starting at top left unless
from-current-window is not nil"
  (if (or from-current-window dim:switch-window-relative)
      (window-list nil nil)
    (window-list nil nil (window-at 0 0))))

(defun dim:switch-window-display-number (win num)
  "prepare a temp buffer to diplay in the window while choosing"
  (let ((buf (get-buffer-create
	      (concat " *"
		      (number-to-string num) 
		      ": " 
		      (buffer-name (window-buffer win))
		      "*"))))
    (with-current-buffer buf
      (let* ((w (window-width win))
	     (h (window-body-height win))
	     (increased-lines (/ (float h) dim:switch-window-increase))
	     (scale (if (> increased-lines 1) dim:switch-window-increase h))
	     (lines-before (/ increased-lines 2))
	     (margin-left (/ w h) ))
	;; increase to maximum dim:switch-window-increase
	(text-scale-increase scale)
	;; make it so that the huge number appears centered
	(dotimes (i lines-before) (insert "\n"))
	(dotimes (i margin-left)  (insert " "))
	(insert (number-to-string num))))

    (set-window-buffer win buf)
    buf))

(defun dim:switch-to-window-number (n)
  "move to given window, target is the place of the window in (dim:switch-window-list)"
  (let ((c 1))
    (dolist (win (dim:switch-window-list))
      (when (eq c n)
	(select-window win))
      (setq c (1+ c)))
    (unless (minibuffer-window-active-p (selected-window))
      (message "Moved to %S" 
	       (substring-no-properties 
		(buffer-name (window-buffer (selected-window))))))))

(defun dim:switch-window ()
  "Display an overlay in each window showing a unique key, then
ask user for the window where move to"
  (interactive)
  (if (< (length (window-list)) 3)
      (call-interactively 'other-window)

    (let ((config (current-window-configuration))
	  (num 1)
	  (minibuffer-num nil)
	  key buffers)

      ;; arrange so that C-g will get back to previous window configuration
      (unwind-protect 
	  (progn
	    ;; display big numbers to ease window selection
	    (dolist (win (dim:switch-window-list))
	      (if (minibuffer-window-active-p win)
		  (setq minibuffer-num num)
		(push (dim:switch-window-display-number win num) buffers))
	      (setq num (1+ num)))

	    (while (not key)
	      (let ((input 
		     (event-basic-type
		      (read-event 
		       (if minibuffer-num
			   (format "Move to window [minibuffer is %d]: " 
				   minibuffer-num)
			 "Move to window: ")
		       nil dim:switch-window-timeout))))

		(if (or (null input) (eq input 'return)) 
		    (keyboard-quit) ; timeout or RET
		  (unless (symbolp input)
		    (if (and (<= ?1 input) (>= ?9 input)) ; 1 to 9
			(setq key (- input 48))
		      (keyboard-quit)))))))

	;; get those huge numbers away
	(mapc 'kill-buffer buffers)
	(set-window-configuration config)
	(when key
	  (dim:switch-to-window-number key))))))

(global-set-key (kbd "C-x o") 'dim:switch-window)
(provide 'dim-switch-window)
