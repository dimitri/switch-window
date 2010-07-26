;;; dim-switch-window.el
;;
;; Offer a *visual* way to choose a window to switch to
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/switch-window.el
;; Version: 0.4
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

(defgroup dim:switch-window nil "dim:switch-window customization group"
  :group 'convenience)

(defcustom dim:switch-window-increase 12
  "How much to increase text size in the window numbering"
  :type 'integer
  :group 'dim:switch-window)

(defcustom dim:switch-window-timeout 5
  "After this many seconds, cancel the window switching"
  :type 'integer
  :group 'dim:switch-window)

(defun dim:switch-window-display-number (win num)
  "prepare a temp buffer to diplay in the window while choosing"
  (let ((buf (get-buffer-create
	      (concat " *"
		      (number-to-string num) 
		      ": " 
		      (buffer-name (window-buffer win))
		      "*"))))
    (with-current-buffer buf
      (insert "\n\n    " (number-to-string num))
      (text-scale-increase dim:switch-window-increase))

    (set-window-buffer win buf)
    buf))

(defun dim:switch-to-window-number (n)
  "move to given window, target is the place of the window in (window-list)"
  (let ((c 1))
    (unless (eq n 1)
      (dolist (win (window-list))
	(when (eq c n)
	  (select-window win))
	(setq c (1+ c)))

      (message "Moved to %S" (buffer-name (window-buffer (selected-window)))))))

(defun dim:switch-window ()
  "Display an overlay in each window showing a unique key, then
ask user for the window where move to"
  (interactive)
  (if (< (length (window-list)) 3)
      (call-interactively 'other-window)

    (let ((config (current-window-configuration))
	  (num 1)
	  key buffers)

      ;; arrange so that C-g will get back to previous window configuration
      (unwind-protect 
	  (progn
	    ;; display big numbers to ease window selection
	    (dolist (win (window-list))
	      (push (dim:switch-window-display-number win num) buffers)
	      (setq num (1+ num)))

	    (while (not key)
	      (let ((input 
		     (event-basic-type
		      (read-event "Move to window: " 
				  nil dim:switch-window-timeout))))
		
		(if (null input) (setq key 1) ; timeout
		  (unless (symbolp input)
		    (if (and (<= 49 input) (>= 57 input)) ; 1 to 9
			(setq key (- input 48))
		      (setq key 1)))))))

	;; get those huge numbers away
	(mapc 'kill-buffer buffers)
	(set-window-configuration config)
	(dim:switch-to-window-number key)))))

(global-set-key (kbd "C-x o") 'dim:switch-window)
(provide 'dim-switch-window)
