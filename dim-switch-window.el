;;; dim-switch-window.el
;;
;; Offer a *visual* way to choose a window to switch to
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/switch-window.el
;; Version: 0.3
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
      (insert (concat "\n\n    " (number-to-string num)))
      (text-scale-increase dim:switch-window-increase))

    (set-window-buffer win buf)
    buf))

(defun dim:switch-window ()
  "Display an overlay in each window showing a unique key, then
ask user for the window where move to"
  (interactive)
  (let ((config (current-window-configuration))
        (num 1)
	(redisplay-dont-pause t)
	key
        buffers)
    
    ;; display big numbers to ease window selection
    (dolist (win (window-list))
      (setq buffers (cons (dim:switch-window-display-number win num) buffers))
      (setq num (1+ num)))

    ;; choose a window, asking the user when it makes sense ok we could have
    ;; created useless buffers we'll kill. That's either 1 or 2 buffers,
    ;; though. Not excited enough to fix that.
    ;;
    ;; Remember than (eq num (1+ (length (window-list))))
    (cond 
     ((eq num 2)
      (setq key 1))

     ((eq num 3)
      (setq key 2))

     (t
      (while (not key)
	(let ((input 
	       (event-basic-type
		(read-event "Move to window: " nil dim:switch-window-timeout))))
	  
	  (when (or (null input) (not (symbolp input)))
	    (cond ((null input) ; reached timeout
		   (setq key 1))

		  ((and (<= 49 input) (>= 57 input)) ; 1 to 9
		   (setq key (- input 48)))

		  ((eq input 113) ; q
		   (setq key 1))))))))

    ;; get those huge numbers away
    (dolist (buf buffers)
      (kill-buffer buf))

    (set-window-configuration config)

    ;; move to selected window
    (setq num 1)
    (dolist (win (window-list))
      (when (eq num key)
	(select-window win))
      (setq num (1+ num)))

    (message "Moved to %S" (buffer-name (window-buffer (selected-window))))))

(global-set-key (kbd "C-x o") 'dim:switch-window)
(provide 'dim-switch-window)
