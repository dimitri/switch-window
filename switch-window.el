;;; dim-switch-window.el --- A *visual* way to choose a window to switch to
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/switch-window.el
;; Version: 0.9
;; Created: 2010-04-30
;; Keywords: window navigation
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install:
;;  (require 'switch-window)
;;
;; It'll take over your C-x o binding.
;;
;; Changelog
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

;; We use loop and subseq
(require 'cl)
(require 'quail)

(defgroup switch-window nil "switch-window customization group"
  :group 'convenience)

(defcustom switch-window-increase 12
  "How much to increase text size in the window numbering, maximum"
  :type 'integer
  :group 'switch-window)

(defcustom switch-window-timeout 5
  "After this many seconds, cancel the window switching"
  :type 'integer
  :group 'switch-window)

(defcustom switch-window-relative nil
  "Control the ordering of windows, when true this depends on current-window"
  :type 'boolean
  :group 'switch-window)

(defun switch-window-enumerate ()
  "Return a list of one-letter strings to label current windows"
  (subseq
   (loop with layout = (split-string quail-keyboard-layout "")
	 for row from 1 to 4
	 nconc (loop for col from 1 to 10
		     collect (nth (+ 1 (* 2 col) (* 30 row)) layout)))
   0 (length (switch-window-list))))

(defun switch-window-label (num)
  "Return the label to use for a given window number"
  (nth (- num 1) (switch-window-enumerate)))

(defun switch-window-list (&optional from-current-window)
  "list windows for current frame, starting at top left unless
from-current-window is not nil"
  (if (or from-current-window switch-window-relative)
      (window-list nil nil)
    (window-list nil nil (frame-first-window))))

(defun switch-window-display-number (win num)
  "prepare a temp buffer to diplay in the window while choosing"
  (let* ((label (switch-window-label num))
	 (buf (get-buffer-create
	       (format " *%s: %s*" label (buffer-name (window-buffer win))))))
    (with-current-buffer buf
      (let* ((w (window-width win))
	     (h (window-body-height win))
	     (increased-lines (/ (float h) switch-window-increase))
	     (scale (if (> increased-lines 1) switch-window-increase h))
	     (lines-before (/ increased-lines 2))
	     (margin-left (/ w h) ))
	;; increase to maximum switch-window-increase
	(when (fboundp 'text-scale-increase)
	  (text-scale-increase scale))
	;; make it so that the huge number appears centered
	(dotimes (i lines-before) (insert "\n"))
	(dotimes (i margin-left)  (insert " "))
	;; insert the label, with a hack to support ancient emacs
        (if (fboundp 'text-scale-increase)
	    (insert label)
	  (insert (propertize label 'face
			      (list :height (* (* h switch-window-increase)
					       (if (> w h) 2 1))))))))
    (set-window-buffer win buf)
    buf))

(defun switch-to-window-number (n)
  "move to given window, target is the place of the window in (switch-window-list)"
  (let ((c 1))
    (dolist (win (switch-window-list))
      (when (eq c n)
	(select-window win))
      (setq c (1+ c)))
    (unless (minibuffer-window-active-p (selected-window))
      (message "Moved to %S"
	       (substring-no-properties
		(buffer-name (window-buffer (selected-window))))))))

(defun switch-window ()
  "Display an overlay in each window showing a unique key, then
ask user for the window where move to"
  (interactive)
  (if (< (length (window-list)) 3)
      (call-interactively 'other-window)

    (let ((config (current-window-configuration))
	  (num 1)
	  (minibuffer-num nil)
	  key buffers
	  window-points
	  dedicated-windows)

      ;; arrange so that C-g will get back to previous window configuration
      (unwind-protect
	  (progn
	    ;; display big numbers to ease window selection
	    (dolist (win (switch-window-list))
	      (push (cons win (window-point win)) window-points)
	      (when (window-dedicated-p win)
		(push (cons win (window-dedicated-p win)) dedicated-windows)
		(set-window-dedicated-p win nil))
	      (if (minibuffer-window-active-p win)
		  (setq minibuffer-num num)
		(push (switch-window-display-number win num) buffers))
	      (setq num (1+ num)))

	    (while (not key)
	      (let ((input
		     (event-basic-type
		      (read-event
		       (if minibuffer-num
			   (format "Move to window [minibuffer is %s]: "
				   (switch-window-label minibuffer-num))
			 "Move to window: ")
		       nil switch-window-timeout))))

		(if (or (null input) (eq input 'return))
		    (keyboard-quit) ; timeout or RET
		  (unless (symbolp input)
		    (let* ((wchars (mapcar 'string-to-char
					   (switch-window-enumerate)))
			   (pos (position input wchars)))
		      (if pos
			  (setq key (1+ pos))
			(keyboard-quit))))))))

	;; get those huge numbers away
	(mapc 'kill-buffer buffers)
	(set-window-configuration config)
	(dolist (w window-points)
	  (set-window-point (car w) (cdr w)))
	(dolist (w dedicated-windows)
	  (set-window-dedicated-p (car w) (cdr w)))
	(when key
	  (switch-to-window-number key))))))

(global-set-key (kbd "C-x o") 'switch-window)
(provide 'switch-window)
