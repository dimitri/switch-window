;;; dim-switch-window.el
;;
;; Offer a *visual* way to choose a window to switch to

(defgroup dim:switch-window nil "dim:switch-window customization group"
  :group 'convenience)

(defcustom dim:switch-window-increase 12
  "How much to increase text size in the window numbering"
  :type 'integer
  :group 'dim:switch-window)

(defun dim:switch-window-display-number (win num)
  "prepare a temp buffer to diplay in the window while choosing"
  (let ((buf (get-buffer-create
	      (concat " *" (number-to-string num) "*"))))
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

    ;; choose a window
    (while (not key)
      (let ((input (event-basic-type (read-event "Move to window: " nil 5))))
	(when (and (not (symbolp input)) (<= 49 input) (>= 57 input))
	  (setq key (- input 48)))))

    ;; get those huge numbers away
    (dolist (buf buffers)
      (kill-buffer buf))

    (set-window-configuration config)

    ;; move to selected window
    (setq num 1)
    (dolist (win (window-list))
      (when (eq num key)
	(select-window win))
      (setq num (1+ num)))))

(global-set-key (kbd "C-x o") 'dim:switch-window)
(provide 'dim-switch-window)
