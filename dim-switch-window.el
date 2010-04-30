(defun my-switch-window-display-number (win num)
  "prepare a temp buffer to diplay in the window while choosing"
  (let ((buf (get-buffer-create
	      (concat " *" (number-to-string num) "*"))))
    (with-current-buffer buf
      (insert (concat "\n\n    " (number-to-string num)))
      (text-scale-adjust 12))

    (set-window-buffer win buf)
    (select-window win)
    (switch-to-buffer buf 'non-record)
    (sit-for 0)
    buf))

(defun my-switch-window ()
  "Display an overlay in each window showing a unique key, then
ask user for the window where move to"
  (interactive)
  (let ((config (current-window-configuration))
        (num 1)
	(redisplay-dont-pause t)
	key
        buffers)
    
    (dolist (win (window-list))
      (setq buffers (cons (my-switch-window-display-number win num) buffers))
      (setq num (1+ num)))

    (redisplay 1)
    (sit-for 1)
    (while (not key)
      (let ((input (event-basic-type (read-event "Move to window: " nil 5))))
	(when (and (not (symbolp input)) (<= 49 input) (>= 57 input))
	  (setq key (- input 48)))))
    (message "%S" key)

    (dolist (buf buffers)
      (kill-buffer buf)
      (set-window-configuration config))

    (setq num 1)
    (dolist (win (window-list))
      (when (eq num key)
	(select-window win))
      (setq num (1+ num)))))
