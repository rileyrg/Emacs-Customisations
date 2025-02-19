(add-hook 'elpaca-after-init-hook (lambda()(run-with-idle-timer 1 nil  (lambda()(dired "~")))))
(add-hook 'server-after-make-frame-hook (lambda(&optional f)(run-with-idle-timer 1 nil  (lambda()(switch-to-buffer (other-buffer))))))
