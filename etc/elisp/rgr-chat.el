(use-package erc :demand t
  :config
  (defun rgr/erc-switch-to-channel(&optional channel)
    (when (string= (or channel "#emacs") (buffer-name (current-buffer)))
      (switch-to-buffer (current-buffer))))

  (defun rgr/erc-start()
    (interactive)
    (unless(get-buffer "libera.chat:6697")
      (progn
        (erc-tls :server "irc.libera.chat" :port 6697 :nick "rgr")
        (add-hook 'erc-join-hook 'rgr/erc-switch-to-channel)))))

(provide 'rgr/chat)
