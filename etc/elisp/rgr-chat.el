(use-package erc
  :demand
  :config
  (defun rgr/erc-switch-to-channel(&optional channel)
    (when (string= (or channel "#emacs") (buffer-name (current-buffer)))
      (switch-to-buffer (current-buffer))))

  (defun rgr/erc-start()
    (interactive)
    (unless(get-buffer "libera.chat:6697")
      (progn
        (erc-tls :server "irc.libera.chat" :port 6697)
        (add-hook 'erc-join-hook 'rgr/erc-switch-to-channel))))

  (defun rgr/erc-quit()
    (interactive)
    (erc-quit-server ""))

  :bind
  (:map erc-mode-map
        (("C-c C-q" . rgr/erc-quit))))

(provide 'rgr/chat)
