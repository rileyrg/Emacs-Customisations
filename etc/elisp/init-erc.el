;; generally loaded from init-erc.el ins a kiosk like mode
(require 'erc)
(defun rgr/erc-switch-to-channel(&optional channel)
  (when (string= (or channel "#emacs") (buffer-name (current-buffer)))
    (switch-to-buffer (current-buffer))))

(defun rgr/erc-start()
  (interactive)
  (define-key erc-mode-map (kbd "C-c C-q") 'rgr/erc-quit )
  (when (rgr/erc-session)
    (global-set-key (kbd "C-x b") 'erc-switch-to-buffer)
    (global-set-key (kbd "C-c x")  'rgr/erc-quit)
    (setq kill-emacs-hook nil))
  (if(get-buffer "Libera.Chat")
      (progn
        (emacs-alert "Switching to IRC...")
        (rgr/erc-switch-to-channel))
    (progn
      (emacs-alert "IRC Starting...")
      (erc-tls :server "irc.libera.chat" :port 6697)
      (add-hook 'erc-join-hook 'rgr/erc-switch-to-channel))))

(defun rgr/erc-quit()
  (interactive)
  (erc-quit-server "")
  (when (rgr/erc-session)
    (kill-emacs)))

(rgr/erc-start)
