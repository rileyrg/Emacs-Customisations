;; -*- lexical-binding: t; -*-
;; generally loaded from init-erc.el ins a kiosk like mode
(require 'erc)

(add-hook 'find-file-hook (lambda()(read-only-mode 1)))
(set-face-background 'mode-line "orangered")
(defun my/erc-buffer-connected-p (buffer)
  "Check if ERC BUFFER is connected."
  (with-current-buffer buffer
    (and (erc-server-process-alive)
         erc-server-connected)))

;;https://emacs.stackexchange.com/questions/64361/how-to-check-if-erc-is-running
(defun my/erc-start-or-switch (&optional rest)
  (interactive)
  (let ((erc-buffers '("Libera.Chat" "irc.libera.chat" "irc.libera.chat:6667"))
        (connected nil))
    (dolist (buffer erc-buffers)
      (when (and (get-buffer buffer)
                 (my/erc-buffer-connected-p buffer))
        (setq connected t)))
    (if connected
        (rgr/erc-switch-to-channel)
      (progn
        (emacs-alert "Connecting to IRC")
        (erc :server "irc.libera.chat" :port 6667)
        (global-set-key (kbd "C-c C-q") 'rgr/irc-quit )))))

(defun rgr/erc-switch-to-channel(&optional frame channel)
  (let ((c (or channel "#emacs")))
    (if (get-buffer c)
        (switch-to-buffer c))))

(defun rgr/irc-quit()
  (interactive)
  (erc-quit-server "leaving emacs session")
  (when (functionp 'ement-disconnect)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'ement-disconnect))))

(setq kill-emacs-hook '(rgr/irc-quit))

(use-package ement
  :disabled t
  :demand t
  :bind ("M-g M-l" . 'ement-room-list))

(defun rgr/irc-reconnect(&optional f)
  (interactive)
  (my/erc-start-or-switch)
  (if (featurep 'ement)
      (if ement-sessions
      (ement-room-list)
    (call-interactively 'ement-connect))))

(add-hook 'server-after-make-frame-hook #'rgr/irc-reconnect)
;;  (add-hook 'elpaca-after-init-hook (lambda()(run-with-idle-timer 1 nil #'rgr/irc-reconnect)))
