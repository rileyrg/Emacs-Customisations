;; generally loaded from init-erc.el ins a kiosk like mode
(require 'erc)
(setq erc-join-buffer 'buffer)
(defun my/erc-buffer-connected-p (buffer)
  "Check if ERC BUFFER is connected."
  (with-current-buffer buffer
    (and (erc-server-process-alive)
         erc-server-connected)))

;;https://emacs.stackexchange.com/questions/64361/how-to-check-if-erc-is-running
(defun my/erc-start-or-switch ()
  "Connects to ERC, or switch to last active buffer.

  This function serves multiple purposes:

  1. Check Active Buffers: It iterates through a predefined list of ERC buffers
     to determine if any of them are actively connected to an IRC server.

  2. Verify Connection Status: For each buffer, it checks whether the associated
     ERC process is alive and whether there is an established network connection
     to the server. This is done using the `erc-server-process-alive' function and
     the `erc-server-connected' variable.

  3. Switch to Active Buffer: If any buffer is found to be actively connected,
     the function switches to that buffer using `erc-track-switch-buffer'.

  4. Reconnect if Disconnected: If none of the checked buffers are connected,
     the function prompts the user to reconnect to the IRC server. If the user
     confirms, a new connection is initiated using the `erc' command with the
     server and port specified (`irc.libera.chat` on port 6667)."
  (interactive)
  (global-set-key (kbd "C-c C-q") 'rgr/erc-quit )
  (global-set-key (kbd "C-x b") 'erc-switch-to-buffer)
  (global-set-key (kbd "C-c x")  'rgr/erc-quit)
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
        (sleep-for 2)
        (rgr/erc-switch-to-channel)))))

(defun rgr/erc-switch-to-channel(&optional channel)
  (let ((c (or channel "#emacs")))
    (if (get-buffer c)
        (switch-to-buffer c))))

(setq kill-emacs-hook nil))

(defun rgr/erc-quit()
  (interactive)
  (erc-quit-server "")
  (when (rgr/erc-session)
    (kill-emacs)))
