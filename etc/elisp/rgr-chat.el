
(use-package erc :demand t
  :diminish erc-mode
  :config
  (require 'erc-replace)
  (defun textReadFont ()
    "Set font to a variable width (proportional) fonts in current buffer."
    (interactive)
    ;; (setq buffer-face-mode-face '(:family "arial"))
    ;; (buffer-face-mode)
    )

  (defun rgr/erc-switch-to-channel(&optional channel)
    (when (string= (or channel "#emacs") (buffer-name (current-buffer)))
      (switch-to-buffer (current-buffer))))

  (defun rgr/erc-quit()
    (when (get-buffer "irc.freenode.net:6667")
      (progn
        (switch-to-buffer(get-buffer "irc.freenode.net:6667"))
        (erc-cmd-QUIT "bye"))))

  (defun rgr/erc-start()
    (interactive)
    (unless(get-buffer "irc.freenode.net:6667")
      (progn
        (erc :server "irc.freenode.net"  :port 6667)
        (add-hook 'erc-join-hook 'rgr/erc-switch-to-channel))))

  (defun rgr/erc-format-nick (&optional user _channel-data)
    "Return the nickname of USER.
             See also `erc-format-nick-function'."
    ;; (when user (format "%-024s" (erc-server-user-nickname user))))
    (when user (format "%s" (erc-server-user-nickname user))))
  :hook
  (erc-mode . textReadFont))

(use-package slack :demand t
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (defhydra slack-hydra (:color gold :hint none)
    "
          team                       channel                   message              Entry
          -------------------------------------------------------------------------------------------------
          _c_: change current team   _s_: select channel       _i_: select im       _S_: start slack
          _r_: register team         _l_: channel list update  _u_: im list update  _C_: close connections
                                                                                    _q_: close hydra
          "
    ;; Team
    ("c" slack-change-current-team)
    ("r" slack-register-team)

    ;; Channel
    ("s" slack-channel-select)
    ("l" slack-channel-list-update)

    ;; Message
    ("i" slack-im-select)
    ("u" slack-im-list-update)

    ;; Entry
    ("S" slack-start)
    ("C" slack-ws-close)

    ("q"  nil                    "cancel"     :color orange))

  (slack-register-team
   :name "emacs-slack"
   :default t
   :token slack-api-token
   :subscribed-channels slack-subscribed-channels
   :full-and-display-names t)
  :bind
  ("C-c S" . slack-hydra/body))

(use-package gitter :demand d)


(defgroup rgr/chat-clients nil
  "Chat provides a standalone emacs instance with chat channels open"
  :group 'rgr)

(defcustom rgr/chat-functions nil
  "Start client functions"
  :group 'rgr/chat-clients
  :type '(repeat function))

(defcustom rgr/chat-close-functions nil
  "Close client functions"
  :group 'rgr/chat-clients
  :type '(repeat function))

(defun rgr/start-chats()
  (interactive)
  (dolist (fn rgr/chat-functions)
    (when (fboundp fn)
      (progn
        (message "Calling %s in start-chats" (symbol-name fn))
        (funcall fn))))
  (add-hook 'kill-emacs-hook #'rgr/close-chats))

(defun rgr/close-chats()
  (dolist (fn rgr/chat-close-functions)
    (when (fboundp fn)
      (progn
        (message "Calling %s in close-chats" (symbol-name fn))
        (funcall fn)))))

(recentf-mode -1)
(save-place-mode -1)
(savehist-mode -1)

(global-unset-key (kbd "C-c i"))
(global-set-key (kbd "C-c i") 'rgr/start-chats)

(when(yes-or-no-p "start chats?")
  (rgr/start-chats))

(provide 'rgr-chat)
