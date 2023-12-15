(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)

  ;; (defun rgr/startup-hook (f)
  ;;   (desktop-save-mode)
  ;;   (desktop-read)
  ;;   )

  ;; (add-hook 'desktop-after-read-hook (lambda()(consult-buffer)))
  ;; (add-hook 'after-make-frame-functions #'rgr/startup-hook)
  ;; ;;(add-hook 'server-switch-hook #'rgr/startup-hook)

  (defun rgr/quit-or-close-emacs(&optional kill)
    (interactive)
    (if (or current-prefix-arg kill)
        (rgr/server-shutdown)
      (delete-frame)))

  (defun rgr/server-shutdown ()
    "Save buffers, Quit, and Shutdown (kill) server"
    (interactive)
    (clean-buffer-list)
    ;;(savehist-save)
    (save-buffers-kill-emacs))

  (global-set-key (kbd "C-c x") 'rgr/quit-or-close-emacs)

(provide 'rgr/startup)
