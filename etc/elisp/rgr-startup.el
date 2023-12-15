(use-package emacs

  :bind
  (("C-c x" . rgr/quit-or-close-emacs))

  :init

  ;(recentf-mode 1)
  ;(savehist-mode 1)
  ;(save-place-mode 1)
  (desktop-save-mode 1)
  (add-to-list 'after-make-frame-functions '(lambda()((desktop-save-mode 1))))

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

  )

(provide 'rgr/startup)
