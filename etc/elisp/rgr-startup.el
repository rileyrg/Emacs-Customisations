(use-package emacs

  :bind
  (("C-c x" . rgr/quit-or-close-emacs))

  :init
  (recentf-mode 1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude "~/.pub-cache")
  (require 'saveplace)
  (save-place-mode t)
  (require 'savehist)
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'global-mark-ring)
  (add-to-list 'savehist-ignored-variables 'file-name-history)
  (savehist-mode 1)

  :config

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
