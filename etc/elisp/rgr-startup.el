(use-package emacs
  :custom
  (desktop-path '("~/.emacs.d/var/desktop"))
  (desktop-save t)
  (desktop-load-locked-desktop t)
  (desktop-restore-frameset nil)
  (desktop-restore-eager  10)
  :config
  (defun quit-or-close-emacs(&optional kill)
    (interactive)
    (if (or current-prefix-arg kill)
        (rgr/server-shutdown)
      (delete-frame)))

  (defun rgr/server-shutdown ()
    "Save buffers, Quit, and Shutdown (kill) server"
    (interactive)
    (clean-buffer-list)
    (savehist-save)
    (save-buffers-kill-emacs))

  (save-place-mode 1)

  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'global-mark-ring)
  (add-to-list 'savehist-ignored-variables 'file-name-history)

  (recentf-mode 1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude "~/.pub-cache")

  (desktop-save-mode t)

  (global-set-key (kbd "C-c x") 'quit-or-close-emacs)
  )

;; start emacs-server if not running
(unless(daemonp)
  (add-hook 'after-init-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (message "Starting EmacsServer from init as not already running.")
                (server-start))
              )))

(use-package alert
  :init
  (let ((alert-fade-time 5))
    (if (daemonp) (alert "Emacs is starting..." :title "Emacs"))))

(provide 'rgr/startup)
