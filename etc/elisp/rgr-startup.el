(use-package emacs
  :custom
  (desktop-path '("~/.emacs.d/var/desktop"))
  (desktop-save t)
  (desktop-load-locked-desktop t)
  (desktop-restore-frameset nil)
  (desktop-restore-eager  10)
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

  (use-package saveplace
    :config
    (save-place-mode t))

  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'global-mark-ring)
  (add-to-list 'savehist-ignored-variables 'file-name-history)

  (recentf-mode 1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude "~/.pub-cache")

  (desktop-save-mode -1) ;; TODO - doesnt work with lsp-deferred

  (use-package psession
    :disabled
    :init
    (psession-mode 1)
    (psession-autosave-mode 1)
    (psession-savehist-mode 1))

  (global-set-key (kbd "C-c x") 'rgr/quit-or-close-emacs)
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
