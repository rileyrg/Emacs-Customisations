(use-package emacs
  :custom
  (desktop-path '("~/.emacs.d/var/desktop"))
  (desktop-save t)
  (desktop-load-locked-desktop t)
  :init
  (desktop-save-mode t)
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

(defun quit-or-close-emacs(&optional kill)
  (interactive)
  (if (or current-prefix-arg kill)
      (rgr/server-shutdown)
    (delete-frame)))

(defun rgr/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  ;;(when desktop-save-mode (desktop-save desktop-path))
  (save-buffers-kill-emacs))

(global-set-key (kbd "C-c x") 'quit-or-close-emacs)
(global-set-key (kbd "C-x C-c") 'nil)

(use-package alert
  :init
  (let ((alert-fade-time 5))
    (if (daemonp) (alert "Emacs is starting..." :title "Emacs"))))

(provide 'rgr/startup)
