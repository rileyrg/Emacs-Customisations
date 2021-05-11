;; start emacs-server if not running
(unless(daemonp)
  (add-hook 'after-init-hook (lambda ()
                               (require 'server)
                               (unless (server-running-p)
                                 (message "Starting EmacsServer from init as not already running.")
                                 (server-start))
                               )))

(defun startHook()
  )

(add-hook 'emacs-startup-hook 'startHook)

(defun quit-or-close-emacs(&optional kill)
  (interactive)
  (if (or current-prefix-arg kill)
      (server-shutdown)
    (delete-frame)))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(global-set-key (kbd "C-c x") 'quit-or-close-emacs)
(global-set-key (kbd "C-x C-c") 'nil)

(use-package alert
  :init
  (alert "Emacs is starting..."))

(provide 'rgr/daemon)
