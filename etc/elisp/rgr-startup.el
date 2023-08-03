(use-package emacs
  :custom
  (desktop-path '("~/.emacs.d/var/desktop"))
  (desktop-save t)
  (desktop-load-locked-desktop t)
  :init
  (defun rgr/restore-desktop()
    (when (fboundp 'alert)
      (alert (message (format "loading desktop from %s" desktop-path))))
    ;;I include the run-with-timer despite being able to get this to work without as it's a timing
    ;;issue and a little delay does no one any harm
    (run-at-time "1" nil (lambda()
                           (desktop-read)
                           (desktop-save-mode 1))))
  ;; (add-hook 'emacs-startup-hook
  ;;           (lambda()
  ;;             (if (daemonp)
  ;;                 (add-hook 'server-after-make-frame-hook 'rgr/restore-desktop)
  ;;               (rgr/restore-desktop))))
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
  (let ((alert-fade-time 5))
    (if (daemonp) (alert "Emacs is starting..." :title "Emacs"))))

(provide 'rgr/startup)
