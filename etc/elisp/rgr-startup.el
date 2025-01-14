(setq recentf-save-file (expand-file-name "var/recentf-save.el" user-emacs-directory))
(recentf-mode)

(setq savehist-file (expand-file-name "var/savehist.el" user-emacs-directory))
(savehist-mode)

(use-package saveplace
  :ensure nil
  :custom
  (save-place-file (expand-file-name "var/save-place.el" user-emacs-directory))
  :config
  (save-place-mode))

(defun rgr/save-current-file-to-register ()
  "Save current file to register."
  ;; https://www.reddit.com/r/emacs/comments/oui4c6/using_register_to_save_current_file
  (interactive)
  (let ((reg (register-read-with-preview "File name to register: ")))
    (set-register reg `(file . ,(buffer-file-name)))))

(defun rgr/startup-hook ()
  (elpaca-wait)
  (switch-to-buffer (recentf-open-most-recent-file 1)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'rgr/startup-hook)
  (add-hook 'emacs-startup-hook 'rgr/startup-hook))

;; quitting emacs
(defun rgr/quit-or-close-emacs(&optional kill)
  (interactive)
  (if (or current-prefix-arg kill)
      (rgr/server-shutdown)
    (delete-frame)))

(defun rgr/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-buffers-kill-emacs))

(use-package emacs
  :ensure nil
  :bind ("C-c x" . rgr/quit-or-close-emacs))

(provide 'rgr/startup)
