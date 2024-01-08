(recentf-mode)
(savehist-mode) ;; (el-docstring-sap--history projectile-project-command-history global-mark-ring kill-ring search-ring regexp-search-ring register-alist)
(save-place-mode)

(use-package better-registers
  :demand t
  :custom
  (better-registers-save-file (no-littering-expand-var-file-name "better-registers.el"))
  :config
  (better-registers-install-save-registers-hook)
  (load better-registers-save-file))

(defun rgr/startup-hook ()
  (switch-to-buffer "*scratch*")
  (switch-to-buffer (get-register ?L))
  )

(defun rgr/remember-last-buffer (f)
  (when buffer-file-name
    (set-register ?L (buffer-name))))

(add-hook 'window-buffer-change-functions #'rgr/remember-last-buffer)

(add-hook 'server-after-make-frame-hook #'rgr/startup-hook)

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
