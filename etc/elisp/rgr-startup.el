(recentf-mode)
;; (savehist-mode) ;; (el-docstring-sap--history projectile-project-command-history global-mark-ring kill-ring search-ring regexp-search-ring register-alist)
;; (save-place-mode)

;; ;;
(defun rgr/startup-hook ()
  (setq desktop-restore-forces-onscreen nil)
  (desktop-save-mode 1)
  (midnight-mode)
  (add-hook 'desktop-save-hook 'clean-buffer-list)
  (desktop-read))

;; I use a persistent register to remember the last file buffer and to resore it on emacs daemon restart when a frame appears.
;; (let ((fname (get-register ?L)))
;;   (when (and fname (file-exists-p fname))
;;     (find-file fname))))

;; (defun rgr/remember-last-buffer (f)
;;   (when buffer-file-name
;;     (set-register ?L (buffer-file-name))))

;; (add-hook 'window-buffer-change-functions #'rgr/remember-last-buffer)

(add-hook 'server-after-make-frame-hook #'rgr/startup-hook)

(defun rgr/quit-or-close-emacs(&optional kill)
  (interactive)
  (if (or current-prefix-arg kill)
      (rgr/server-shutdown)
    (delete-frame)))

(defun rgr/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-buffers-kill-emacs))

(global-set-key (kbd "C-c x") 'rgr/quit-or-close-emacs)

(provide 'rgr/startup)
