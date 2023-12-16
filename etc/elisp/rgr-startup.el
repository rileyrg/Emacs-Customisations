(use-package emacs
  :custom
  (desktop-restore-eager  4)
  (desktop-globals-to-save
   '(search-ring global-mark-ring regexp-search-ring register-alist file-name-history))
  '(desktop-load-locked-desktop t)
  :init

  (savehist-mode 1)

  (defun my/startup-hook ()
    (switch-to-buffer (get-register ?l)))

  (defun my/remember-last-buffer (f)
    (unless
        (when buffer-file-name
          (set-register ?l (buffer-name)))))

  (add-hook 'window-buffer-change-functions #'my/remember-last-buffer)

  (add-hook 'server-after-make-frame-hook #'my/startup-hook)

  (desktop-save-mode))


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
