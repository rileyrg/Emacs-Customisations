(recentf-mode)
(savehist-mode)
(save-place-mode)

(defun rgr/startup-hook ()
                                        ;(switch-to-buffer (get-register ?l))
  (switch-to-buffer "*scratch*")
  )

(defun rgr/remember-last-buffer (f)
  (when buffer-file-name
    (set-register ?l (buffer-name))))

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
