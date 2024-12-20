(recentf-mode)
(savehist-mode)
(save-place-mode)
;;(desktop-save-mode t)
;;(midnight-mode t)


(defun rgr/save-current-file-to-register ()
  "Save current file to register."
  ;; https://www.reddit.com/r/emacs/comments/oui4c6/using_register_to_save_current_file
  (interactive)
  (let ((reg (register-read-with-preview "File name to register: ")))
    (set-register reg `(file . ,(buffer-file-name)))))

(defun rgr/startup-hook ()
  (when (featurep 'recentf)
    (recentf-open-most-recent-file 1)))


(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'rgr/startup-hook)
  (add-hook 'after-init-hook #'rgr/startup-hook))

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
