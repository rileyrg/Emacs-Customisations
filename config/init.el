;; add host specific library directores to load path
(load (expand-file-name "straight-bootstrap.el" user-emacs-directory ))
;;
(push (concat user-emacs-directory "elisp") load-path )
(push (concat user-emacs-directory "elisp/hosts") load-path )

;; (use-package straight
;;   :custom
;;   (straight-use-package-by-default t)
;;   (straight-vc-git-default-protocol 'git)
;;   (straight-vc-git-force-protocol t))

(use-package org :straight org-plus-contrib)
(use-package ob-tangle :straight org-plus-contrib)

(defun init-load()
  (interactive)
  (org-babel-load-file (expand-file-name "config/config.org" user-emacs-directory))
  (setq custom-file (expand-file-name "config/custom.el" user-emacs-directory ))
  (load custom-file t)
  (load-host-customisation)
  (message "init.el successfully completed"))

(init-load)

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'init)

(put 'scroll-left 'disabled nil)
