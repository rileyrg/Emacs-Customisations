(require 'iso-transl) ;; supposed to cure deadkeys when my external kbd is plugged into my thinkpad T44460.  It doesnt.
                                        ; t60
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(winner-mode 1)
(tooltip-mode 1)
(global-auto-revert-mode)

(global-visual-line-mode 1)

(setq column-number-mode t)

(delete-selection-mode 1)

(global-set-key (kbd "S-<f1>") 'describe-face)

(global-set-key (kbd "S-<f10>") #'menu-bar-open)
                                        ;          (global-set-key (kbd "<f10>") #'imenu)


(setq frame-title-format (if (member "-chat" command-line-args)  "Chat: %b" "Emacs: %b")) ;; used to select the window again (frame-list)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; ;; restore desktop
(setq desktop-dirname (expand-file-name "desktop" user-emacs-directory))
;; (desktop-save-mode 1)

(setq disabled-command-function nil)

(global-hl-line-mode t)

(use-package boxquote
  :straight (:branch "main")
  :bind
  ("C-S-r" . boxquote-region))

(use-package
  browse-url-dwim)

(use-package
  all-the-icons)

(use-package beacon
  :custom
  (beacon-blink-delay 1)
  (beacon-size 10)
  (beacon-color "orange" nil nil "Customized with use-package beacon")
  (beacon-blink-when-point-moves-horizontally 32)
  (beacon-blink-when-point-moves-vertically 8)
  :config
  (beacon-mode 1))

(use-package
  webpaste
  :bind ("C-c y" . (lambda()(interactive)(call-interactively 'webpaste-paste-region)(deactivate-mark)))
  ("C-c Y" . webpaste-paste-buffer))

;; brings visual feedback to some operations by highlighting portions relating to the operations.
(use-package
  volatile-highlights
  :init (volatile-highlights-mode 1))
;; display dir name when core name clashes
(require 'uniquify)

(add-to-list 'Info-directory-list (expand-file-name "info" user-emacs-directory)) ;; https://www.emacswiki.org/emacs/ExternalDocumentation


(global-set-key (kbd "C-c r") 'query-replace-regexp)

;; (set-face-attribute 'default nil :height 60 :family "JetBrainsMono Nerd Font" :foundry "JB")

(use-package
  darkroom
  :bind
  ( "<f7>" . 'darkroom-mode))

(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 50)))

(use-package xclip
  :demand t
  :config
  (xclip-mode))

(use-package centaur-tabs
  :straight ( :local-repo "~/development/projects/emacs/centaur-tabs/" :fork ( :type git :host github :repo "rileyrg/centaur-tabs"))

  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-set-bar 'under)
  (x-underline-at-descent-line t)
  :config
  (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode +1)
  ;; (setq uniquify-separator "/")
  ;; (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ;; ((derived-mode-p 'dired-mode)
      ;;  "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-1" . centaur-tabs-backward)
  ("C-2" . centaur-tabs-forward)
  ("C-x t s" . centaur-tabs-switch-group)
  ("C-x t p" . centaur-tabs-group-by-projectile-project)
  ("C-x t g" . centaur-tabs-group-buffer-groups)
  )

(save-place-mode +1)

(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'kill-ring)
(add-to-list 'savehist-additional-variables 'global-mark-ring)
;; (add-hook 'kill-emacs-hook 'rgr/unpropertize-kill-ring)
;; (defun rgr/unpropertize-kill-ring ()
;; (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(use-package recentf-ext
  :config
  (recentf-mode 1)
  ;;(setq savehist-minibuffer-history-variables (remove 'file-name-history savehist-minibuffer-history-variables))
  (if (featurep 'savehist)
      (add-to-list 'savehist-ignored-variables 'file-name-history))
  (if (featurep 'no-littering)
      (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(provide 'rgr/general-config)
