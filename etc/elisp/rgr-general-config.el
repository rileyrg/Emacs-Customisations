(require 'iso-transl) ;; supposed to cure deadkeys when my external kbd is plugged into my thinkpad T44460.  It doesnt.
                                        ; t60
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(winner-mode 1)

(global-auto-revert-mode)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(global-visual-line-mode 1)

(setq column-number-mode t)

(delete-selection-mode 1)

(global-set-key (kbd "S-<f1>") 'describe-face)
(global-set-key (kbd "M-m") 'manual-entry)

(global-set-key (kbd "S-<f10>") #'menu-bar-open)
                                        ;          (global-set-key (kbd "<f10>") #'imenu)


(setq frame-title-format (if (member "-chat" command-line-args)  "Chat: %b" '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " — Emacs")))

(defalias 'yes-or-no-p 'y-or-n-p)

;; ;; restore desktop
(setq desktop-dirname (expand-file-name "desktop" user-emacs-directory))
;; (desktop-save-mode 1)

(setq disabled-command-function nil)

(global-hl-line-mode t)

(use-package
  browse-url-dwim)

(use-package
  all-the-icons)

;; display dir name when core name clashes
(require 'uniquify)

(add-to-list 'Info-directory-list (expand-file-name "info" user-emacs-directory)) ;; https://www.emacswiki.org/emacs/ExternalDocumentation


(global-set-key (kbd "C-c r") 'query-replace-regexp)

(use-package beacon
  :custom
  (beacon-blink-delay 1)
  (beacon-size 10)
  (beacon-color "orange" nil nil "Customized with use-package beacon")
  (beacon-blink-when-point-moves-horizontally 32)
  (beacon-blink-when-point-moves-vertically 8)
  :config
  (beacon-mode 1))

(straight-use-package
 '(blackout :host github :repo "raxod502/blackout"))

(use-package boxquote
  :straight (:branch "main")
  :bind
  ("C-S-r" . boxquote-region))

(use-package
  volatile-highlights
  :init (volatile-highlights-mode 1))

(use-package
  webpaste
  :bind ("C-c y" . (lambda()(interactive)(call-interactively 'webpaste-paste-region)(deactivate-mark)))
  ("C-c Y" . webpaste-paste-buffer))

(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :foundry "JB")

(use-package
  darkroom
  :bind
  ( "<f7>" . 'darkroom-mode))

(use-package xclip
  :demand t
  :config
  (xclip-mode))

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
