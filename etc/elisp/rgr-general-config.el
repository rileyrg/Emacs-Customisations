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

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))  ;; (modus-themes-load-operandi))

(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 50)))

(use-package xclip
  :demand t
  :config
  (xclip-mode))

(defun consult-buffer-other-tab ()
  "Variant of `consult-buffer' which opens in other tab."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-tab))
    (consult-buffer)))

(use-package tab-bar
  :defer t
  :custom
  (tab-bar-show t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(control))
  :custom-face
  (tab-bar ((t (:background "gray24" :foreground "#ffffff"))))
  (tab-bar-tab-inactive ((t (:background "gray24" :foreground "#ffffff"))))
  (tab-bar-tab ((t (:background "black" :foreground "#ffffff"))))
  :bind (:map tab-prefix-map
              (("x" . tab-close)
               ("b" . consult-buffer-other-tab)
               ("p" . tab-previous)
               ("n" . tab-next)
               ("c" . tab-bar-new-tab)
               ("s" . tab-bar-switch-to-tab))))

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
