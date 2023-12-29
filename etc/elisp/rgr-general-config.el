(require 'iso-transl) ;; supposed to cure deadkeys when my external kbd is plugged into my thinkpad T44460.  It doesnt.
                                        ; t60
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(winner-mode 0)

(global-auto-revert-mode 1)
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

(setq disabled-command-function nil)

(global-hl-line-mode t)

(use-package
  browse-url-dwim)

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
  :disabled
  :init (volatile-highlights-mode 1))

(use-package
  dpaste
  :init
  ;; Choosing githup gist only
  (setq webpaste-provider-priority '("gist.github.com"))
  ;; You can always append this list as much as you like, and which providers
  ;; that exists is documented below in the readme.
  :bind ("C-c y" . dpaste-region-or-buffer))

;;(set-frame-font "-JB-JetBrainsMono Nerd Font-regular-normal-normal-*-14-*-*-*-*-0-fontset-auto1" nil t)

(use-package
  darkroom
  :bind
  ( "<C-f7>" . 'darkroom-mode))

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

(provide 'rgr/general-config)
