(use-package emacs
  :init
  (require 'iso-transl) ;; supposed to cure deadkeys when my external kbd is plugged into my thinkpad T44460.  It doesnt.
                                        ; t60
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (show-paren-mode 1)
  (winner-mode 1)

  (use-package repeat
    ;;When Repeat mode is enabled, certain commands bound to multi-key
    ;;sequences can be repeated by typing a single key, after typing the
    ;;full key sequence once.
    :config
    (repeat-mode))

  (global-auto-revert-mode 1)
  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  (global-visual-line-mode 1)

  (setq column-number-mode t)

  (delete-selection-mode 1)

  (setq frame-title-format (if (member "-chat" command-line-args)  "Chat: %b" '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " — Emacs")))

  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq disabled-command-function nil)

  (global-hl-line-mode t)

  ;; https://github.com/rolandwalker/browse-url-dwim
  ;; Context-sensitive external browse URL or Internet search from Emacs.
  (use-package
    browse-url-dwim
    :config
    (browse-url-dwim-mode))

  (use-package alert)

  ;; display dir name when core name clashes
  (require 'uniquify)

  (defun rgr/kill-current-buffer()
    (interactive)
    (if (member (buffer-name) '("*Messages*" "*scratch*"))
        (progn
          (message "Can't delete %s. Are you mad? Closing window instead." (buffer-name))
          (delete-window))
      (kill-current-buffer)
      (delete-window)))

  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :bind
  ("C-x C-q" . view-mode)
  ("C-c e" . rgr/erc-start)
  ("C-x C-b" . ibuffer)
  ("C-x C-i" . imenu)
  ("C-x k" . rgr/kill-current-buffer)
  ("M-0" . delete-window)
  ("M-1" . delete-other-windows)
  ("S-<f1>" . describe-face)
  ( "M-m"  . manual-entry)
  ("S-<f10>" . menu-bar-open))

(use-package posframe)

(use-package ace-window
  ;; :init
  ;; (defalias 'other-window 'ace-window)
  :bind
  ("M-o" . other-window)
  ("C-x o" . ace-window)
  ("M-S o" . ace-delete-window))

(use-package ace-link
  :demand
  :config
  (ace-link-setup-default)
  :bind*
  (:map emacs-lisp-mode-map
        ("C-c o" . ace-link-addr))
  ("C-c o" . ace-link)
  )

(use-package ace-jump-mode
  :bind
  ("M-s c" . ace-jump-mode)
  )

(use-package
  golden-ratio
  :init
  (golden-ratio-mode 1))

(use-package beacon
  :disabled t
  :custom
  (beacon-blink-delay 1)
  (beacon-size 10)
  (beacon-color "orange" nil nil "Customized with use-package beacon")
  (beacon-blink-when-point-moves-horizontally 32)
  (beacon-blink-when-point-moves-vertically 8)
  :config
  (beacon-mode 1))

(use-package pulsar
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.2)
  (pulsar-iterations 15)
  (pulsar-face 'isearch)
  (pulsar-highlight-face 'pulsar-green)
  :init
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-middle)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  :config
  (
   pulsar-global-mode 1))

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

(use-package bookmark+
  :demand t
  :bind
  ("C-x x <right>" . bmkp-next-bookmark)
  ("C-x x <left>" . bmkp-previous-bookmark))

(use-package emojify
  :init
  (global-emojify-mode))

(defun centreCursorLineOn()
  "set properties to keep current line approx at centre of screen height. Useful for debugging."
  ;; a faster more concise alternative to MELPA's centered-cursor-mode
  (interactive)
  (setq  scroll-preserve-screen-position_t scroll-preserve-screen-position scroll-conservatively_t
         scroll-conservatively maximum-scroll-margin_t maximum-scroll-margin scroll-margin_t
         scroll-margin)
  (setq scroll-preserve-screen-position t scroll-conservatively 0 maximum-scroll-margin 0.5
        scroll-margin 99999))

(defun centreCursorLineOff()
  (interactive)
  (setq  scroll-preserve-screen-position scroll-preserve-screen-position_t scroll-conservatively
         scroll-conservatively_t maximum-scroll-margin maximum-scroll-margin_t scroll-margin
         scroll-margin_t))

(use-package multiple-cursors
  :bind (("C-<mouse-1>" . mc/add-cursor-on-click)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)
         ))

(use-package hideshow
  :config
  (defun toggle-selective-display (column)
    (interactive "P")
    (set-selective-display
     (or column
         (unless selective-display
           (1+ (current-column))))))
  (defun toggle-hiding (column)
    (interactive "P")
    (if hs-minor-mode
        (if (condition-case nil
                (hs-toggle-hiding)
              (error t))
            (hs-show-all))
      (toggle-selective-display column)))
  (add-hook 'prog-mode-hook (lambda()(hs-minor-mode t)))
  :bind ( "C-+" . toggle-hiding)
  ("C-\\" . toggle-selective-display))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("<f8>" . jinx-correct)
         ("C-<f8>" . jinx-languages)))

(use-package
  ripgrep)

(provide 'rgr/general-config)
