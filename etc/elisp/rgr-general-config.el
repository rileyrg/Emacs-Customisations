(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(winner-mode 1)

(repeat-mode)

(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq auto-revert-use-notify nil)

(global-visual-line-mode 1)

(setq column-number-mode t)

(delete-selection-mode 1)

(setq frame-title-format (if (member "-chat" command-line-args)  "Chat: %b" '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " — Emacs")))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq disabled-command-function nil)

(global-hl-line-mode t)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; https://github.com/rolandwalker/browse-url-dwim
;; Context-sensitive external browse URL or Internet search from Emacs.
(use-package
  browse-url-dwim
  :config
  (browse-url-dwim-mode))

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

(use-package emacs
  :ensure nil
  :hook
  (before-save  . delete-trailing-whitespace)
  :bind
  ("C-x C-q" . view-mode)
  ( "C-c e" . rgr/erc-start)
  ( "C-x C-b" . ibuffer)
  ( "C-x C-i" . imenu)
  ( "C-x k" . rgr/kill-current-buffer)
  ( "M-0" . delete-window)
  ( "M-1" . delete-other-windows)
  ( "S-<f1>" . describe-face)
  (  "M-m"  . manual-entry)
  ( "S-<f10>" . menu-bar-open))

(use-package modus-themes
                                        ;:disabled
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  ;; (modus-themes-load-themes)
  :config
  (load-theme 'modus-operandi :no-confirm))
;; (modus-themes-load-vivendi))

(use-package ef-themes
  :disabled
  :demand t
  :config
  (ef-themes-select 'ef-duo-light))

(use-package posframe)

(use-package ace-window
  :demand t
  ;; (defalias 'other-window 'ace-window)
  :init
  (windmove-default-keybindings)
  :bind
  ("M-o" . ace-window)
  ("M-d" . ace-delete-window))

(use-package ace-link
  :demand
  :config
  (ace-link-setup-default))

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

(use-package boxquote
  ;;:straight (:branch "main")
  :bind
  ("C-S-r" . boxquote-region))

(use-package
  volatile-highlights
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
  :ensure nil
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
               ("b" . consult-buffer-other-tab)))

(use-package multiple-cursors
  :bind
  ("C-<mouse-1>" . mc/add-cursor-on-click)
  ("C-S-n" . mc/mark-next-like-this)
  ("C-S-p" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  ("C-c C-SPC" . mc/edit-lines)
  )

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind
  ("<f8>" . jinx-correct)
  ("S-<f8>" . jinx-correct-word)
  ("C-<f8>" . jinx-languages))

(provide 'rgr/general-config)
