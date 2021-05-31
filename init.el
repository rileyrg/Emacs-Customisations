(setq custom-file  (expand-file-name  "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(eval-and-compile
  (require 'package)
  (setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  ;; i always fetch the archive contents on startup and during compilation, which is slow
  ;;(package-refresh-contents)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  ;; i don't really know why this isn't the default...
  (setf use-package-always-ensure t))

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(defvar elisp-dir (expand-file-name "elisp" no-littering-etc-directory) "my elisp directory. directories are recursively added to path.")
(add-to-list 'load-path elisp-dir)
(let ((default-directory elisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

(require 'rgr/early-init "rgr-early-init")

(require 'rgr/security "rgr-security" 'NOERROR)

(require 'rgr/utils "rgr-utils" 'NOERROR)

(require 'rgr/daemon "rgr-daemon" 'NOERROR)

(require 'rgr/minibuffer "rgr-minibuffer" 'NOERROR)

(require 'rgr/completion "rgr-completion" 'NOERROR)

(require  'rgr/general-config "rgr-general-config" 'NOERROR)

(require 'rgr/org "rgr-org" 'NOERROR)

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

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

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

(use-package flyspell
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word)
    )

  :bind (("C-<f8>" . flyspell-mode)
         ("S-<f8>" . flyspell-check-previous-highlighted-word)
         ("C-S-<f8>" . flyspell-buffer)
         ("M-<f8>" . flyspell-word)
         ))

(use-package
  ripgrep)

(use-package
  ag)

(use-package deft
  :config
  (setq deft-directory (expand-file-name "orgfiles" user-emacs-directory))
  (setq deft-recursive t)
  :bind(("M-<f3>" . #'deft)))

(require 'rgr/reference "rgr-reference" 'NOERROR)

(require 'rgr/emms "rgr-emms" 'NOERROR)

(defun eshell/emacs-clean (&rest args)
  "run a clean emacs"
  (interactive)
  (message "args are %s" args)
  (save-window-excursion
    (shell-command "emacs -Q -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el &")))

(defun eshell/_ftrace_fn (&rest args)
  "useage: _ftrace_fn &optional function-name(def:printf)  depth(def:1)
creates a report in function-name.ftrace and opens it in a buffer"
  (interactive)
  (let ((fn (or (nth 2 args) "printf"))
        (depth (or (nth 3 args) 1)))
    (shell-command (format "sudo trace-cmd record -p function_graph --max-graph-depth %s -e syscalls -F %s && trace-cmd report | tee %s.ftrace" depth fn fn))
    (switch-to-buffer (find-file-noselect (format "%s.ftrace" fn) ))))

(use-package
  eshell
  :init
  (require 'em-hist)
  (require 'em-tramp)
  (require 'em-smart)
  :config
  (defun eshell-mode-hook-func ()
    ;; (setq eshell-path-env (concat "/home/rgr/bin:" eshell-path-env))
    ;; (setenv "PATH" (concat "/home/rgr/bin:" (getenv "PATH")))
    (setq pcomplete-cycle-completions nil))
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)

  (use-package
    eshell-git-prompt
    :config
    (eshell-git-prompt-use-theme 'powerline)
    (define-advice
        eshell-git-prompt-powerline-dir
        (:override ()
                   short)
      "Show only last directory."
      (file-name-nondirectory (directory-file-name default-directory)))))

(use-package vterm
  :custom (vterm-shell "/usr/bin/zsh")
  :bind
  ("M-g v" . vterm))

(use-package docker)

(defun rgr/toggle-buffer(n)
  "jump to or from buffer named n else default to *Messages*"
  (interactive "bbuffer:")
  (let ((n (or n
               "*Messages*")))
    (switch-to-buffer (if (string= (buffer-name) n)
                          (other-buffer) n))))

(use-package emacs
  :demand
  :config
  (defun rgr/kill-current-buffer()
    (interactive)
    (if (member (buffer-name) '("*Messages*" "*scratch*"))
        (progn
          (message "Can't delete %s. Are you mad? Closing window instead." (buffer-name))
          (delete-window))
      (kill-current-buffer)))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :bind
  ("C-x k" . rgr/kill-current-buffer)
  ("M-0" . 'delete-window)
  ("M-1" . 'delete-other-windows))

(use-package dired-git
  :config
  :hook (dired-mode . dired-git-mode))

(use-package dired-subtree
  :config
  (use-package dash)
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package dired-filter
  :config
  (use-package dash)
  )

(use-package dired-quick-sort
  :disabled t
  :config
  (dired-quick-sort-setup))

(use-package posframe)

(use-package popper
  :ensure t
  :init
  (use-package posframe)
  ;;(setq popper-display-function 'rgr/popper-display-posframe)
  (setq popper-reference-buffers
        '(
          "\\*Messages\\*"
          magit-mode
          ;;      help-mode
          helpful-mode
          inferior-python-mode
          dictionary-mode
          compilation-mode))
  (popper-mode +1)
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)))

(use-package transpose-frame
  :disabled t
  :config
  (defun window-split-toggle ()
    "Toggle between horizontal and vertical split with two windows."
    (interactive)
    (if (> (length (window-list)) 2)
        (error "Can't toggle with more than 2 windows!")
      (let ((func (if (window-full-height-p)
                      #'split-window-vertically
                    #'split-window-horizontally)))
        (delete-other-windows)
        (funcall func)
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))))
  :bind
  ("C-M-t" . transpose-frame)
  ("C-c T" . window-split-toggle)
  )

(use-package
  hyperbole
  :disabled t)

(use-package undohist
  :disabled t
  :config
  (undohist-initialize))

(use-package undo-tree
  :disabled t
  :config
  (global-undo-tree-mode))

(use-package undo-fu
  :disabled t
  :init
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package back-button
  :disabled t
  :config
  (back-button-mode 1)
  :bind
  ("M-<left>" . previous-buffer)
  ("M-<right>" . next-buffer))

(use-package ace-window
  :init
  (defalias 'other-window 'ace-window)
  :bind*
  ("M-o" . ace-window)
  ("M-S o" . ace-delete-window))

(use-package ace-link
  :demand t
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

(defun htop-regexp()
  (interactive)
  (let ((s (completing-read (format "HTtop filter (%s): " (symbol-at-point)) minibuffer-history nil nil (symbol-at-point))))
    (condition-case nil
        (shell-command (format "htop-regexp %s" s))
      (error nil))))
(global-set-key (kbd "C-S-p") 'htop-regexp)

(use-package
  treemacs
  :custom
  (treemacs-follow-after-init t)
  :config
  (treemacs-follow-mode +1)
  (treemacs-fringe-indicator-mode)
  (treemacs-git-mode 'deferred)
  (use-package treemacs-icons-dired
    :config (treemacs-icons-dired-mode))
  (use-package treemacs-magit)
  :bind
  ("M-9"   . 'treemacs-select-window)
  (:map treemacs-mode-map
        ("<right>" . treemacs-peek)))

(defun rgr/load-chats(switch)
  (require  'rgr-chat))
(add-to-list 'command-switch-alist '("chat" . rgr/load-chats))

(defun eshell/chat-client
    (&rest
     args)
  "chat with emacs.."
  (interactive)
  (save-window-excursion (call-process "oneemacs-chat" nil 0)))

(global-set-key (kbd "C-c i") 'eshell/chat-client)

(use-package keycast
  )

(require 'rgr/elisp-utils (expand-file-name "rgr-elisp-utils" elisp-dir))

(use-package scratch
  :bind ("<f2>" . (lambda()
                    (interactive)
                    (switch-to-buffer(scratch--create 'emacs-lisp-mode "*scratch*"))))
  ("C-<f2>" . (lambda()
                (interactive)
                (switch-to-buffer(messages-buffer)))))

(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))

(global-set-key (kbd "S-<f2>") 'linum-mode)
(add-hook 'prog-mode-hook (lambda() (linum-mode t)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(use-package project)

;; try to work with next-error for bash's "set -x" output
(use-package compile
  :config
  (add-to-list 'compilation-error-regexp-alist
               'bash-set-x)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pascal
                 "\\(.+?\\)\\(\\([0-9]+\\),\\([0-9]+\\)\\).*" 1 2 3)))

(use-package
  yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\.yaml\\'" . yaml-mode))
  )

(use-package
  flycheck
  :custom
  (flycheck-global-modes '(not org-mode org-src-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  ;;(flycheck-check-syntax-automatically '(save))
  :config (use-package
            flycheck-pos-tip
            :config
            (flycheck-pos-tip-mode))
  (global-flycheck-mode +1)
  :bind ("<f8>" . (lambda()
                    (interactive)
                    (flycheck-mode 'toggle)
                    (let((s (if flycheck-mode "on" "off")))
                      (message "flycheck %s" s)))))

(use-package
  magit
  :custom
  (vc-handled-backends '(git))
  :config
  (add-hook 'magit-post-commit-hook 'magit-mode-bury-buffer)
  :bind
  ("C-x g" . magit-status))

(use-package orgit
  :after magit)

(use-package forge
  :after magit
  :config
  (use-package orgit-forge))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  :bind
  ("C-x v ="  . git-gutter:popup-hunk))

(use-package rjsx-mode
  :config
  (use-package npm-mode)
  (defun rgr/js2-mode-hook ()
    ;;         (setq-local zeal-at-point-docset '("JavaScript" "jQuery"))
    (npm-mode t)
    (setq-local dash-docs-docsets '("React" "JavaScript" "jQuery")))
  (add-hook 'js2-mode-hook 'rgr/js2-mode-hook)
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
  )

(use-package prettier-js
  :custom
  (prettier-js-args '(
                      "--trailing-comma" "all"
                      "--bracket-spacing" "false"
                      "--print-width" "80"
                      )))

(defun rgr/js-mode-hook ()
  (setq-local js-indent-level 2)
  (prettier-js-mode t)
  (setq-local dash-docs-docsets '("React" "JavaScript" "jQuery")))

(add-hook 'js-mode-hook 'rgr/js-mode-hook)



;;(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))

(use-package rjsx-mode
  :disabled t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
  )

(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-mode))
  (defun rgr/ts-mode-hook ()
    (setq-local dash-docs-docsets '("React" "JavaScript")))
  (add-hook 'typescript-mode-hook 'rgr/ts-mode-hook))

(require 'rgr/lsp "rgr-lsp" 'NOERROR)

(defgroup rgr/serial-ports nil
  "serial port customization"
  :group 'rgr)

(defcustom rgr/serialIOPort "/dev/ttyACM0"
  "Serial device for emacs to display"
  :type 'string
  :group 'rgr/serial-ports)

(defcustom rgr/serialIOPortBaud 9600
  "Default serial baud rate"
  :type 'integer
  :group 'rgr/serial-ports)

(defun selectSerialPortBuffer()
  (setq ser (get-buffer rgr/serialIOPort))
  (if ser (switch-to-buffer ser)
    (serial-term rgr/serialIOPort rgr/serialIOPortBaud)))

(global-set-key (kbd "C-c s")
                (lambda()
                  (interactive)
                  (selectSerialPortBuffer)))

(use-package platformio-mode)

(use-package  python
  :config
  (defun rgr/python-shell-send-buffer(orig-func &rest args)
    "create a python shell if there isnt one"
    (interactive)
    (save-selected-window
      (save-excursion(run-python))
      (apply orig-func current-prefix-arg)
      (unless (get-buffer-window (python-shell-get-buffer))
        (switch-to-buffer-other-window (python-shell-get-buffer)))))
  (advice-add 'python-shell-send-buffer :around #'rgr/python-shell-send-buffer))

(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(use-package lsp-python-ms
  :custom
  (lsp-python-ms-auto-install-server t)
  (lsp-python-ms-parse-dot-env-enabled t)
  :config
  (require 'lsp-python-ms)
  :hook (python-mode . (lambda ()
                         (lsp-deferred))))

(use-package auto-virtualenv
  :config
  (add-hook 'python-mode-hook  #'auto-virtualenv-set-virtualenv))

(use-package blacken
  :demand t
  :config
  (add-hook 'python-mode-hook  #'blacken-mode))

(use-package clang-format)
(setq clang-format-style-option "llvm")
(fset 'c-indent-region 'clang-format-region)
(fset 'c-indent-buffer 'clang-format-buffer)

(defun rgr/c++-mode-hook ()
  (setq-local dash-docs-docsets '("C++")))
(add-hook 'c++-mode-hook 'rgr/c++-mode-hook)

(use-package logview
  :demand t
  :init
  (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
  (add-to-list 'auto-mode-alist '("log\\'" . logview-mode)))

(use-package strace-mode)

(defgroup rgr/symfony nil
  "Symfony Development"
  :group 'rgr)

(defcustom symfony-server-command "~/.symfony/bin/symfony server:start"
  "Start the symfony web server"
  :type 'string
  :group 'rgr/symfony)

(use-package php-mode
  :custom
  (lsp-intelephense-licence-key (get-auth-info "licenses" "intelephense"))
  :config
  (add-to-list 'display-buffer-alist
               (cons "\\*Symfony Web Server\\*.*" (cons #'display-buffer-no-window nil)))
  (defun start-symfony-web-server()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (if (and default-directory (file-exists-p "bin/console") (eq (length (shell-command-to-string "pgrep symfony")) 0) (yes-or-no-p "Start web server?"))
          (async-shell-command symfony-server-command "*Symfony Web Server*"))))
  (defun php-mode-webserver-hook ()
    (interactive)
    (start-symfony-web-server)
    ))

(use-package
  web-mode
  ;;:disabled t
  :demand t
  :config
  (defun rgr/web-mode-hook()
    (setq-local dash-docs-docsets '("Twig" "CSS" "HTML"))
    )
  (add-hook 'web-mode-hook 'rgr/web-mode-hook)
  ;; (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(use-package elf-mode
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(?:a\\|so\\)\\'" . elf-mode)))

(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))  ;; (modus-themes-load-operandi))

(load-el-gpg (no-littering-expand-etc-file-name "late-load"))
