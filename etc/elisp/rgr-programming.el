(global-set-key (kbd "C-c C-r") 'recompile)

(use-package indent-bars
  ;;:disabled
  :ensure t
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook
  (prog-mode . indent-bars-mode))

(use-package emacs
  :bind
  ("C-S-d" . 'duplicate-line))

(use-package breadcrumb
  :straight (breadcrumb :local-repo "~/development/projects/emacs/breadcrumb"))

(use-package rmsbolt
  :config
  (defun rgr/rmsbolt-toggle()
    (interactive)
    (if rmsbolt-mode
        (progn
          (when (get-buffer
                 rmsbolt-output-buffer)
            (with-current-buffer rmsbolt-output-buffer
              (kill-buffer-and-window)))
          (rmsbolt-mode -1))
      (progn
        (rmsbolt-mode +1)
        (rmsbolt-compile))))
  :bind
  (:map prog-mode-map
        ("C-c d" . rgr/rmsbolt-toggle)))

(defun my/parrot-animate-when-compile-success (buffer result)
  (if (string-match "^finished" result)
      (parrot-start-animation)))

(use-package parrot
  :ensure t
  :config
  (parrot-mode)
  (add-to-list 'compilation-finish-functions 'my/parrot-animate-when-compile-success))

(require 'rgr/elisp-utils (expand-file-name "rgr-elisp-utils" elisp-dir))

(use-package scratch
  :disabled
  :bind ("<f2>" . (lambda()
                    (interactive)
                    (switch-to-buffer(scratch--create 'emacs-lisp-mode "*scratch*"))))
  ("C-<f2>" . (lambda()
                (interactive)
                (switch-to-buffer(messages-buffer)))))

(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))

(global-set-key (kbd "S-<f2>") 'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda() (display-line-numbers-mode t)))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package rainbow-identifiers
  :disabled
  :config
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(require 'project)

(use-package projectile
  :demand
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(use-package org-projectile
  ;;:disabled
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODOs.org")
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)q))
  (push (org-projectile-project-todo-entry) org-capture-templates))

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

(use-package json-reformat)
(use-package hydra)

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (defun rgr/sh-mode-hook()
    (flymake-shellcheck-load)
    (flymake-mode +1))
  :hook (sh-mode . rgr/sh-mode-hook))

(use-package
  magit
  :custom
  (vc-handled-backends '(git))
  ;; :config
  ;; (add-hook 'magit-post-commit-hook 'magit-mode-bury-buffer)
  ;; (require 'magit-extras)
  :bind
  ("C-x g" . magit-status))

(use-package orgit
  :after magit)

(use-package ediff+
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :config
  (when (fboundp 'winnder-undo)
    (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
  :bind (:map prog-mode-map ("C-c C-d" . 'ediff-files)))

(use-package forge
  :disabled
  :after magit
  ;; :init
  ;; (use-package emacsql-sqlite-builtin)
  ;; (setq forge-database-connector 'sqlite-builtin)
  :config
  (use-package orgit-forge)
  )

(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  :bind
  ("C-x v ="  . git-gutter:popup-hunk))

(use-package dart-mode
  :custom
  (lsp-dart-flutter-widget-guides t)
  :init
  (use-package flutter
    :after dart-mode
    :custom
    (flutter-sdk-path "~/bin/thirdparty/flutter")
    :config
    (use-package flutter-l10n-flycheck)
    (setenv "JAVA_HOME" (concat (getenv "ANDROID_STUDIO_HOME") "/jbr"))
    :bind (:map dart-mode-map
                ("C-M-x" . (lambda()
                             (interactive)
                             (save-buffer)
                             (flutter-run-or-hot-reload))))
    :hook   (dart-mode . (lambda()
                           (flutter-test-mode))))
  :config
  (add-to-list 'devdocs-browser-major-mode-docs-alist '(dart-mode "dart"))
  (use-package lsp-dart :after lsp)
  :hook   (dart-mode . (lambda()
                         (setq-local dash-docs-docsets '("Dart"))
                         ;;(eglot-ensure)
                         (lsp-deferred)
                         )))

;; (use-package emacs
;;   :hook (java-mode . eglot-ensure)
;;   )

(use-package js
  :config
  (defun rgr/js-mode-hook ()
    ;;(eglot-ensure)
    (local-unset-key (kbd "M-."))
    (setq-local dash-docs-docsets '("React" "JavaScript" "jQuery")))
  :hook
  (js-mode . rgr/js-mode-hook)
  :bind
  (:map js-mode-map
        ("M-." . #'lsp-ui-peek-find-definitions)))

(use-package treesit-auto
  ;;:disable
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; (message "**** treesit dart ***")
  ;; (setq my-dart-tsauto-config
  ;;       (make-treesit-auto-recipe
  ;;        :lang 'dart
  ;;        :ts-mode 'dart-mode
  ;;        :remap 'dart-mode
  ;;        :url "https://github.com/UserNobody14/tree-sitter-dart"))
  ;; (add-to-list 'treesit-auto-recipe-list my-dart-tsauto-config)

  (use-package treesitter-context
    :straight (:host github :type git :repo "zbelial/treesitter-context.el" )
    :init
    (use-package posframe-plus
      :straight (:host github :type git :repo "zbelial/posframe-plus" ))
    :config
    (treesitter-context-mode t))

  (global-treesit-auto-mode)
  :hook
  ;;(dart-mode . (lambda()(treesit-inspect-mode())))
  (c-ts-base-mode . (lambda()
                      (treesit-inspect-mode t)
                      (rgr/c-mode-common-hook))))

;; sudo npm i -g typescript-language-server
(use-package typescript-mode
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  (defun rgr/ts-mode-hook ()
    ;;(eglot-ensure)
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

(use-package platformio-mode
  :demand t
  :custom

  (platformio-mode-silent nil)
  :init
  (require 'ansi-color)
  (defun rgr/platformio-compilation-mode-filter (buf _)
    (interactive)
    (with-current-buffer buf
      (when (derived-mode-p 'platformio-compilation-mode)
        (let ((inhibit-read-only t))
          (ansi-color-apply-on-region (point-min) (point-max))))))

  (add-hook 'compilation-finish-functions
            'rgr/platformio-compilation-mode-filter))

(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(use-package auto-virtualenv
  :config
  (add-hook 'python-mode-hook  #'auto-virtualenv-set-virtualenv))

;; I'm typically confused when it comes to haskell. Note that the interactive stuff I cribbed doesnt work.
(use-package haskell-mode
  :config
  ;;(add-hook 'haskell-mode-hook #'eglot-ensure)
  ;;(add-hook 'haskell-literate-mode-hook #'eglot-ensure)
  (eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
  (eval-after-load "haskell-cabal"
              '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package lldb-voltron
  :straight (lldb-voltron :local-repo "~/development/projects/emacs/emacs-lldb-voltron" :type git :host github :repo "rileyrg/emacs-lldb-voltron" )
  ;;:config
  ;; (breadcrumb-mode t)
  )

(use-package emacs
  :config
  (defun rgr/c-mode-common-save-hook()
                                        ;(eglot-format-buffer)
    )
  (defun rgr/c-mode-common-hook ()
    (add-hook 'before-save-hook #'rgr/c-mode-common-save-hook nil t)
    ;;(eglot-ensure)
    (lsp-deferred)
    ;;(if (fboundp 'indent-bars-mode)
      ;;  (indent-bars-mode))
    (if(featurep 'platformio-mode)
        (platformio-conditionally-enable))
    (if (featurep 'yasnippet)
        (yas-minor-mode)))
  :hook
  (c-mode-common . rgr/c-mode-common-hook)
  :bind  ( :map c-mode-base-map
           (("M-<return>" . rgr/c-complete-line)
            ("TAB" . rgr/c-indent-complete)
            )))

(defun rgr/c-mode-hook ()
  (setq-local dash-docs-docsets '("C")))
(add-hook 'c-mode-hook 'rgr/c-mode-hook)

(defun rgr/c-complete-line()
  (interactive)
  (end-of-line)
  (delete-trailing-whitespace)
  (unless (eql ?\; (char-before (point-at-eol)))
    (progn (insert ";")))
  (newline-and-indent))
;;(define-key c-mode-map (kbd "M-<return>") 'rgr/c-complete-line)
(defun rgr/c-insert-previous-line()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  (insert (string-trim (current-kill 0))))
(defun rgr/c-newline-below()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun rgr/c-indent-complete()
  (interactive)
  (let (( p (point)))
    (c-indent-line-or-region)
    (when (= p (point))
      (call-interactively 'complete-symbol))))

(defun rgr/c++-mode-hook ()
  (setq-local dash-docs-docsets '("C++")))
(add-hook 'c++-mode-hook 'rgr/c++-mode-hook)

(use-package logview
  :demand t
  :init
  (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
  (add-to-list 'auto-mode-alist '("log\\'" . logview-mode)))

(use-package strace-mode)

(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "rileyrg/emacs-gdscript-mode")
  :init
  (defun franco/godot-gdscript-lsp-ignore-error (original-function &rest args)
    "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
    (if (string-equal major-mode "gdscript-mode")
        (let ((json-data (nth 0 args)))
          (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                   (not (gethash "id" json-data nil))
                   (not (gethash "method" json-data nil)))
              nil ; (message "Method not found")
            (apply original-function args)))
      (apply original-function args)))
  (advice-add #'lsp--get-message-type :around #'franco/godot-gdscript-lsp-ignore-error)
  )

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
  (add-to-list 'magic-mode-alist '("\dELF" . elf-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(?:a\\|so\\)\\'" . elf-mode)))

(provide 'rgr/programming)
