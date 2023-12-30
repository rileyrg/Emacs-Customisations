(global-set-key (kbd "C-c C-r") 'recompile)

(use-package indent-bars
  :disabled
  :ensure t
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook
  (prog-mode . indent-bars-mode))

(use-package duplicate-thing
  :bind
  ("C-S-d" . 'duplicate-thing))

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
  :disabled
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

;;(require 'project)

(use-package projectile
  :demand
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-x p") #'projectile-command-map)
  (define-key projectile-command-map  (kbd "b") #'consult-project-buffer))

(projectile-register-project-type 'npm '("package.json")
                                  :project-file "package.json"
       			    :compile "npm install"
       			    :test "npm test"
       			    :run "alacritty -e npm start"
       			    :test-suffix ".spec")

(use-package org-project-capture
  :demand
  :custom
  (org-project-capture-per-project-filepath "TODO.org")
  :config
  (use-package org-projectile :demand)
  (setq org-project-capture-default-backend
        (make-instance 'org-project-capture-projectile-backend))
  (org-project-capture-per-project)
  (push (org-projectile-project-todo-entry) org-capture-templates) ;; this doesnt work. I had to exec it then save in custom
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)))

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
  :init
  (use-package magit-filenotify)
  (use-package transient
    :straight (transient :type git :host github :repo "magit/transient"))
  :bind
  ("C-x g" . magit-status)
  :hook
  (magit-status-mode . magit-filenotify-mode))

(straight-use-package 'sqlite3)
(use-package forge
  :disabled
  :after magit)

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
  (defun rgr/init-dart-buffer()
    ;;(setq-local dash-docs-docsets '("Dart"))
    (lsp-deferred) )
  :hook   (dart-mode . rgr/init-dart-buffer ))

;; (use-package emacs
;;   :hook (java-mode . eglot-ensure)
;;   )

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package js
  :disabled
  :config
  (defun rgr/js-mode-hook ()
    (electric-pair-mode 1)
    (js-jsx-enable)
    (lsp-deferred)
    (local-unset-key (kbd "M-."))
    ;(setq-local dash-docs-docsets '("React" "JavaScript" "jQuery"))
    )
  :hook
  (js-ts-mode . rgr/js-mode-hook)
  :bind
  (:map js-mode-map
        ("M-." . #'lsp-ui-peek-find-definitions)))

(use-package typescript-ts-mode
  :demand t
  :init
  (defun rgr/typescript-ts-mode-hook ()
    (message "rgr/typescript-ts-mode-hook")
    (electric-pair-mode 1)
    (lsp))
  :hook
  (typescript-ts-mode .  rgr/typescript-ts-mode-hook)
  :mode (("\\.js\\'" . typescript-ts-mode)))

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
  :disabled t
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

(use-package c-ts-mode
  :config

  (defun rgr/c-complete-line()
    (interactive)
    (end-of-line)
    (delete-trailing-whitespace)
    (unless (eql ?\; (char-before (point-at-eol)))
      (progn (insert ";")))
    (newline-and-indent))

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

  (defun rgr/c-ts-mode-hook ()
    )

  (defun rgr/c-ts-mode-common-hook ()
    ;;(eglot-ensure)
    (lsp-deferred)
    ;;(if (fboundp 'indent-bars-mode)
    ;;  (indent-bars-mode))
    (if(featurep 'platformio-mode)
        (platformio-conditionally-enable))
    (if (featurep 'yasnippet)
        (yas-minor-mode)))

  :hook
  (c-mode-common . rgr/c-ts-mode-common-hook)
  (c-ts-mode . rgr/c-ts-mode-hook)
  :bind  ( :map c++-ts-mode-map
           (("M-<return>" . rgr/c-complete-line))
           :map c-ts-mode-map
           (("M-<return>" . rgr/c-complete-line))))

(defun rgr/c++-mode-hook ()
  )
(add-hook 'c++-ts-mode-hook 'rgr/cc++-mode-hook)

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
  :disabled t
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
  :disabled
  :demand t
  :config
  (defun rgr/web-mode-hook()
    ;;(setq-local dash-docs-docsets '("Twig" "CSS" "HTML"))
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
