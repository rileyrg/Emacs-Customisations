(global-set-key (kbd "C-c C-r") 'recompile)

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

(use-package project
  :demand
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  :init
  ;; Returns the parent directory containing a .project.el file, if any,
  ;; to override the standard project.el detection logic when needed.
  (defun zkj-project-override (dir)
    (let ((override (locate-dominating-file dir ".project")))
      (if override
          (cons 'vc override)
        nil)))
  (add-hook 'project-find-functions #'zkj-project-override))

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

(use-package ediff+
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :config
  (when (fboundp 'winnder-undo)
    (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
  :bind (:map prog-mode-map ("C-c C-d" . 'ediff-files)))

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

(defun rgr/c-mode-common-hook ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (local-set-key (kbd "M-<return>") 'c-complete-line)
  (setq-local dash-docs-docsets '("C")))
  (setq-local c-tab-always-indent 'complete)
(add-hook 'c-mode-common-hook 'rgr/c-mode-common-hook)

(defun c-complete-line()
  (interactive)
  (end-of-line)
  (delete-trailing-whitespace)
  (unless (eql ?\; (char-before (point-at-eol)))
    (progn (insert ";")))
  (newline-and-indent))
(define-key c-mode-map (kbd "M-<return>") 'c-complete-line)
(defun c-insert-previous-line()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  (insert (string-trim (current-kill 0))))
(defun c-newline-below()
  (interactive)
  (end-of-line)
  (newline-and-indent))

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
  ;;       :disabled t
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

(provide 'rgr/programming)
