(global-set-key (kbd "C-c C-r") 'recompile)

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

(defgroup rgr/llvm  nil
  "llvm options"
  :group 'rgr)

(defcustom rgr/lldb-command "lldb"
  "the llvm debugger command"
  :type 'string
  :group 'rgr/llvm)

(define-minor-mode rgr/lldb-mode "my lldb mode" :lighter "lldb"
  :keymap '(
            ( [f10]   . (lambda()(interactive)(process-send-string lldb-console "thread step-over\n")))
            ( [f11]   . (lambda()(interactive)(process-send-string lldb-console "thread step-in\n")))
            ( [S-f11] . (lambda()(interactive)(process-send-string lldb-console "thread step-out\n")))
            ( [f12>]  . (lambda()(interactive)(process-send-string lldb-console "thread step-inst\n"))))
  (setq-local lldb-console rgr/lldb-buffer-name))

(defun rgr/projectLLDB(dir)
  "Run a vterm with lldb for the current buffer's directory, default DIR. Launch a lldb-ui instance unless prefix arg."
  (interactive "DDirectory:")
  (let* ((dirbase (file-name-nondirectory(directory-file-name dir)))
         (lldb-ui-command (format "%s %s %s &" "lldb-ui" dir dirbase))
         (vterm-buffer-name (format "*lldb-%s*" dirbase)))
    (if (get-buffer vterm-buffer-name)
        (switch-to-buffer vterm-buffer-name)
      (progn
        (vterm)
        (process-send-string vterm-buffer-name (format "%s && tmux kill-session -t %s && exit\n" rgr/lldb-command dirbase))
        (unless current-prefix-arg
          (call-process-shell-command lldb-ui-command)
          (process-send-string vterm-buffer-name "lv\n"))
        (with-current-buffer vterm-buffer-name
          (setq rgr/lldb-buffer-name vterm-buffer-name)
          (rgr/lldb-mode))))))

(use-package projectile
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  :bind
  (:map projectile-command-map ("D" . rgr/projectLLDB))
  )

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

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

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

(use-package eglot
  :hook (python-mode . (lambda ()
                         (eglot-ensure))))

(use-package auto-virtualenv
  :config
  (add-hook 'python-mode-hook  #'auto-virtualenv-set-virtualenv))

(use-package blacken
  :demand t
  :config
  (add-hook 'python-mode-hook  #'blacken-mode))

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

(use-package emacs
  :config
  (defun rgr/c-save-hook()
    ;;    (lsp-format-buffer)
    ;;(eglot-format-buffer)
    )
  (defun rgr/c-mode-common-hook ()
    (add-hook 'before-save-hook #'rgr/c-save-hook nil t)
    (setq-local dash-docs-docsets '("C"))
    (if(featurep 'corfu)
        (setq completion-category-defaults nil))
    (if(featurep 'eglot)
        (eglot-ensure)))
  :hook
  (c-mode-common . rgr/c-mode-common-hook)
  :bind  ( :map c-mode-base-map
           (("M-<return>" . rgr/c-complete-line)
            ("TAB" . rgr/c-indent-complete))))

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
