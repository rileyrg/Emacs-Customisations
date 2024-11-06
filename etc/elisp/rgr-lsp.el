(use-package lsp-mode
  ;;:disabled
  :init
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024))
  (use-package lsp-ui
    ;;:disabled t
    :init
    (defun rgr/lsp-ui-mode-hook()
      (message "rgr/lsp-ui-mode-hook")
      ;;(dap-mode t)
      (lsp-ui-doc-mode -1)
      (when buffer-file-name
        (setq-local buffer-save-without-query t))
      ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
      )
    (use-package dape
      :preface
      ;; By default dape shares the same keybinding prefix as `gud'
      ;; If you do not want to use any prefix, set it to nil.
      (setq dape-key-prefix "\C-x\C-a")

      :hook
      ;; Save breakpoints on quit
      ((kill-emacs . dape-breakpoint-save)
       ;; Load breakpoints on startup
       (after-init . dape-breakpoint-load))

      :config
      ;; Turn on global bindings for setting breakpoints with mouse
      (dape-breakpoint-global-mode)

      ;; Info buffers to the right
      ;; (setq dape-buffer-window-arrangement 'right)

      ;; Info buffers like gud (gdb-mi)
      (setq dape-buffer-window-arrangement 'gud)
      (setq dape-info-hide-mode-line nil)

      ;; Pulse source line (performance hit)
      ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

      ;; Showing inlay hints
      (setq dape-inlay-hints t)

      ;; Save buffers on startup, useful for interpreted languages
      ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

      ;; Kill compile buffer on build success
      (add-hook 'dape-compile-hook 'kill-buffer)

      ;; Projectile users
      (setq dape-cwd-fn 'projectile-project-root)
      )

    :custom
    (lsp-ui-doc-mode 1)
    :bind (:map lsp-ui-mode-map
                ("C-h ." . lsp-ui-doc-focus-frame)
                ("C-h d" . lsp-ui-doc-mode)
                ("C-h g" . lsp-ui-doc-glance)
                ("M-." . lsp-find-definition)
                ("C-x C-i" . lsp-ui-imenu))
    :hook
    (lsp-ui-mode . rgr/lsp-ui-mode-hook))
  (use-package lsp-treemacs
    :config
    (lsp-treemacs-sync-mode t)
    :commands lsp-treemacs-errors-list)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))



(use-package eglot
  :disabled
  ;;:straight `(eglot ,@(when (>= emacs-major-version 29) '(:type built-in)))
  ;; :config
  ;; (use-package eldoc-box)
  ;; :hook
  ;; (prog-mode . eldoc-box-hover-at-point-mode)
  :bind
  (:map eglot-mode-map
        ("<C-return>" . eglot-code-actions)))

(provide 'rgr/lsp)
