(use-package lsp-mode
  ;;:disabled
  :init
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024))
  (use-package lsp-ui
    ;;:disabled t
    :init
    (use-package dap-mode
      ;;:disabled t
      :bind (:map dap-mode-map
                  (("<f8>" . dap-next)
                   ("S-<f8>" . dap-continue)
                   ("<f7>" . dap-step-in)
                   ("S-<f7>" . dap-step-out)
                   ("M-<f8>" . dap-debug)
                   ("C-<f8>" . dap-disconnect)
                   ))
      :init
      (require 'dap-cpptools)
      (dap-cpptools-setup))
    (defun rgr/lsp-ui-mode-hook()
      (message "rgr/lsp-ui-mode-hook")
      ;;(dap-mode t)
      (lsp-ui-doc-mode -1)
      (when buffer-file-name
        (setq-local buffer-save-without-query t))
      ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
      )
    :custom
    (lsp-ui-doc-mode 1)
    :bind (:map lsp-ui-mode-map
                ("C-h ." . lsp-ui-doc-focus-frame)
                ("C-h d" . lsp-ui-doc-mode)
                ("C-h f" . lsp-ui-doc-glance)
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
