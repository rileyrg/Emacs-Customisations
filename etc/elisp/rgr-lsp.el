(use-package lsp-mode
  ;;:disabled
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024))
  (use-package lsp-ui
    :init
    (use-package dap-mode
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
      (dap-mode t)
      )
    :custom
    (lsp-ui-doc-mode 1)
    :bind (:map lsp-ui-mode-map
                ("M-." . #'lsp-find-definition))
    :hook
    (lsp-ui-mode . rgr/lsp-ui-mode-hook))
  (use-package lsp-treemacs
    :custom
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
