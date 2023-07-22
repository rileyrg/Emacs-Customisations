(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :config
  (use-package lsp-ui :commands lsp-ui-mode)
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
  (lsp-treemacs-sync-mode 1)
  (use-package flycheck)
  (use-package dap-mode)
  (use-package lsp-dart)
  (setq lsp-completion-provider :capf)
  ;; (defun corfu-lsp-setup ()
  ;;   (setq-local completion-styles '(orderless)
  ;;               completion-category-defaults nil))
  ;; (add-hook 'lsp-completion-mode-hook #'corfu-lsp-setup)

  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package eglot
  :straight `(eglot ,@(when (>= emacs-major-version 29) '(:type built-in)))
  :config
  (use-package eldoc-box)
  :hook
  (prog-mode . eldoc-box-hover-at-point-mode)
  :bind
  (:map flymake-mode-map
        ([remap next-error] . flymake-goto-next-error)
        ([remap previous-error] . flymake-goto-prev-error))
  (:map eglot-mode-map
        ("<C-return>" . eglot-code-actions)))

(provide 'rgr/lsp)
