(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  (setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))
  :config
  (use-package lsp-ui :commands lsp-ui-mode)

  (use-package lsp-treemacs
    :custom
    (lsp-treemacs-sync-mode t)
    :commands lsp-treemacs-errors-list)

  (use-package flycheck)
  (use-package dap-mode
        :bind (:map dap-mode-map
                (("<f8>" . dap-next)
                 ("<f7>" . dap-step-in)
                 ("S-<f8>" . dap-continue)
                 ("M-<f8>" . dap-debug)
                 ("C-<f8>" . dap-disconnect)
                 ("S-<f7>" . dap-step-out)
                 )))
  (setq lsp-completion-provider :none) ;; we use corfu
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
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
