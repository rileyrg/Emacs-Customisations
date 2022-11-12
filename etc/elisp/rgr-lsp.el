;; if you want to change prefix for lsp-mode keybindings.
(use-package lsp-mode
  :custom
  (lsp-auto-configure t)
  (lsp-auto-guess-root nil)
  (lsp-clients-clangd-args '("--header-insertion-decorators=0" "--fallback-style=Google"))
  (lsp-completion-enable  t)
  (lsp-completion-provider :none)
  (lsp-completion-show-kind t)
  (lsp-diagnostics-provider :auto)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-lens-enable t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-signature-auto-activate t)
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-delay 2.5)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics nil)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . #'lsp-ui-peek-find-references)))

(use-package dap-mode
  :commands rgr/dap-debug
  :custom
  (dap-auto-configure-features '(locals  tooltip))
  :config
  (setq dap-ui-buffer-configurations
        `((,"*Dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.50))) ;; changed this to 0.50
          (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.50)))
          (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.50)))
          (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
          (,"*debug-window*" . ((side . bottom) (slot . 3) (window-width . 0.20)))))
  (defun rgr/dap-debug()
    (interactive)
    (if current-prefix-arg
        (call-interactively 'dap-debug)
      (dap-debug-last)))
  ;;(require 'dap-gdb-lldb)
  ;;(dap-gdb-lldb-setup)
  ;;(require 'dap-codelldb)
  ;;(dap-codelldb-setup)
  (require 'dap-cpptools)
  ;;(dap-cpptools-setup)
  ;; (require 'dap-lldb)
  (add-hook 'dap-stopped-hook
            (lambda (arg)
              (call-interactively #'dap-hydra)))
  :config
  (require 'dap-chrome)
  :bind
  (:map lsp-mode-map
        ("C-<f9>" . 'rgr/dap-debug))
  (:map dap-mode-map
        ("<f8>" . dap-continue)
         ("C-S-<f8>" . dap-delete-session)
         ("<f9>" . dap-hydra)
         ("<f10>" . dap-next)
         ("<f11>" . dap-step-in)
         ("S-<f11>" . dap-step-out)
         ))

(provide 'rgr/lsp)
