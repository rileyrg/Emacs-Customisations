(use-package eglot
  ;;:disabled t
  :config
  (defun rgr/eglot-format-buffer()
    (when eglot--managed-mode
      (eglot-format-buffer)
      ))
  (defun rgr/eglot-hook()
    (setq-local rgr/orig-eldoc-documentation-functions eldoc-documentation-functions)
    (setq-local rgr/eldoc-documentation-functions
                (remove #'eglot-signature-eldoc-function (remove #'eglot-hover-eldoc-function  eldoc-documentation-functions)))
    (setq-local eldoc-documentation-functions rgr/eldoc-documentation-functions))

  (defun rgr/eldoc-box-help-at-point()
    (interactive)
    (setq-local eldoc-documentation-functions rgr/orig-eldoc-documentation-functions )
    (eldoc-box-help-at-point)
    (setq-local eldoc-documentation-functions
          rgr/eldoc-documentation-functions))
  :hook
  (before-save . rgr/eglot-format-buffer)
  (eglot-managed-mode . rgr/eglot-hook)
  ((js-ts-mode c-ts-mode c++-ts-mode php-mode auctex-mode) . #'eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-." . rgr/eldoc-box-help-at-point)
        ("<C-return>" . eglot-code-actions)))

(use-package eglot-booster
  ;;:disabled t
  :straight (:type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config	(eglot-booster-mode))

(use-package dape
  :demand t
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-a")

  :custom
  (dape-default-breakpoints-file (no-littering-expand-var-file-name  "dape/dape-breakpoints"))
  (dape-buffer-window-arrangement 'right)
  (dape-info-hide-mode-line nil)
  (dape-inlay-hints t)
  ;;(dape-cwd-fn 'projectile-project-root)
  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
   (after-init . dape-breakpoint-load)
   (dape-display-source . pulsar-pulse-line)
   (dape-compile .  kill-buffer))

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (add-to-list 'recentf-exclude "dape-breakpoints")
  (dape-breakpoint-global-mode))

(provide 'rgr/lsp)
