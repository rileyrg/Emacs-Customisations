(use-package eglot
  ;;:disabled t
   :config
   (defun rgr/eglot-format-buffer()
     (when eglot--managed-mode
       (eglot-format-buffer)
       ))
   :hook
   (before-save . rgr/eglot-format-buffer)
   ((js-ts-mode c-ts-mode c++-ts-mode php-mode auctex-mode) . #'eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-." . eldoc-box-eglot-help-at-point)
        ("<C-return>" . eglot-code-actions)))

(use-package eglot-booster
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
