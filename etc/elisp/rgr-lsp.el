(use-package eglot
  :demand t
  ;;:straight `(eglot ,@(when (>= emacs-major-version 29) '(:type built-in)))
  :init
  (add-hook 'after-save-hook 'eglot-format-buffer)
  :custom
  (eglot--mode-line-format nil)
  :bind
  (:map eglot-mode-map
        ("<C-return>" . eglot-code-actions)))

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
  (dape-cwd-fn 'projectile-project-root)
  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
   (after-init . dape-breakpoint-load)
   (dape-display-source . pulsar-pulse-line)
   (dape-compile .  kill-buffer))

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode))

(provide 'rgr/lsp)
