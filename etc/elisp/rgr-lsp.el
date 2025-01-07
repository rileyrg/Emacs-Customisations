(use-package eglot
  :straight(:type built-in)
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-ignored-server-capabilities '( :documentHighlightProvider));; dont let eglot/eldoc show doc, rather flymake.
  :config
  ;;(add-hook  'eglot-stay-out-of 'yasnippet)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun rgr/eglot-on-save()
    (when eglot--managed-mode
      (eglot-format-buffer)
      )
    )
  (defun rgr/eglot-managed-mode-hook()
    (message "rgr/eglot hook"))
  :hook
  (before-save . rgr/eglot-on-save)
  (eglot-managed-mode . rgr/eglot-managed-mode-hook)
  ((js-ts-mode c-ts-mode c++-ts-mode php-mode auctex-mode) . #'eglot-ensure)
  :bind
  (:map eglot-mode-map
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
  ;;(setq dape-key-prefix "\C-x\C-a")

  :custom
  (dape-default-breakpoints-file (no-littering-expand-var-file-name  "dape/dape-breakpoints"))
  (dape-buffer-window-arrangement 'right)
  (dape-info-hide-mode-line nil)
  (dape-inlay-hints t)
  ;;(dape-cwd-fn 'projectile-project-root)
  :hook
;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)
  ;;(dape-start . dape-breakpoint-load)
  (dape-display-source . pulsar-pulse-line)
  (dape-compile .  kill-buffer)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  ;; (advice-add 'dape-quit :after (lambda(&rest r)(dape-breakpoint-save dape-default-breakpoints-file)))
  (add-to-list 'recentf-exclude "dape-breakpoints")
  (dape-breakpoint-global-mode)
  (add-hook 'dape-info-parent-mode-hook
            (defun dape--info-rescale ()
              (face-remap-add-relative 'default :height 0.8)))
  )

(provide 'rgr/lsp)
