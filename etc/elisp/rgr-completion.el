(use-package
  which-key
  :demand t
  :config (which-key-mode))

(setq-default abbrev-mode 1)

;; Configure corfu
(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  ;; :bind (:map corfu-map
  ;;        ("TAB" . corfu-next)
  ;;        ("S-TAB" . corfu-previous))

  ;; Enable the overlay only for certain modes.
  ;; For example it is not a useful UI for completions at point in the
  ;; minibuffer.
  :hook ((prog-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :config

  ;; Optionally enable cycling for `corfu-next' and `corfu-previous'.
  ;; (setq corfu-cycle t)
  )
;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; Completion is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(provide 'rgr/completion)
