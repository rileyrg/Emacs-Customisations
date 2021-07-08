(use-package
  which-key
  :demand t
  :config (which-key-mode))

(setq-default abbrev-mode 1)

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Enable vertico
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  ;; Use `consult-completion-in-region' if Vertico is enabled
  (when (not (featurep 'corfu))
             (add-hook 'vertico-mode-hook (lambda ()
                                 (setq completion-in-region-function
                                       (if vertico-mode
                                           #'consult-completion-in-region
                                         #'completion--in-region)))))
  (vertico-mode)
  :bind
  (:map vertico-map
        ( "<tab>" . vertico-next)
        ( "<backtab>" . vertico-previous)))

(setq-default abbrev-mode 1)

(provide 'rgr/completion)
