(use-package
  which-key
  :demand t
  :config (which-key-mode))

(setq-default abbrev-mode 1)

;; Enable vertico
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  (add-hook 'vertico-mode-hook (lambda ()
                                 (setq completion-in-region-function
                                       (if vertico-mode
                                           #'consult-completion-in-region
                                         #'completion--in-region))))
  (vertico-mode))

(provide 'rgr/completion)
