(use-package
  which-key
  :demand t
  :config (which-key-mode))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode))

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
  :config
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)
  (defun disable-selection ()
    (when (eq minibuffer-completion-table #'org-tags-completion-function)
      (setq-local vertico-map minibuffer-local-completion-map
                  completion-cycle-threshold nil
                  completion-styles '(basic))))
  (advice-add #'vertico--setup :before #'disable-selection))

(setq-default abbrev-mode 1)

(provide 'rgr/completion)
