(use-package
  which-key
  :demand t
  :config (which-key-mode))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

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
  ;; (advice-add #'completing-read-multiple
  ;;             :override #'consult-completing-read-multiple)
  (defun disable-selection ()
    (when (eq minibuffer-completion-table #'org-tags-completion-function)
      (setq-local vertico-map minibuffer-local-completion-map
                  completion-cycle-threshold nil
                  completion-styles '(basic))))
  (advice-add #'vertico--setup :before #'disable-selection))

(setq-default abbrev-mode 1)

(provide 'rgr/completion)
