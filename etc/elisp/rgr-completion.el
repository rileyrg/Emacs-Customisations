(use-package
  which-key
  :demand t
  :config (which-key-mode))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode))

(setq-default abbrev-mode 1)

(use-package company
  ;;:disabled
  :config
  (use-package company-box
    :config
    (setf (alist-get 'internal-border-width company-box-doc-frame-parameters) 1)
    :hook (company-mode . company-box-mode))
  :bind( :map company-mode-map
         ("<tab>" .  company-indent-or-complete-common))
  :hook
  (prog-mode . company-mode))

(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil);;'((file (styles partial-completion))))
  ;; A few more useful configurations...
  (use-package emacs
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
    ;; Vertico commands are hidden in normal buffers.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t)))

(use-package corfu
  :disabled
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;;(corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current t)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :straight (:files (:defaults "extensions/*"))
  :init
  (use-package lsp-mode
    :custom
    (lsp-completion-provider :none) ;; we use Corfu!
    :init
    (defun my/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))

    :hook
    (lsp-completion-mode . my/lsp-mode-setup-completion))
  :config
  (corfu-popupinfo-mode)
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

;; Add extensions
(use-package cape
  :disabled
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)

  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Enable vertico
(use-package vertico
  ;;:disabled
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(setq-default abbrev-mode 1)

(provide 'rgr/completion)
