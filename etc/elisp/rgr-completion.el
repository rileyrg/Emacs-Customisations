(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current t)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :config
  (use-package orderless
    :custom
    ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
    ;; (orderless-component-separator #'orderless-escapable-split-on-space)
    (orderless-component-separator " +\\|[-/]")
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)

(use-package
  which-key
  :demand t
  :config (which-key-mode))

(use-package yasnippet
  :demand t
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode))
(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(setq-default abbrev-mode 1)
(defadvice expand-abbrev (after my-expand-abbrev activate)
  ;; if there was an expansion
  (if ad-return-value
      ;; start idle timer to ensure insertion of abbrev activator
      ;; character (e.g. space) is finished
      (run-with-idle-timer 0 nil
                           (lambda ()
                             ;; if there is the string "@@" in the
                             ;; expansion then move cursor there and
                             ;; delete the string
                             (let ((cursor "%CHANGEME%"))
                               (when (search-backward  cursor last-abbrev-location t)
                                 (goto-char  last-abbrev-location)
                                 (search-forward cursor)
                                 (backward-word)
                                 (highlight-symbol-at-point)
                                 (delete-char (length cursor))
                                 ))))))

(use-package company
  :disabled t
  :config
  (use-package company-box
    :config
    (setf (alist-get 'internal-border-width company-box-doc-frame-parameters) 1)
    :hook (company-mode . company-box-mode))
  :hook
  (prog-mode . company-mode)
  :bind( :map company-mode-map
         ("<tab>" .  company-indent-or-complete-common)))

;; Enable vertico
(use-package vertico
  ;;:disabled
  :custom
  (vertico-cycle t)
  :config
  (use-package vertico-prescient
    :disabled
    :init (vertico-prescient-mode)
    :custom
    (vertico-prescient-enable-sorting nil))
  :init
  (vertico-mode))

(setq-default abbrev-mode 1)

(provide 'rgr/completion)
