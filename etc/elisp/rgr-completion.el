(setq-default abbrev-mode 1)

(use-package
  company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :custom
  (company-backends
   '((company-capf :with company-dabbrev company-yasnippet company-files company-ispell)))
  :config
  (use-package
    company-box
    :hook (company-mode . company-box-mode))
  (require 'company-ispell)
  (use-package company-prescient
    :after company
    :config
    (company-prescient-mode +1))
  :bind ("C-<tab>" . company-complete))

(use-package
  yasnippet
  :init (yas-global-mode 1)
  :config
  (require 'company-yasnippet)
  (use-package
    el-autoyas)
  (use-package
    yasnippet-snippets)
  (use-package
    yasnippet-classic-snippets))

(use-package
  which-key
  :demand t
  :config (which-key-mode))

(defun check-expansion ()
  (save-excursion (if (looking-at "\\_>") t (backward-char 1)
                      (if (looking-at "\\.") t (backward-char 1)
                          (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (yas/create-php-snippet)))))
;;              (indent-for-tab-command)))))

(provide 'rgr/completion)
