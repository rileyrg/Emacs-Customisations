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
