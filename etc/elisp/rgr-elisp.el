(use-package rgr-kill-dwim
  :straight (rgr-kill-dwim :local-repo "~/development/projects/emacs/rgr-kill-dwim" :type git :host github :repo "rileyrg/rgr-kill-dwim" )
  :bind
  ("M-w" . rgr/kill-dwim))

(use-package flymake
  :custom
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-no-changes-timeout 1.5)
  :bind
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error))

(use-package flymake-shellcheck
  :disabled t
  :commands flymake-shellcheck-load
  :init
  (defun rgr/sh-mode-hook()
    (flymake-shellcheck-load)
    (flymake-mode +1))
  :hook (sh-mode . rgr/sh-mode-hook))

(defun rgr/emacs-lisp-help (&optional s)
  "Elisp help at point. default to `helpful-at-point' if available, else `describe-function' or `describe-variable'."
  (interactive)
  (let* ((sym (or s (thing-at-point 'symbol)))
         (sym (if (symbolp sym) sym (intern-soft sym))))
    (when sym
      (if (fboundp sym)
          (if (featurep 'helpful)
              (helpful-function sym)
            (describe-function sym))
        (if (boundp sym)
            (if (featurep 'helpful)
                (helpful-variable sym)
              (describe-variable sym))
          (progn
            (let ((msg (format "No elisp help for '%s" sym)))
              (alert msg))
            (setq sym nil)))))))

(add-to-list 'Info-directory-list (no-littering-expand-etc-file-name  "info"))

(defun rgr/elisp-edit-mode()
  "return non nil if this buffer edits elisp"
  (member major-mode '(emacs-lisp-mode lisp-interaction-mode)))

(use-package package-lint)

(use-package helpful)

(use-package el-docstring-sap
  :straight (el-docstring-sap :local-repo "~/development/projects/emacs/el-docstring-sap" :type git :host github :repo "rileyrg/el-docstring-sap" )
  :init
  (use-package quick-peek)
  :hook
  (emacs-lisp-mode . el-docstring-sap-mode)
  :bind
  ("M-<f2>" . el-docstring-sap-display)
  ("M-<f1>" . el-docstring-sap-mode))

(use-package edebug-x
  :custom
  (debugger-stack-frame-as-list t)
  ;;(edebug-trace nil)
  :config
  (defun rgr/instrumentForDebugging() "use the universal prefix arg (C-u) to remove instrumentation"
         (interactive)
         (if current-prefix-arg (eval-defun nil)
           (eval-defun 0)))
  :bind
  ("C-S-<f9>" . toggle-debug-on-error)
  ("C-<f9>" . rgr/instrumentForDebugging))

;; bit more convoluted than it needs to be
;; but I fancied using thing-at-point
(defun rgr/edebug-display-thing-at-point()
  (let ((tap (thing-at-point 'var)))
    (if tap
        (message "%s: %s" (nth 0 tap) (nth 1 tap)))))

(defun rgr/edebug-thing-at-point()
  "message display the vale of the symbol at point"
  (let((tap (thing-at-point 'symbol)))
    (if tap
        (let((sym (if (symbolp tap) tap (intern-soft tap))))
          (condition-case nil
              (list sym (edebug-eval  sym))
            (error nil))))))

(defun rgr/edebug-mode-hook()
  (setq-local thing-at-point-provider-alist
              (append thing-at-point-provider-alist
                      '((var . rgr/edebug-thing-at-point))))
  "add a call to display the value at point when debugging with edebug"
  (add-hook 'post-command-hook #'rgr/edebug-display-thing-at-point nil :local))

(add-hook 'edebug-mode-hook  #'rgr/edebug-mode-hook)

(use-package
  elisp-format
  :bind
  (:map emacs-lisp-mode-map
        ("C-c f" . elisp-format-region)))

(use-package popup
  :config
  (defun rgr/show-symbol-details ()
    (interactive)
    (popup-tip (format "intern-soft thing-at-point: %s, symbolp: %s, symbol-name:%s"
                       (setq-local sym (intern-soft (thing-at-point 'symbol)))
                       (symbolp sym)
                       (symbol-name sym))))
  :bind
  (:map emacs-lisp-mode-map (("M-6" . #'rgr/show-symbol-details))))

(provide 'rgr/elisp)
