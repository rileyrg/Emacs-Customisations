;; TODO (straight-use-package `(eglot ,@(when (>= emacs-major-version 29) '(:type built-in))))
;; not working :   (use-package `(eglot ,@(when (>= emacs-major-version 29) '(:straight (:type built-in)))))
(use-package eglot :straight (:type built-in)
  :config
  (use-package eldoc-box)
  :hook
  (prog-mode . eglot-ensure)
  (prog-mode . eldoc-box-hover-mode)
  :bind
  (:map flymake-mode-map
        ([remap next-error] . flymake-goto-next-error)
        ([remap previous-error] . flymake-goto-prev-error))
  (:map eglot-mode-map
        ("<M-return>" . eglot-code-actions)))

(provide 'rgr/lsp)
