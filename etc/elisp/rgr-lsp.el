(use-package eglot
  :config
  (use-package eldoc-box)
  :hook
  (prog-mode . eglot-ensure)
  (prog-mode . eldoc-box-hover-mode)
  :bind
  (:map flymake-mode-map
        ([remap next-error] . flymake-goto-next-error)
        ([remap previous-error] . flymake-goto-prev-error)))

(provide 'rgr/lsp)
