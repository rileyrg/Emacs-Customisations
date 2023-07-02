(use-package eglot
  :demand
  :bind
  (:map flymake-mode-map
        ([remap next-error] . flymake-goto-next-error)
        ([remap previous-error] . flymake-goto-prev-error)))

(provide 'rgr/lsp)
