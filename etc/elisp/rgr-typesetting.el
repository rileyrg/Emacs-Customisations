(use-package auctex
  :init
  (require 'ox-latex)
  (use-package lsp-latex)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-PDF-mode t)
  (org-preview-latex-default-process 'dvipng)
  :config
  (defun rgr/latex-mode-hook()
    ;; buggy as this then gets savd to the global abbrevs
    (load-file (no-littering-expand-etc-file-name "abbrev/latex-songbook-chords.el"))
    (setq abbrevs-changed nil)
    (turn-on-reftex)
    (visual-line-mode)
    (LaTeX-math-mode)
    (flyspell-mode)
    (lsp-deferred))

  :hook
  (TeX-mode .
            (lambda () (rgr/latex-mode-hook)(TeX-fold-mode 1)))); Automatically activate TeX-fold-mode.

(use-package
  lilypond
  :disabled)

(provide 'rgr/typesetting)
