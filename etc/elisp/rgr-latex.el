(use-package emacs
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-PDF-mode t)
  (org-preview-latex-default-process 'dvipng)
  :hook
  (TeX-mode .
                 (lambda () (TeX-fold-mode 1))); Automatically activate TeX-fold-mode.
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . LaTeX-math-mode))

(provide 'rgr/latex)
