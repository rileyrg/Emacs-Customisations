(use-package  eat
  :custom
  (eat-kill-buffer-on-exit t)
  :bind
  ("M-g t" . #'eat)
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package  vterm)

(provide 'rgr/shells)
