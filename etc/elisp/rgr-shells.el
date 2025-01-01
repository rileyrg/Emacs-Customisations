(use-package  eat
  :custom
  (eat-kill-buffer-on-exit t)
  :config
  (defun rgr/eat()
    (interactive)
    (split-window)
    (eat))
  :bind
  ("M-g t" . rgr/eat)
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package  multi-vterm
  :init
  (add-to-list 'display-buffer-alist  '("vterm" (display-buffer-reuse-mode-window display-buffer-below-selected) (dedicated . t) (window-height . 0.3)) ))

(provide 'rgr/shells)
