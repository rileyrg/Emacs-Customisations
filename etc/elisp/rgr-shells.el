;; back to vterm
(use-package  eat
  :disabled  t
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
  (add-to-list 'display-buffer-alist  '((or (major-mode . vterm-mode))
                                        (display-buffer-reuse-mode-window
                                         display-buffer-in-direction)
                                        (direction . below)
                                        (window-height . 0.3)))
  :bind
  ("M-g t" . multi-vterm-project))

(provide 'rgr/shells)
