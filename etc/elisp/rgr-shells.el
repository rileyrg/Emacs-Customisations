(use-package
  eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (defun rgr/projectile-term()
    (interactive)
    (if (string-equal major-mode "eat-mode")
        (previous-buffer)
      (let ((default-directory (projectile-project-root)))
        (eat))))

  :bind
  ("M-g v" . #'rgr/projectile-term))

(provide 'rgr/shells)
