(use-package org-plus-contrib
  :custom
  (org-babel-default-header-args:python
   '((:results  . "output")))
  :config
  (defun rgr/org-refile-targets() ;;(rgr/org-refile-targets)
    (directory-files-recursively org-directory "^[[:alnum:]].*\\.\\(org\\|gpg\\)\\'"))
  (defun rgr/org-agenda (&optional arg)
    (interactive "P")
    (let ((org-agenda-tag-filter-preset '("-trash")))
      (org-agenda arg "a")))
  :bind
  ("C-c a" . org-agenda)
  ("C-c A" . rgr/org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c C-l" . org-insert-link)
  ("C-c C-s" . org-schedule)
  ("C-c C-t" . org-todo))

(require 'org-id)

(use-package ob-async)

(use-package ox-gfm)

(provide 'rgr/org)
