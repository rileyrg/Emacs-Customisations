;;(straight-use-package '(org-plus-contrib :includes (org)))

(use-package org
  :demand t
  :custom
  (org-agenda-files (no-littering-expand-etc-file-name "org/agenda-files.txt"))
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-src-preserve-indentation t)
  (org-babel-default-header-args:python
   '((:results  . "output")))
  :config
  (set-face-attribute 'org-headline-done nil :strike-through t)
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
  ("C-c C-t" . org-todo)
  (:map org-mode-map  ("M-." . find-function-at-point)
        ("<f11>" . org-edit-special))
  (:map org-src-mode-map ("<f11>" . org-edit-src-exit)))

(require 'org-id)

(require 'org-crypt)
(org-crypt-use-before-save-magic)

(use-package ob-async)

(use-package
  ox-gfm
  :demand)

(provide 'rgr/org)
