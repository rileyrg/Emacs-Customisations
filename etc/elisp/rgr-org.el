(use-package org
  :demand t
  :custom
  (org-agenda-files (no-littering-expand-etc-file-name "org/agenda-files.txt"))
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-babel-default-header-args:python
   '((:results  . "output")))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  :config
  (use-package org-contrib)
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

(use-package org-super-agenda
  :custom
  (org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Today"  ; Optionally specify section name
                :time-grid t  ; Items that appear on the time grid
                :todo "TODAY")  ; Items that have this TODO keyword
         (:name "Important"
                ;; Single arguments given alone
                :tag "bills"
                :priority "A")
         ;; Set order of multiple groups at once
         (:order-multi (2 (:name "home"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "@home"))
                          (:name "caravan"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "@caravan"))
                          (:name "shopping all"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "shopping" :not (:tag "@home @caravan")))
                          (:name "shopping"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "shopping" :not (:tag "@home @caravan")))
                          (:name "Emacs related"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :tag ("emacs"))
                          (:name "Programming related"
                                 :and (:tag ("programming") :not (:tag "emacs")))
                          (:name "Food-related"
                                 ;; Multiple args given in list with implicit OR
                                 :tag ("food" "dinner" "lunch" "breakfast"))
                          (:name "Personal"
                                 :habit t
                                 :tag "personal")
                          ))
         ;; Groups supply their own section names when none are given
         (:todo "WAITING" :order 8)  ; Set order of this section
         (:todo "STARTED" :order 8)
         (:todo ("SOMEDAY" "TOREAD" "CHECK" "TO-WATCH" "WATCHING")
                ;; Show this group at the end of the agenda (since it has the
                ;; highest number). If you specified this group last, items
                ;; with these todo keywords that e.g. have priority A would be
                ;; displayed in that group instead, because items are grouped
                ;; out in the order the groups are listed.
                :order 9)
         (:priority<= "B"
                      ;; Show this section after "Today" and "Important", because
                      ;; their order is unspecified, defaulting to 0. Sections
                      ;; are displayed lowest-number-first.
                      :order 1)
         ;; After the last group, the agenda will display items that didn't
         ;; match any of these groups, with the default order position of 99
         ))
  :init
  (org-super-agenda-mode))

(use-package org-roam
  :demand
  :custom
  (epa-file-encrypt-to "rileyrg")
  (epa-file-select-keys "auto")
  (org-roam-dailies-directory "daily/")
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :if-new
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}
")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :if-new (file+head "%<%Y-%m-%d>.org.gpg"
                         "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-directory (no-littering-expand-var-file-name "org/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n r" . org-roam-add-ref)
         ("C-c n g" . org-roam-graph)
         ;; ("C-c n t" . org-roam-dailies-goto-today)
         ;; ("C-c n y" . org-roam-dailies-goto-yesterday)
         ;; ("C-c n d" . org-roam-dailies-goto-date)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n C" . org-roam-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (org-roam-setup)
  (require 'org-roam-dailies)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package
  ox-gfm
  :demand)

(provide 'rgr/org)
