(use-package org
  :custom
  (org-babel-default-header-args:python
        '((:results  . "output")))
  :config
  (use-package ob-async)

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
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(require 'org-crypt)
(setq  org-crypt-key "rileyrg")
(org-crypt-use-before-save-magic)

(use-package org-journal
  :demand t
  :custom
  (org-journal-dir (expand-file-name "journals" org-directory))
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-date-format "%d/%m/%Y")
  :config
  ;;(add-to-list 'org-agenda-files org-journal-dir)

  :hook (org-journal-mode  . (lambda ()
                               (local-unset-key (kbd "C-c C-s"))))
  :bind (
         ("C-c S" . org-journal-search)
         ("C-c J" . org-journal-new-entry)
         ))

(defun insert-property(&optional p)
  "insert PROPERTY value of pdftools link"
  (unless p (setq p "TEST"))
  (message "property passed is: %s" p)
  (let ((pvalue
         (save-window-excursion
           (message "%s" (org-capture-get :original-buffer))
           (switch-to-buffer (org-capture-get :original-buffer))
           (org-entry-get (point) p)
           )))
    pva))

(use-package f);; f-touch

(use-package ox-gfm);; github compliant markdown

;; (defun rgr/org-tangle-and-export(&optional filename)
;;   (when (buffer-file-name)
;;     (let* ((filename (if filename filename (buffer-file-name)))
;;            (f-org (concat (file-name-sans-extension filename) ".org"))
;;            (f-export (concat (file-name-sans-extension f-org) ".export"))
;;            (f-tangle (concat (file-name-sans-extension f-org) ".tangle"))
;;            (alert-fade-time 3))
;;       (when (and (file-exists-p f-export) (file-newer-than-file-p f-org f-export))
;;         (when (featurep 'alert)
;;           (alert (format "%s is older than %s,exporting." f-export f-org)))
;;         (org-gfm-export-to-markdown)
;;         (f-touch f-export))
;;       (when (and (file-exists-p f-tangle) (file-newer-than-file-p f-org f-tangle))
;;         (when(featurep 'alert)
;;           (alert (format "%s is older than %s,tangling." f-tangle f-org)))
;;         (org-babel-tangle)
;;         (f-touch f-tangle)))))

;; (advice-add #'magit-status :before (lambda()"look to see if we need to export and tangle"(interactive)(rgr/org-tangle-and-export)))

;; ;;             (add-hook 'after-save-hook (lambda(tangle)(interactive "P")(when tangle (call-interactive #'rgr/org-tangle-and-export))))
;; (add-hook 'after-save-hook (lambda()(when current-prefix-arg (rgr/org-tangle-and-export))))

(provide 'rgr/org)
