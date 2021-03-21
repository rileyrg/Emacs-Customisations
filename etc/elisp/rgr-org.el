(use-package org
  :demand t
  :custom
  (org-M-RET-may-split-line nil)
  (org-agenda-files (no-littering-expand-etc-file-name "org/agenda-files.txt" ))
  (org-agenda-include-diary t)

  (org-agenda-remove-tags nil)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-show-inherited-tags t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-scheduled-delay-if-deadline t)
  (org-agenda-start-on-weekday 0)
  (org-agenda-window-setup 'current-window)
  (org-babel-load-languages '((emacs-lisp . t)(python . t) (shell . t)))
  ;;         (org-babel-python-command "python3")
  (org-catch-invisible-edits 'smart)
  (org-clock-into-drawer t)
  (org-clock-persist 'history)
  (org-confirm-babel-evaluate nil)
  (org-contacts-files (no-littering-expand-etc-file-name  "org/orgfiles/contacts.org"))
  (org-crypt-disable-auto-save t)
  (org-ctrl-k-protect-subtree t)
  (org-default-notes-file "refile.org")
  (org-directory (no-littering-expand-var-file-name "org/orgfiles" ))
  (org-enforce-todo-dependencies t)
  (org-export-backends '(ascii html icalendar latex md odt))
  (org-google-weather-location "Hamburg,Germany" t)
  (org-goto-interface 'outline-path-completion)
  (org-link-search-must-match-exact-headline nil)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-note-clock-out t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-outline-path 'file)
  (org-refile-targets         '((org-agenda-files :tag . "refile")           (org-agenda-files :maxlevel . 16)))
  (org-remember-clock-out-on-exit nil t)
  (org-return-follows-link t)
  (org-reverse-note-order t)
  (org-tag-persistent-alist '(("noexport" . 110) ("trash" . 116)))
  (org-tags-exclude-from-inheritance '("PROJECT" "DEFAULTCLOCKTASK" "crypt"))
  (org-use-property-inheritance t)
  :init
  (defun rgr/org-refile-targets()
    (directory-files-recursively org-directory "^[[:alnum:]].*\\.\\(org\\|gpg\\)\\'"))
  :config
  (setq org-babel-default-header-args:python
        '((:results  . "output")))

  (progn
    ;;  stuff for setting fixed ref links - https://blog.phundrak.com/better-custom-ids-orgmode/
    (require 'org-id)
    (defun eos/org-id-new (&optional prefix)
      "Create a new globally unique ID.

       An ID consists of two parts separated by a colon:
       - a prefix
       - a   unique   part   that   will   be   created   according   to
         `org-id-method'.

       PREFIX  can specify  the  prefix,  the default  is  given by  the
       variable  `org-id-prefix'.  However,  if  PREFIX  is  the  symbol
       `none', don't  use any  prefix even if  `org-id-prefix' specifies
       one.

       So a typical ID could look like \"Org-4nd91V40HI\"."
      (let* ((prefix (if (eq prefix 'none)
                         ""
                       (concat (or prefix org-id-prefix)
                               "-"))) unique)
        (if (equal prefix "-")
            (setq prefix ""))
        (cond
         ((memq org-id-method
                '(uuidgen uuid))
          (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
          (unless (org-uuidgen-p unique)
            (setq unique (org-id-uuid))))
         ((eq org-id-method 'org)
          (let* ((etime (org-reverse-string (org-id-time-to-b36)))
                 (postfix (if org-id-include-domain
                              (progn
                                (require 'message)
                                (concat "@"
                                        (message-make-fqdn))))))
            (setq unique (concat etime postfix))))
         (t (error "Invalid `org-id-method'")))
        (concat prefix (car (split-string unique "-")))))
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
    (defun eos/org-custom-id-get (&optional pom create prefix)
      "Get the CUSTOM_ID property of the entry at point-or-marker POM.

       If POM is nil, refer to the entry at point. If the entry does not
       have an CUSTOM_ID, the function returns nil. However, when CREATE
       is non nil, create a CUSTOM_ID if none is present already. PREFIX
       will  be passed  through to  `eos/org-id-new'. In  any case,  the
       CUSTOM_ID of the entry is returned."
      (interactive)
      (org-with-point-at pom
        (let* ((orgpath (mapconcat #'identity (org-get-outline-path) "-"))
               (heading (replace-regexp-in-string
                         "/\\|~\\|\\[\\|\\]" ""
                         (replace-regexp-in-string
                          "[[:space:]]+" "_" (if (string= orgpath "")
                                                 (org-get-heading t t t t)
                                               (concat orgpath "-" (org-get-heading t t t t))))))
               (id (org-entry-get nil "CUSTOM_ID")))
          (cond
           ((and id
                 (stringp id)
                 (string-match "\\S-" id)) id)
           (create (setq id (eos/org-id-new (concat prefix heading)))
                   (org-entry-put pom "CUSTOM_ID" id)
                   (org-id-add-location id
                                        (buffer-file-name (buffer-base-buffer)))
                   id)))))

    (defun eos/org-add-ids-to-headlines-in-file ()
      "Add CUSTOM_ID properties to all headlines in the current file
       which do not already have one.

       Only adds ids if the `auto-id' option is set to `t' in the file
       somewhere. ie, #+OPTIONS: auto-id:t"
      (interactive)
      (save-excursion
        (widen)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t"
                                 (point-max)
                                 t)
          (org-map-entries (lambda ()
                             (eos/org-custom-id-get (point)
                                                    'create))))))

    (add-hook 'org-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (when (and (eq major-mode 'org-mode)
                                       (eq buffer-read-only nil))
                              (eos/org-add-ids-to-headlines-in-file)))))))

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

(use-package ob-async)

(use-package
  org-bullets
  :demand
  :config (add-hook 'org-mode-hook (lambda ()
                                     (org-bullets-mode 1)
                                     ;; (org-num-mode 1)
                                     )))

(org-clock-persistence-insinuate)
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

;; The following lines are always needed.  Choose your own keys.

(defface org-canceled
  ;; originally copied from font-lock-type-face
  (org-compatible-face nil '((((class color)
                               (min-colors 16)
                               (background light))
                              (:foreground "darkgrey"
                                           :bold t))
                             (((class color)
                               (min-colors 16)
                               (background dark))
                              (:foreground "grey"
                                           :bold t))
                             (((class color)
                               (min-colors 8))
                              (:foreground "grey"))
                             (t
                              (:bold t))))
  "Face used for todo keywords that indicate DONE items."
  :group 'org-faces)

(defface org-wait
  ;; originally copied from font-lock-type-face
  (org-compatible-face nil '((((class color)
                               (min-colors 16)
                               (background light))
                              (:foreground "darkgrey"
                                           :bold t))
                             (((class color)
                               (min-colors 16)
                               (background dark))
                              (:foreground "grey"
                                           :bold t))
                             (((class color)
                               (min-colors 8))
                              (:foreground "grey"))
                             (t
                              (:bold t))))
  "Face used for todo keywords that indicate DONE items."
  :group 'org-faces)

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(use-package org-appear
  :disabled ;; PITA
  :straight(:type git :host github :repo "awth13/org-appear")
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("ba" . "src bash"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

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

(defun rgr/org-tangle-and-export(&optional filename)
  (when (buffer-file-name)
    (let* ((filename (if filename filename (buffer-file-name)))
           (f-org (concat (file-name-sans-extension filename) ".org"))
           (f-export (concat (file-name-sans-extension f-org) ".export"))
           (f-tangle (concat (file-name-sans-extension f-org) ".tangle"))
           (alert-fade-time 3))
      (when (and (file-exists-p f-export) (file-newer-than-file-p f-org f-export))
        (when (featurep 'alert)
          (alert (format "%s is older than %s,exporting." f-export f-org)))
        (org-gfm-export-to-markdown)
        (f-touch f-export))
      (when (and (file-exists-p f-tangle) (file-newer-than-file-p f-org f-tangle))
        (when(featurep 'alert)
          (alert (format "%s is older than %s,tangling." f-tangle f-org)))
        (org-babel-tangle)
        (f-touch f-tangle)))))

(advice-add #'magit-status :before (lambda()"look to see if we need to export and tangle"(interactive)(rgr/org-tangle-and-export)))

;;             (add-hook 'after-save-hook (lambda(tangle)(interactive "P")(when tangle (call-interactive #'rgr/org-tangle-and-export))))
(add-hook 'after-save-hook (lambda()(when current-prefix-arg (rgr/org-tangle-and-export))))

(provide 'rgr/org)
