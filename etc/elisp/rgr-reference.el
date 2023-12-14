(use-package eww
  :config
  ;; Advice EWW to launch certain URLs using the generic launcher rather than EWW.
  (defcustom rgr/eww-external-launch-url-chunks '("youtube")
    "If any component of this list is contained in an EWW url then it will use `browse-url-generic to launch that url instead of `eww"
    :type '(repeat string))
  (defadvice eww (around rgr/eww-extern-advise activate)
    "Use `browse-url-generic if any part of URL is contained in `rgr/eww-external-launch-url-chunks"
    (if (string-match-p (regexp-opt rgr/eww-external-launch-url-chunks) url)
        (browse-url-generic url)
      ad-do-it))

  :bind
  ("C-c o" . 'eww)
  (:map eww-mode-map
        ( "&" . (lambda()
                  (interactive)
                  (alert "Launching external browser")
                  (eww-browse-with-external-browser)))))

(require 'rgr/google "rgr-google")

(defun sys-browser-lookup(w template)
  (interactive)
  (browse-url-xdg-open (replace-regexp-in-string "%S%" (if w w (rgr/region-symbol-query)) template)))

(defun rgr/describe-symbol(w)
  (interactive (cons (rgr/region-symbol-query) nil))
  (let ((s (if (symbolp w) w (intern-soft w))))
    (if s (describe-symbol s)
      (message "No such symbol: %s" w))))

(use-package
  dictionary
  :commands (rgr/dictionary-search)
  :config
  (use-package mw-thesaurus)
  (defun rgr/dictionary-search(&optional w)
    (interactive)
    (dictionary-search (if w w (rgr/region-symbol-query))))
  :bind
  ("<f6>" . rgr/dictionary-search)
  ("S-<f6>" . mw-thesaurus-lookup-at-point))

(defun rgr/elisp-lookup-reference-dwim (&optional sym)
  "Checks to see if the 'thing' is known to elisp and, if so, use internal docs and return symbol else return nil to signal maybe fallback"
  (interactive)
  (let* ((sym (if sym sym (rgr/region-symbol-query)))
         (sym (if (symbolp sym) sym (intern-soft sym))))
    (when sym
      (if (fboundp sym)
          (if (featurep 'helpful)
              (helpful-function sym)
            (describe-function sym))
        (if (boundp sym)
            (if (featurep 'helpful)
                (helpful-variable sym)
              (describe-variable sym))
          (progn
            (let ((msg (format "No elisp help for '%s" sym)))
              (if (featurep 'alert)
                  (alert msg)
                (message msg)))
            (setq sym nil)))))
    sym))

(use-package
  goldendict
  :commands (goldendict-dwim)
  :config
  (defun goldendict-dwim
      (&optional
       w)
    "lookup word at region, thing at point or prompt for something, in goldendict. Use a prefix to force prompting."
    (interactive)
    (let ((w (if w w (rgr/region-symbol-query))))
      (call-process-shell-command (format  "goldendict \"%s\"" w ) nil 0)))
  :bind (("C-x G" . goldendict-dwim)))

(use-package devdocs-browser
  :custom
  (devdocs-browser-cache-directory (no-littering-expand-var-file-name  "devdocs-browser"))
  :config
  (defun rgr/devdocs(&optional i)
    (interactive)
    (if current-prefix-arg
        (call-interactively 'devdocs-browser-open-in)
      (devdocs-browser-open)))
  :bind
    ("C-q" . rgr/devdocs))

(use-package dash-docs)

(use-package elfeed
  :config
  (use-package elfeed-org
    :ensure t
    :custom
    (rmh-elfeed-org-files (list (no-littering-expand-etc-file-name "elfeed/elfeed.org")))
    :config
    (elfeed-org))
  (run-at-time nil (* 8 60 60) #'elfeed-update)
  :bind
  ( "C-c w" . elfeed)
  (:map elfeed-show-mode-map
        ("&" . (lambda()(interactive)(message "opening in eternal browser")(elfeed-show-visit t))))
  (:map elfeed-search-mode-map
        ("&" . (lambda()(interactive)(message "opening in eternal browser")(elfeed-search-browse-url t)))))



(use-package pdf-tools
  :after (org-plus-contrib)
  :config
  (pdf-tools-install)
  (add-hook 'pdf-isearch-minor-mode-hook (lambda () ;; (ctrlf-local-mode -1)
                                           ))
  (use-package org-pdftools
    :hook (org-mode . org-pdftools-setup-link)))

(use-package impatient-showdown
  :hook (markdown-mode . impatient-showdown-mode))

(provide 'rgr/reference)
