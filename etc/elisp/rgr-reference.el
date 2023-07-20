(custom-set-variables
 '(eww-search-prefix "https://google.com/search?q=")
 '(browse-url-browser-function 'eww-browse-url)
 '(browse-url-generic-program "google-chrome")
 '(browse-url-secondary-browser-function 'browse-url-default-browser))

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
  (:map eww-mode-map
        ( "&" . (lambda()
                  (interactive)
                  (alert "Launching external browser")
                  (eww-browse-with-external-browser)))))

(require 'rgr/google "rgr-google")

(defgroup rgr/lookup-reference nil
  "Define functions to be used for lookup"
  :group 'rgr)

(defcustom mode-lookup-reference-functions-alist '(
                                                   (nil (goldendict-dwim goldendict-dwim))
                                                   (c-mode  (rgr/devdocs rgr/devdocs))
                                                   (c++-mode  (rgr/devdocs rgr/devdocs))
                                                   (flutter-mode  (rgr/devdocs rgr/devdocs))
                                                   (dart-mode  (rgr/devdocs rgr/devdocs))
                                                   (gdscript-mode  (rgr/devdocs rgr/devdocs))
                                                   ;;                                                         (gdscript-mode  (rgr/gdscript-docs-browse-symbol-at-point rgr/devdocs))
                                                   (php-mode  (rgr/devdocs rgr/devdocs))
                                                   (web-mode  (rgr/devdocs rgr/devdocs))
                                                   (org-mode (rgr/elisp-lookup-reference-dwim))
                                                   (Info-mode (rgr/elisp-lookup-reference-dwim))
                                                   (js2-mode (rgr/devdocs rgr/devdocs))
                                                   (python-mode (rgr/devdocs rgr/devdocs))
                                                   (js-mode (rgr/devdocs rgr/devdocs))
                                                   (rjsx-mode (rgr/devdocs rgr/devdocs))
                                                   (typescript-mode (rgr/devdocs rgr/devdocs))
                                                   (lisp-interaction-mode (rgr/elisp-lookup-reference-dwim rgr/devdocs))
                                                   (emacs-lisp-mode (rgr/elisp-lookup-reference-dwim rgr/devdocs)))
  "mode lookup functions"
  :group 'rgr/lookup-reference)

(defun get-mode-lookup-reference-functions(&optional m)
  (let* ((m (if m m major-mode))
         (default-funcs (copy-tree(cadr (assoc nil mode-lookup-reference-functions-alist))))
         (mode-funcs (cadr (assoc m mode-lookup-reference-functions-alist))))
    (if mode-funcs (progn
                     (setcar default-funcs (car mode-funcs))
                     (if (cadr mode-funcs)
                         (setcdr default-funcs (cdr mode-funcs)))))
    default-funcs)) ;; (get-mode-lookup-reference-functions 'org-mode)

(defcustom linguee-url-template "https://www.linguee.com/english-german/search?source=auto&query=%S%"
  "linguee url search template"
  :type 'string
  :group 'rgr/lookup-reference)

(defcustom php-api-url-template "https://www.google.com/search?q=php[%S%]"
  "php api url search template"
  :type 'string
  :group 'rgr/lookup-reference)

(defcustom jquery-url-template "https://api.jquery.com/?s=%S%"
  "jquery url search template"
  :type 'string
  :group 'rgr/lookup-reference)

(defcustom  lookup-reference-functions '(rgr/describe-symbol goldendict-dwim rgr/linguee-lookup rgr/dictionary-search google-this-search)
  "list of functions to be called via C-n prefix call to lookup-reference-dwim"
  :type 'hook
  :group 'rgr/lookup-reference)

(defun sys-browser-lookup(w template)
  (interactive)
  (browse-url-xdg-open (replace-regexp-in-string "%S%" (if w w (rgr/region-symbol-query)) template)))

(defun rgr/describe-symbol(w)
  (interactive (cons (rgr/region-symbol-query) nil))
  (let ((s (if (symbolp w) w (intern-soft w))))
    (if s (describe-symbol s)
      (message "No such symbol: %s" w))))

(defun rgr/linguee-lookup(w)
  (interactive (cons (rgr/region-symbol-query) nil))
  (sys-browser-lookup w linguee-url-template))

(defun rgr/gdscript-docs-browse-symbol-at-point(&optional w)
  (gdscript-docs-browse-symbol-at-point))

(defun lookup-reference-dwim(&optional secondary)
  "if we have a numeric prefix then index into lookup-reference functions"
  (interactive)
  (let((w (rgr/region-symbol-query))
       ;; PREFIX integer including 4... eg C-2 lookup-reference-dwim
       (idx (if (and  current-prefix-arg (not (listp current-prefix-arg)))
                (- current-prefix-arg 1)
              nil)))
    (if idx (let((f (nth idx lookup-reference-functions)))
              (funcall (if f f (car lookup-reference-functions)) w))
      (let* ((funcs (get-mode-lookup-reference-functions))
             (p (car funcs))
             (s (cadr funcs)))
        (if (not secondary)
            (unless (funcall p w)
              (if s (funcall s w)))
          (if s (funcall s w)))))))

(defun lookup-reference-dwim-secondary()
  (interactive)
  (lookup-reference-dwim t))

(bind-key* "C-q" 'lookup-reference-dwim) ;; overrides major mode bindings
(bind-key* "C-S-q" 'lookup-reference-dwim-secondary)

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
      (devdocs-browser-open))))

(use-package dash-docs)

(use-package elfeed
  :config
  (use-package elfeed-org
    :ensure t
    :custom
    (rmh-elfeed-org-files (list (no-littering-expand-etc-file-name "elfeed/elfeed.org")))
    :config
    (elfeed-org))
  (use-package elfeed-goodies
    :disabled
    :config
    (elfeed-goodies/setup))
  (run-at-time nil (* 8 60 60) #'elfeed-update)
  :bind
  ( "C-c w" . elfeed)
  (:map elfeed-show-mode-map
        ("&" . (lambda()(interactive)(message "opening in eternal browser")(elfeed-show-visit t))))
  (:map elfeed-search-mode-map
        ("&" . (lambda()(interactive)(message "opening in eternal browser")(elfeed-search-browse-url t)))))



(use-package pdf-tools
  :demand t
  :config
  ;;(pdf-tools-install)
  (add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1)))
  (use-package org-pdftools
    :hook (org-mode . org-pdftools-setup-link)))

(use-package impatient-showdown
  :hook (markdown-mode . impatient-showdown-mode))

(provide 'rgr/reference)
