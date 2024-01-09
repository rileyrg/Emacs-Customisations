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

(use-package
  google-this
  :after org
  :custom
  (google-this-keybind "g")
  :config
  (google-this-mode 1))

(use-package go-translate
  ;;:disabled
  :custom
  (gts-translate-list '(("en" "de")))
  (gts-default-translator
   (gts-translator
    :picker (gts-prompt-picker)
    :engines (list (gts-bing-engine) (gts-google-engine))
    :render (gts-buffer-render)))
  :bind
  ("C-c T" . gts-do-translate))

(use-package google-translate
  :disabled ;; use go-translate
  :init
  (require 'google-translate)

  :custom
  (google-translate-backend-method 'curl)
  (google-translate-pop-up-buffer-set-focus t)
  :config

  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

  (defun google-translate-swap-default-languages()
    "swap google-translate default languages"
    (setq google-translate-default-source-language  (prog1 google-translate-default-target-language (setq google-translate-default-target-language  google-translate-default-source-language))))

  (defun rgr/google-translate-in-history-buffer(&optional phrase)
    (interactive)
    (when current-prefix-arg
      ;;swap source and dest languages
      (google-translate-swap-default-languages))
    (let  ((phrase (if phrase phrase (rgr/region-symbol-query))))
      (switch-to-buffer "*Google Translations*")
      ;; need to make aminor mode and do this properly based on file - org-mode?
      (local-set-key (kbd "<return>") (lambda() (interactive)
                                        (goto-char(point-max))
                                        (newline)))
      (unless (= (current-column) 0)
        (end-of-line)
        (newline))
      (insert  (format "<%s>: %s" (format-time-string "%Y-%m-%d %T") phrase))
      (rgr/google-translate-at-point)))

  (defun rgr/google-translate-at-point()
    "reverse translate word/region if prefix"
    (interactive)
    (when current-prefix-arg
      ;;swap source and dest languages
      (google-translate-swap-default-languages))
    (google-translate-at-point)
    (if google-translate-pop-up-buffer-set-focus
        (select-window (display-buffer "*Google Translate*"))))

  (defun rgr/google-translate-query-translate()
    "reverse translate input if prefix"
    (interactive)
    (when current-prefix-arg
      ;;swap source and dest languages
      (google-translate-swap-default-languages))
    (google-translate-query-translate)
    (if google-translate-pop-up-buffer-set-focus
        (select-window (display-buffer "*Google Translate*"))))

  :bind
  ("C-c T" . rgr/google-translate-at-point)
  ("C-c t" . rgr/google-translate-query-translate)
  ("C-c b" . rgr/google-translate-in-history-buffer))

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

(add-to-list 'Info-directory-list (expand-file-name "info" user-emacs-directory)) ;; https://www.emacswiki.org/emacs/ExternalDocumentation

(use-package devdocs-browser
  :custom
  (devdocs-browser-cache-directory (no-littering-expand-var-file-name  "devdocs-browser"))
  :config
  (defun rgr/devdocs(&optional i)
    (interactive)
    (if (or (derived-mode-p  'emacs-lisp-mode) (and (eq major-mode 'org-mode) (string= "emacs-lisp" (car (org-babel-get-src-block-info)))))
        (rgr/elisp-lookup-reference-dwim)
    (if current-prefix-arg
        (call-interactively 'devdocs-browser-open-in)
      (devdocs-browser-open))))
  :bind
  ("C-q" . rgr/devdocs))

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
