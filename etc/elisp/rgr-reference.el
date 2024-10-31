(use-package gptel
  :bind
  ("C-c q" . #'gptel-send))

(use-package ellama
  :custom
  (ellama-sessions-directory (no-littering-expand-var-file-name "ellama-sessions"))
  :init
  (setopt ellama-language "German")
  (require 'llm-ollama))

(use-package eww
  :config
  ;; Advice EWW to launch certain URLs using the generic launcher rather than EWW.
  (defcustom rgr/eww-external-launch-url-chunks '("youtube")
    "If any component of this list is contained in an EWW url then it will use `browse-url-generic to launch that url instead of `eww"
    :type '(repeat string))
  (defadvice eww (around rgr/eww-extern-advise activate)
    "Use `browse-url-generic if any part of URL is contained in `rgr/eww-external-launch-url-chunks"
    (if (string-match-p (regexp-opt rgr/eww-external-launch-url-chunks) url)
        (progn
          (call-process-shell-command "swaymsg workspace number 2" nil 0)
          (browse-url-generic url))
      ad-do-it))
  (defun rgr/eww-after-render ()
    ;;move point line to top
    (condition-case err
        (dotimes (_ 2)
          (recenter-top-bottom))
      (error nil)))
  (defun rgr/eww-launch-external-browser-from-buffer()
    (interactive)
    (alert "Launching external browser")
    (call-process-shell-command "swaymsg workspace number 2" nil 0)
    (eww-browse-with-external-browser)
    (quit-window))
  :hook (eww-after-render . rgr/eww-after-render)
  :bind
  ("C-c o" . 'eww)
  (:map eww-mode-map
        ( "&" . rgr/eww-launch-external-browser-from-buffer)))

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
    (let  ((phrase (if phrase phrase (rgr/thing-at-point-dwim))))
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

(use-package emacs
  :init
  (defcustom rgr/browser-doc-url "https://www.google.com/search?q=%s" "format url variable used for function `rgr/browser-doc-search'")
  (defun rgr/browser-doc-search(&optional sym)
    "call function `browse-url' with a url variable `rgr/browser-doc-url' formatted with variable `sym'"
    (interactive
     (list
      (let((sym (replace-regexp-in-string  "^\\." "" (rgr/kill-dwim) )))
        (read-string (format "search(%s):" sym)
                     nil nil sym))))
     (browse-url (format rgr/browser-doc-url sym))))

(use-package
  dictionary
  :commands (rgr/dictionary-search)
  :custom
  (dictionary-server "dict.org")
  ;;(dictionary-server "localhost")
  :config
  (use-package mw-thesaurus)
  (defun rgr/dictionary-search(&optional w)
    (interactive)
    (dictionary-search (if w w (rgr/thing-at-point-dwim))))
  :bind
  ("<f6>" . rgr/dictionary-search)
  ("S-<f6>" . mw-thesaurus-lookup-dwim))

(use-package
  goldendict
  :commands (goldendict-dwim)
  :config
  (defun goldendict-dwim
      (&optional
       w)
    "lookup word at region, thing at point or prompt for something, in goldendict. Use a prefix to force prompting."
    (interactive)
    (let ((w (if w w (rgr/thing-at-point-dwim))))
      (call-process-shell-command (format  "goldendict \"%s\"" w ) nil 0)))
  :bind (("C-x G" . goldendict-dwim)))

(use-package dash-docs
  :disabled t
  :custom
  (dash-docs-common-docsets dash-docs-common-docsets)
  (dash-docs-docsets-path (no-littering-expand-var-file-name "dashdocs"))
  :config
  (defun rgr/dash-search(&optional s)
    (interactive)
    (let ((sym (if s s (thing-at-point 'symbol))))
      (message "dash docs search %s" sym)
      (dash-docs-search sym)))
  :bind
  ("C-S-a" . rgr/dash-search ))

(use-package devdocs-browser
  :custom
  (devdocs-data-dir (no-littering-expand-var-file-name  "devdocs-browser"))
  (devdocs-browser-cache-directory (no-littering-expand-var-file-name  "devdocs-browser/cache"))
  (devdocs-browser-data-directory (no-littering-expand-var-file-name  "devdocs-browser/data"))
  :config
  (defun rgr/devdocs()
    "If in an emacs-lisp buffer or bable block use `rgr/elisp-lookup-reference' else devdocs."
    (interactive)
    (if (or (derived-mode-p  'emacs-lisp-mode) (and (eq
                                                     major-mode 'org-mode) (string= "emacs-lisp" (car (org-babel-get-src-block-info)))))
        (rgr/emacs-lisp-help)
      (devdocs-browser-open)))
  :bind
  ("C-q" . rgr/devdocs))

(use-package elfeed
  :config
  (use-package elfeed-org
    :custom
    (rmh-elfeed-org-files (list (no-littering-expand-etc-file-name "elfeed/elfeed.org")))
    :config
    (elfeed-org))
  (run-at-time nil (* 8 60 60) #'elfeed-update)
  :bind
  ( "C-c w" . elfeed)
  (:map elfeed-show-mode-map
        ("b" . (lambda()(call-process-shell-command "swaymsg workspace number 2" nil 0)(interactive)(elfeed-show-visit t))))
  (:map elfeed-search-mode-map
        ("b" . (lambda()(call-process-shell-command "swaymsg workspace number 2" nil 0)(interactive)(elfeed-search-browse-url t)))))



(use-package pdf-tools
  :after (org-plus-contrib)
  :config
  (pdf-tools-install)
  (add-hook 'pdf-isearch-minor-mode-hook (lambda () ;; (ctrlf-local-mode -1)
                                           ))
  (use-package org-pdftools
    :hook (org-mode . org-pdftools-setup-link)))

(use-package impatient-showdown
  :disabled
  :hook (markdown-mode . impatient-showdown-mode))

(provide 'rgr/reference)
