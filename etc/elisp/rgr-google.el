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
    :render (gts-buffer-render))))

(use-package google-translate
  ;;:disabled
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

(provide 'rgr/google)
