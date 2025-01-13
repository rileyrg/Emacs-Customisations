(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".project"))
  ;;(project-mode-line t)
  :config
  (defun rgr/project-url(url)
    (interactive (if (boundp 'rgr/project-url) `(,rgr/project-url) (list (read-string "url: "))))
    (eww url))
  (define-key project-prefix-map "v" '("vterm" .  multi-vterm-project))
  (define-key project-prefix-map "u" '("project url" .  rgr/project-url)))

(defun rgr/toggle-buffer(n)
  "jump to or from buffer named n else default to *Messages*"
  (interactive "bbuffer:")
  (let ((n (or n
               "*Messages*")))
    (switch-to-buffer (if (string= (buffer-name) n)
                          (other-buffer) n))))

(defun rgr/elisp-write-var (f v)
  (with-temp-file f
    (prin1 v (current-buffer))))

(defun rgr/elisp-read-var (f)
  (with-temp-buffer
    (insert-file-contents f)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

(defvar rgr/complete-line-f 'rgr/newline-below "The fname called by `rgr/complete-line'")

(defun rgr/complete-line()
  (interactive)
  (funcall rgr/complete-line-f))

(defun rgr/c-complete-line()
  (end-of-line)
  (delete-trailing-whitespace)
  (unless (eql ?\; (char-before (point-at-eol)))
    (insert ";"))
  (newline-and-indent))

(defun rgr/insert-previous-line()
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  (insert (string-trim (current-kill 0))))

(defun rgr/newline-below()
  (end-of-line)
  (newline-and-indent))


(global-set-key (kbd "C-S-<return>")  'rgr/complete-line)

(setq lazyll-dir (expand-file-name "lazy-lang-learn" emacs-project-dir ))
(use-package lazy-lang-learn
  :ensure  `(:repo ,lazyll-dir)
  :bind
  ("C-c L" . lazy-lang-learn-mode)
  ("<f12>" . lazy-lang-learn-translate)
  ("S-<f12>" . lazy-lang-learn-translate-from-history))

(provide 'rgr/utils)
