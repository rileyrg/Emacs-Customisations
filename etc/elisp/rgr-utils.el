(use-package project  :straight(:type built-in)
  :custom
  (project-vc-extra-root-markers '(".project"))
  (project-mode-line t)
  :config
  ;;(defvar rgr/project-url nil "project url to launch eg for cmake tutorial")
  (defun rgr/project-url(url)
    "launch url associated with this project 'rgr/project-url"
    (interactive (if (boundp 'rgr/project-url) `(,rgr/project-url) (list (read-string "url: "))))
    (eww url))
  (add-to-list  'project-switch-commands  '(multi-vterm-project "vterm" "v"))
  (add-to-list  'project-switch-commands  '(rgr/project-url "url" "u"))
 :bind(:map project-prefix-map
            ("v" . multi-vterm-project)
            ("u" . rgr/project-url)))

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

(use-package emacs
  :init
  (defvar rgr/complete-line-f 'rgr/newline-below "The fname called by `rgr/complete-line'")
  :config

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

  :bind
  ("<C-S-return>" . rgr/complete-line))

(use-package lazy-lang-learn
  :straight (lazy-lang-learn :local-repo "~/development/projects/emacs/lazy-lang-learn" :type git :host github :repo "rileyrg/lazy-lang-learn" )
  :bind
  ("C-c L" . lazy-lang-learn-mode)
  ("<f12>" . lazy-lang-learn-translate)
  ("S-<f12>" . lazy-lang-learn-translate-from-history))

(provide 'rgr/utils)
