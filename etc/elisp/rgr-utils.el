(use-package gif-screencast
  :custom
  (gif-screencast-program "grim")
  (gif-screencast-screenshot-directory "~/tmp")
  (gif-screencast-output-directory "~/tmp"))

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
  (setq rgr/complete-line-function 'rgr/newline-below)
  :config
  (defun rgr/c-complete-line()
    (interactive)
    (end-of-line)
    (delete-trailing-whitespace)
    (unless (eql ?\; (char-before (point-at-eol)))
      (progn (insert ";")))
    (if current-prefix-arg
        (newline-and-indent)
      (progn
        (next-line)
        (beginning-of-visual-line))))

  (defun rgr/insert-previous-line()
    (interactive)
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    (insert (string-trim (current-kill 0))))

  (defun rgr/newline-below()
    (interactive)
    (end-of-line)
    (newline-and-indent))
  :bind
  ("<C-return>" . (lambda()(interactive)(funcall rgr/complete-line-function))))

(use-package lazy-lang-learn
  :straight (lazy-lang-learn :local-repo "~/development/projects/emacs/lazy-lang-learn" :type git :host github :repo "rileyrg/lazy-lang-learn" )
  :bind
  ("C-c L" . lazy-lang-learn-mode)
  ("<f12>" . lazy-lang-learn-translate)
  ("S-<f12>" . lazy-lang-learn-translate-from-history))

(provide 'rgr/utils)
