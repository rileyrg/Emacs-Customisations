(defun rgr/elisp-write-var (f v)
  (with-temp-file f
    (prin1 v (current-buffer))))

(defun rgr/elisp-read-var (f)
  (with-temp-buffer
    (insert-file-contents f)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

(defun c-complete-line()
  (interactive)
  (end-of-line)
  (unless (eql ?\; (char-after (- (point-at-eol) 1)))
    (progn (insert ";")))
  (newline-and-indent))
(defun c-insert-previous-line()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  (insert (string-trim (current-kill 0))))
(defun c-newline-below()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(provide 'rgr/utils)
