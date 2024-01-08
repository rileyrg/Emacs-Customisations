(defun rgr/region-symbol-query()
  "if a prefix argument (4)(C-u) read from input, else if we have a region select then return that and deselect the region, else try symbol-at-point and finally fallback to input"
  (let* ((w (if (or  (not current-prefix-arg) (not (listp current-prefix-arg)))
                (if(use-region-p)
                    (let ((sel-text
                           (buffer-substring-no-properties
                            (mark)
                            (point))))
                      sel-text)
                  (thing-at-point 'symbol)) nil))
         (result (if w w (read-string "lookup:"))))
    result))

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
  (defun rgr/complete-c-line()
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

(provide 'rgr/utils)
