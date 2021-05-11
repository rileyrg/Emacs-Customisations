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

(provide 'rgr/utils)

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
