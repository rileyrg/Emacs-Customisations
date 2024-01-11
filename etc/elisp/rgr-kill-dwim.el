(use-package emacs

  :config

  (defun rgr/get-region()
    "return string in region if selected and deactivate, else nil"

    (if(use-region-p)
        (let ((txt (buffer-substring-no-properties
                    (mark)
                    (point))))
          (deactivate-mark)
          txt)
      nil))

  (defun rgr/thing-at-point-dwim()
    "if a prefix argument (4)(C-u) read from input, else if we have a region select then return that else... url,filename,symbol,sexp,word in that order"
    (if current-prefix-arg
        (read-string "text:")
      (or (rgr/get-region) (thing-at-point 'url) (thing-at-point 'filename) (thing-at-point 'symbol) (thing-at-point 'sexp) (thing-at-point 'word) )))

  (defun rgr/kill-dwim ()
    "work out what to pick up from point and stick in the kill ring"
    (interactive)
    (let ((s (rgr/thing-at-point-dwim)))
      (when s
        (message (format "'%s' saved to kill-ring" s))
        (kill-new s))))

  :bind
  ( "M-w" .  rgr/kill-dwim))

(provide 'rgr/kill-dwim)
