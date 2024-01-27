(use-package emacs
   :init
   (setq thing-at-point-provider-alist (append thing-at-point-provider-alist
           '((kill-dwim . rgr/thing-at-point-kill-dwim))))

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

 (defun rgr/thing-at-point-kill-dwim()
   (or (rgr/get-region) (thing-at-point 'url) (thing-at-point 'filename) (thing-at-point 'email) (thing-at-point 'symbol) (thing-at-point 'sexp) (thing-at-point 'word) ))

 (defun rgr/kill-dwim ()
   "work out what to pick up from point and stick in the kill ring"
   (interactive)
   (let ((s (thing-at-point 'kill-dwim)))
     (condition-case nil
         (message s)
       (error nil))
     "if a prefix argument (4)(C-u) allow edit / read from input"
     (if current-prefix-arg
         (setq s (read-string "text:" s)))
     (when s
       (kill-new s))))
:bind
 ("M-w" . #'rgr/kill-dwim))

(provide 'rgr/kill-dwim)
