(use-package emacs
  :init
  (setq thing-at-point-provider-alist (append thing-at-point-provider-alist
                                              '((rgr/tap-kdwim . rgr/tap-kill-dwim)
                                                (rgr/tap-region . rgr/get-region))))

  :config
  (defcustom rgr/kill-dwim-tap-symbols '(rgr/tap-region url filename email symbol sexp word line)
    "`thing-at-point' candidates for killing")

  (defun rgr/get-region()
    "return string in region if selected and deactivate, else nil"
    (if(use-region-p)
        (let ((txt (buffer-substring-no-properties
                    (mark)
                    (point))))
          (deactivate-mark)
          txt)
      nil))

  (defun rgr/tap-kill-dwim()
    (catch 'found
      (mapcar (lambda(x)
                (let ((v (thing-at-point x)))
                  (when v (throw 'found v)))) rgr/kill-dwim-tap-symbols)))

  (defun rgr/kill-dwim ()
    "work out what to pick up from point and stick in the kill ring"
    (interactive)
    (let ((s (thing-at-point 'rgr/tap-kdwim)))
      (if current-prefix-arg
          (setq s (read-string "text:" s)))
      (when s
        (message "%s" s)
        (kill-new s))))
  :bind
  ("M-w" . #'rgr/kill-dwim))

(provide 'rgr/kill-dwim)
