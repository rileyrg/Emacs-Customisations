(defcustom project-external-terminal-script "emacs-project-tmux"
  "script to launch your external terminal - all scripts called with project-root as parameter and optional project name"
  :type 'string
  :group 'project)

(defun project-launch-external-script(&optional script projname )
  "Two parameters passed to the script  : the project home directory and the project name"
  (interactive)
  (let*  ((root (project-root (project-current)))
          (pn (if projname  projname (file-name-nondirectory
                                      (directory-file-name
                                       (file-name-directory root)))))
          (cmd (concat  script " " root " " pn)))
    (call-process-shell-command cmd  nil 0)))

(define-key project-prefix-map "V" '("ext terminal" . (lambda()(interactive)(project-launch-external-script project-external-terminal-script))))
