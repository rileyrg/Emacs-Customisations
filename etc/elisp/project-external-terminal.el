(defcustom project-external-terminal-local-history 't
  "set to true to turn on project local history"
  :type 'boolean
  :group 'project)

(defcustom project-external-terminal "kitty"
  "name of external terminal to launch from a project"
  :type 'string
  :group 'project)

(defun project-external-terminal-launch-string(root) 
  "the terminal string to launch the terminal. It will usually  be prefixed by `project-external-terminal-history-prefix'. The tumx example here uses a socket -L per session to ensure own env"
  (let*  ((projname (file-name-nondirectory
                     (directory-file-name
                      (file-name-directory root))))
          (res (concat project-external-terminal " tmux -L " projname " new  -A -s \"project:" projname "\"")))
    res))


(defun project-external-terminal-history-prefix(root)
  "return a prefix to set env HISTFILE if `project-external-terminal-local-history' is set to true" 
  (if  project-external-terminal-local-history
      (concat "HISTFILE=\"" (expand-file-name ".project-history" root) "\"")
    nil))

(defun project-external-terminal-open()
  "open a terminal in the current project root. see `project-external-terminal-local-history' and `project-external-terminal-history-prefix'. The terminal launch string is created by `project-external-terminal-launch-string'"
  (interactive)
  (let* ((root (project-root (project-current)))
         (cmd (concat "cd " root " && " (project-external-terminal-history-prefix root) " " (project-external-terminal-launch-string root))))
    (message cmd)
    (call-process-shell-command cmd nil 0)))

(define-key project-prefix-map "V" '("ext terminal" .  project-external-terminal-open))
