(setq custom-file  (expand-file-name  "custom.el" user-emacs-directory)) ;;
(load custom-file 'noerror)

(debug-init)

;; look for a debug init file and load, trigger the debugger
(debug-init "debug-init-straight.el")

(defvar elisp-dir (expand-file-name "elisp" no-littering-etc-directory) "my elisp directory. directories are recursively added to path.")
(add-to-list 'load-path elisp-dir)
(let ((default-directory elisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

(defun load-el-gpg (load-dir)
  (message "attempting mass load from %s." load-dir)
  (when (file-exists-p load-dir)
    (dolist (f (directory-files-recursively load-dir "\.[el|gpg]$"))
      (condition-case nil
          (progn
            (message "load-el-gpg loading %s" f)
            (load f 'no-error))
        (error nil)))))
(load-el-gpg (no-littering-expand-etc-file-name "early-load"))

(load-el-gpg (expand-file-name (system-name)  (no-littering-expand-etc-file-name "hosts")))

(require 'rgr/security "rgr-security" 'NOERROR)

(require 'rgr/utils "rgr-utils" 'NOERROR)

(require 'rgr/startup "rgr-startup" 'NOERROR)

(require  'rgr/general-config "rgr-general-config" 'NOERROR)

(require 'rgr/minibuffer "rgr-minibuffer" 'NOERROR)

(require 'rgr/completion "rgr-completion" 'NOERROR)

(require 'rgr/org "rgr-org" 'NOERROR)

(use-package lazy-lang-learn
  :straight (lazy-lang-learn :local-repo "~/development/projects/emacs/lazy-lang-learn" :type git :host github :repo "rileyrg/lazy-lang-learn" )
  :bind
  ("C-c L" . lazy-lang-learn-mode)
  ("<f12>" . lazy-lang-learn-translate)
  ("S-<f12>" . lazy-lang-learn-translate-from-history))

(require 'rgr/reference "rgr-reference" 'NOERROR)

(use-package
  eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (defun rgr/projectile-term()
    (interactive)
    (if (string-equal major-mode "eat-mode")
        (previous-buffer)
      (let ((default-directory (projectile-project-root)))
        (eat))))

  :bind
  ("M-g v" . #'rgr/projectile-term))

(require 'rgr/email "rgr-email" 'NOERROR)

(require 'rgr/programming "rgr-programming" 'NOERROR)

(require 'rgr/themes "rgr-themes" 'NOERROR)

(load-el-gpg (no-littering-expand-etc-file-name "late-load"))
