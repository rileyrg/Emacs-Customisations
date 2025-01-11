;; look for a debug init file and load, trigger the debugger
(defun debug-init (&optional fname)
  (let* ((fname (if fname fname "debug-init.el"))
         (debug-init (expand-file-name fname user-emacs-directory)))
    (if (file-exists-p debug-init)
        (progn
          (message "A debug-init, %s, was found, so loading." debug-init)
          (let ((rgr/debug-init-debugger t)) ;; can set rgr/debug-init-debugger to false in the debug init to avoid triggering the debugger
            (load-file debug-init)
            (if rgr/debug-init-debugger
                (debug)
              (message " After loading %s `rgr/debug-init-debugger was set to nil so not debugging." debug-init))))
      (message "No debug initfile, %s, found so ignoring" debug-init))))
(debug-init)

(setq custom-file  (expand-file-name  "custom.el" user-emacs-directory)) ;;
(load custom-file 'noerror)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(defvar emacs-project-dir "~/development/projects/emacs" "personal elisp libraries" )

(use-package no-littering
  :ensure t
  :commands (no-littering-expand-var-file-name no-littering-expand-etc-file-name)
  :custom
  (make-backup-files t)
  :config
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (defvar elisp-dir (expand-file-name "elisp" no-littering-etc-directory) "my elisp directory. directories are recursively added to path.")
  (add-to-list 'load-path elisp-dir)
  (let ((default-directory elisp-dir))
    (normal-top-level-add-subdirs-to-load-path)))

(use-package notifications
  :demand t
  :config
  (notifications-notify
   :title "Emacs"
   :body " ... is starting up..."))



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

(require 'rgr/typesetting "rgr-typesetting" 'NOERROR)

(use-package lazy-lang-learn  :disabled t
  :straight (lazy-lang-learn :local-repo "~/development/projects/emacs/lazy-lang-learn" :type git :host github :repo "rileyrg/lazy-lang-learn" )
  :bind
  ("C-c L" . lazy-lang-learn-mode)
  ("<f12>" . lazy-lang-learn-translate)
  ("S-<f12>" . lazy-lang-learn-translate-from-history))

(require 'rgr/reference "rgr-reference" 'NOERROR)

(require 'rgr/shells "rgr-shells" 'NOERROR)

(require 'rgr/email "rgr-email" 'NOERROR)

(require 'rgr/chat "rgr-chat" 'NOERROR)

(require 'rgr/programming "rgr-programming" 'NOERROR)

(require 'rgr/elisp (expand-file-name "rgr-elisp" elisp-dir))

(require 'rgr/themes "rgr-themes" 'NOERROR)

(load-el-gpg (no-littering-expand-etc-file-name "late-load"))
