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

(defvar emacs-project-dir "~/development/projects/emacs" "personal elisp libraries" )

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
        ;; Enable use-package :ensure support for Elpaca.
        (elpaca-use-package-mode))

(use-package no-littering
  :ensure  (:wait t) :demand t
  :commands (no-littering-expand-var-file-name no-littering-expand-etc-file-name)
  :custom
  (make-backup-files t)
  :config
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq rgr/elisp-dir (no-littering-expand-etc-file-name "elisp"))
  (add-to-list 'load-path rgr/elisp-dir)
  (let ((default-directory rgr/elisp-dir))
    (normal-top-level-add-subdirs-to-load-path)))

(use-package notifications
  :ensure nil
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
(with-eval-after-load 'no-littering (load-el-gpg (no-littering-expand-etc-file-name "early-load")))

(with-eval-after-load 'no-littering  (load-el-gpg (expand-file-name (system-name)  (no-littering-expand-etc-file-name "hosts"))))

(require 'rgr/security "rgr-security" 'NOERROR)

(require 'rgr/utils "rgr-utils" 'NOERROR)

(require 'rgr/startup "rgr-startup" 'NOERROR)

(require  'rgr/general-config "rgr-general-config" 'NOERROR)

(require 'rgr/minibuffer "rgr-minibuffer" 'NOERROR)

(require 'rgr/completion "rgr-completion" 'NOERROR)

(require 'rgr/org "rgr-org" 'NOERROR)

(require 'rgr/typesetting "rgr-typesetting" 'NOERROR)

(require 'rgr/reference "rgr-reference" 'NOERROR)

(require 'rgr/shells "rgr-shells" 'NOERROR)

(require 'rgr/email "rgr-email" 'NOERROR)

(require 'rgr/chat "rgr-chat" 'NOERROR)

(require 'rgr/programming "rgr-programming" 'NOERROR)

(require 'rgr/elisp (expand-file-name "rgr-elisp" rgr/elisp-dir))

(require 'rgr/themes "rgr-themes" 'NOERROR)

(load-el-gpg (no-littering-expand-etc-file-name "late-load"))
