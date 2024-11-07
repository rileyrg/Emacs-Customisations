;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
;; Maintained in emacs-config.org
(setq max-specpdl-size 13000)

(setq package-enabled-at-startup nil)

(defvar bootstrap-version)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;(straight-use-package `(use-package ,@(when (>= emacs-major-version 29) '(:type built-in))))

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-vc-git-default-protocol 'ssh))

(use-package notifications
  :demand t
  :config
  (notifications-notify
   :title "Emacs"
   :body " ... is starting up..."))

(use-package no-littering
  :custom
  (make-backup-files t)
  :init
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (when (boundp 'native-comp-eln-load-path)
    (startup-redirect-eln-cache (no-littering-expand-var-file-name "eln-cache"))))

(straight-use-package 'org)

;;; early-init.el ends here

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
