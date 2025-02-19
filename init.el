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

(defcustom rgr/emacs-project-dir "~/development/emacs" "personal elisp libraries" )

(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
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

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(setq custom-file  (expand-file-name  "var/secrets/custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; put extra emacs-lisp files into etc/elisp
(let ((default-directory rgr/elisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

(use-package no-littering
  :custom
  (make-backup-files t)
  :config
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(require 'notifications)
(defun emacs-alert(m &optional rest)
  (notifications-notify
   :title "Emacs"
   :body (format m rest)))

(defun rgr/quit-or-close-emacs(&optional kill)
  (interactive)
  (if (or current-prefix-arg kill)
      (rgr/server-shutdown)
    (delete-frame)))

(defun rgr/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-buffers-kill-emacs))

(global-set-key (kbd "C-c x")  'rgr/quit-or-close-emacs)

(setq rgr/daemonName (daemonp))
;; we dont always want a session to use/update history
(setq rgr/session-history t)

;; we always load general init unless daemon name begins with "_" , but if its not the command line choice or a non daemon, dont keep history etc.
(if (and rgr/daemonName (string-match-p "^_.*$" rgr/daemonName))
    (setq rgr/daemonName (substring rgr/daemonName 1))
  (load-file (rgr/user-elisp-file "init-general.el"))
  )

;; now load daemon specific init file
(if (and rgr/daemonName (not (string= rgr/daemonName "general")))
    (let((f (rgr/user-elisp-file (format "init-%s.el" rgr/daemonName))))
      (when (file-exists-p f)
        ;; default to turning off history : the init can override it
        (setq rgr/session-history nil)
        (emacs-alert (format "loading daemon init file : %s" f))
        (load-file  f))))

(if rgr/session-history
    (progn 
      (recentf-mode)
      (save-place-mode)
      (savehist-mode)))

(setq rgr/fileRestored nil)
(defun rgr/after-display()
  (if rgr/session-history
      (set-face-background 'mode-line "lightgreen")
    (set-face-background 'mode-line "orangered"))
  (when rgr/daemonName
    (set-frame-name (format "Emacs-%s" rgr/daemonName)))
  ;; (when (get-buffer "*elpaca-log*")
  ;;   (kill-buffer "*elpaca-log*"))
  (when (and rgr/session-history (not rgr/fileRestored))
    (setq rgr/fileRestored t)
    (run-with-idle-timer 0.3 nil (lambda()(switch-to-buffer (recentf-open-most-recent-file 1))))
    ))

(if rgr/daemonName
    (add-hook 'server-after-make-frame-hook #'rgr/after-display)
  (add-hook 'elpaca-after-init-hook 'rgr/after-display))
