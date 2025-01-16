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

(setq custom-file  (expand-file-name  "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; put extra emacs-lisp files into etc/elisp
(setq rgr/elisp-dir (expand-file-name  "etc/elisp" user-emacs-directory))
(add-to-list 'load-path rgr/elisp-dir)
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
(defun emacs-alert(m)
  (notifications-notify
   :title "Emacs"
   :body m))

(defun load-el-gpg (load-dir)
  (message "attempting mass load from %s." load-dir)
  (when (file-exists-p load-dir)
    (dolist (f (directory-files-recursively load-dir "\.[el|gpg]$"))
      (condition-case nil
          (progn
            (message "load-el-gpg loading %s" f)
            (load f 'no-error))
        (error nil)))))
(load-el-gpg (expand-file-name "etc/early-load" user-emacs-directory))

(load-el-gpg (expand-file-name (system-name)  (expand-file-name "etc/hosts" user-emacs-directory)))

(require 'auth-source)
(setq auth-sources '("~/.gnupg/auth/authinfo.gpg" "~/.gnupg/auth/authirc.gpg"))
(defun get-auth-info (host user &optional port)
  "Interface to `auth-source-search' to fetch a secret for the HOST and USER."
  (let* ((info (nth 0 (auth-source-search
                       :host host
                       :user user
                       :port port
                       :require '(:user :secret)
                       :create nil)))
         (secret (plist-get info :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(use-package modus-themes
                                        ;:disabled
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  ;; (modus-themes-load-themes)
  :config
  (load-theme 'modus-operandi :no-confirm))
;; (modus-themes-load-vivendi))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(defun rgr/erc-session()
  (string= "erc" (daemonp)))

(defun rgr/user-elisp-file(f)
  (expand-file-name f rgr/elisp-dir))

;; trying to set title
(when (daemonp)
  (add-to-list
   'elpaca-after-init-hook
     (lambda()
       (message "setting frame title to %s" (format "Emacs-%s" (daemonp)))
       (modify-frame-parameters
               nil
               (list (cons 'name (format "Emacs-%s" (daemonp))))))))

(defun rgr/init-file()
  (if (daemonp)
      (rgr/user-elisp-file (format "init-%s.el" (daemonp)))
    (rgr/user-elisp-file "init-general.el")))
(message "Loading %s init file" (rgr/init-file))
(load-file (rgr/init-file))

(require 'erc)
(defun rgr/erc-switch-to-channel(&optional channel)
  (when (string= (or channel "#emacs") (buffer-name (current-buffer)))
    (switch-to-buffer (current-buffer))))

(defun rgr/erc-start()
  (interactive)
  (define-key erc-mode-map (kbd "C-c C-q") 'rgr/erc-quit )
  (when (rgr/erc-session)
    (global-set-key (kbd "C-x b") 'erc-switch-to-buffer)
    (global-set-key (kbd "C-c x")  'rgr/erc-quit)
    (setq kill-emacs-hook nil))
  (if(get-buffer "Libera.Chat")
      (progn
        (emacs-alert "Switching to IRC...")
        (rgr/erc-switch-to-channel))
    (progn
      (emacs-alert "IRC Starting...")
      (erc-tls :server "irc.libera.chat" :port 6697)
      (add-hook 'erc-join-hook 'rgr/erc-switch-to-channel))))

(defun rgr/erc-quit()
  (interactive)
  (erc-quit-server "")
  (when (rgr/erc-session)
    (kill-emacs)))

(rgr/erc-start)
