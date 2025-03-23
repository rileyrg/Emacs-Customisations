

# Introduction

Emacs customisation generates [init.el](init.el) and other [emacs elisp utility
files](etc/elisp/)  using [org-babel-tangle](https://orgmode.org/manual/Extracting-Source-Code.html). 


## Own libraries

These libraries are seperate stand alone github libraries. 


### el-docstring-sap

Provides docstring help for symbol at point.

[https://github.com/rileyrg/el-docstring-sap](https://github.com/rileyrg/el-docstring-sap)


### lazy-lang-learn

A small "game" like utility that displays snippets to glance at. You
can then invoke google translate on them. Stores history.

<https://github.com/rileyrg/lazy-lang-learn>


# early-init

Emacs early-init
<https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html>


## redirect emacs cache

    ;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
    ;; Maintained in emacs-config.org
    (when (boundp 'native-comp-eln-load-path)
      (startup-redirect-eln-cache "var/eln-cache"))
    (setq max-specpdl-size 13000)


## elpaca

    (setq package-enable-at-startup nil)


## elisp locations

    (defcustom rgr/elisp-dir (expand-file-name  "etc/elisp" user-emacs-directory) "Where user elisp files should be stored")
    (defun rgr/user-elisp-file(f)
      (expand-file-name f rgr/elisp-dir))
    (setq load-path (cons rgr/elisp-dir load-path))


# emacs main init


## debug init

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


## package management


### own projects

    (defcustom rgr/emacs-project-dir "~/development/emacs" "personal elisp libraries" )


### elpaca

    (defvar elpaca-installer-version 0.10)
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
        (when (<= emacs-major-version 28) (require 'subr-x))
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


### elpaca use-package support

    (elpaca elpaca-use-package
      ;; Enable use-package :ensure support for Elpaca.
      (elpaca-use-package-mode))


## custom.el

    (setq custom-file  (expand-file-name  "var/secrets/custom.el" user-emacs-directory))
    (load custom-file 'noerror)


## no-littering - keep data tidy

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


## notifications

    (require 'notifications)
    (defun emacs-alert(m &optional rest)
      (notifications-notify
       :title "Emacs"
       :body (format m rest)))


## quit emacs

    
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


## load  inits

    
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


# general init


## startup

    (emacs-alert " ... is starting up..." )


## Load early stuff / gpg

Load all files in certain directories.

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


### host specific

:ID:       efe9afb5-3779-407d-a7a9-fd8968ea0f69

Stick a custom in here. eg my thinkpad [custom file](./etc/hosts/thinkpadx270/custom.el).

    (load-el-gpg (expand-file-name (system-name)  (expand-file-name "etc/hosts" user-emacs-directory)))


## Security


### Auth-Sources, get-auth-info

Let emacs take care of security things automagically.
example:

    (setq passw (get-auth-info "licenses" "my-auth-token"))

    (require 'auth-source)
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


### Pass

Uses the unix command line `pass` utility. Can be used via `process-lines`  e.g

    ;;(car (process-lines "pass" "Chat/slack-api-token"))


## themes


### modus themes

<https://github.com/protesilaos/modus-themes>

    (use-package modus-themes
      :config
      (load-theme 'modus-operandi :no-confirm))


## Org functionality

General org-mode config


<a id="orge8a8ed3"></a>

### Org Mode, org-mode

    ;;(push (expand-file-name "builds/org" elpaca-directory) load-path)
    (use-package org
      :custom
      ;;    (org-agenda-files (append (directory-files-recursively "~/.emacs.d/var/org/orgfiles" "^[[:alnum:]].*\\.org\\'") (directory-files-recursively "~/development/projects" "^[[:alnum:]].*\\.org\\'")))
      (org-fontify-done-headline t)
      (org-fontify-todo-headline t)
      (org-clock-idle-time 10)
      (org-babel-default-header-args:python
       '((:results  . "output")))
      (org-export-with-broken-links t)
      (org-refile-use-outline-path 'file)
      (org-outline-path-complete-in-steps nil)
      :config
      (set-face-attribute 'org-headline-done nil :strike-through t)
      (defun rgr/org-agenda (&optional arg)
        (interactive "P")
        (let ((org-agenda-tag-filter-preset '("-trash")))
          (org-agenda arg "a")))
      (require 'org-crypt)
      (org-crypt-use-before-save-magic)
      :hook
      (org-mode .  turn-on-auto-fill)
      :bind
      ("C-c a" . org-agenda)
      ("C-c A" . rgr/org-agenda)
      ("C-c c" . org-capture)
      ("C-c l" . org-store-link)
      ("C-c C-l" . org-insert-link)
      ("C-c C-s" . org-schedule)
      ("C-c C-t" . org-todo)
      ("C-c C-x C-j" . org-clock-goto)
      (:map org-mode-map
            ("M-." . find-function-at-point)))


### org agenda files

See `org-agenda-files` [org-agenda-files](#orge8a8ed3)
maintain a file pointing to agenda sources : NOTE, NOT tangled. ((no-littering-expand-etc-file-name "org/agenda-files.txt"))

    ~/.emacs.d/var/org/orgfiles
    ~/.emacs.d/var/org/orgfiles/journals
    ~/.emacs.d/var/org/orgfiles/projects
    ~/development/education/lessons
    ~/development/education/lessons/bash
    ~/development/education/lessons/python
    ~/development/education/lessons/python/coreyschafer
    ~/development/education/lessons/python/python-lernen.de
    ~/development/education/lessons/elisp


## Project Management


### use emacs project package

    (use-package project
      :ensure t
      :after (org)
      :custom
      (project-vc-extra-root-markers '(".project"))
      :config
      (define-key project-prefix-map "v" '("vterm" .  multi-vterm-project))
      (define-key project-prefix-map "u" '("project url" .  rgr/project-url)))


### add project based TODO

    (setq load-path (cons (expand-file-name "project-org-todo-capture" rgr/emacs-project-dir ) load-path))
    (use-package project-org-todo-capture
      :after org
      :ensure nil
      :init
      (require 'org-capture)
      :bind (:map project-prefix-map
                  (("t" . project-org-todo-capture)
                  ("T" . project-org-todo-capture-file-open))))


## toggle buffer

    (defun rgr/toggle-buffer(n)
      "jump to or from buffer named n else default to *Messages*"
      (interactive "bbuffer:")
      (let ((n (or n
                   "*Messages*")))
        (switch-to-buffer (if (string= (buffer-name) n)
                              (other-buffer) n))))


## read and write elisp vars to file

    
    (defun rgr/elisp-write-var (f v)
      (with-temp-file f
        (prin1 v (current-buffer))))
    
    (defun rgr/elisp-read-var (f)
      (with-temp-buffer
        (insert-file-contents f)
        (cl-assert (eq (point) (point-min)))
        (read (current-buffer))))


## completing lines

    (defvar rgr/complete-line-f 'rgr/newline-below "The fname called by `rgr/complete-line'")
    
    (defun rgr/complete-line()
      (interactive)
      (funcall rgr/complete-line-f))
    
    (defun rgr/c-complete-line()
      (end-of-line)
      (delete-trailing-whitespace)
      (unless (eql ?\; (char-before (point-at-eol)))
        (insert ";"))
      (newline-and-indent))
    
    (defun rgr/insert-previous-line()
      (previous-line)
      (end-of-line)
      (newline-and-indent)
      (insert (string-trim (current-kill 0))))
    
    (defun rgr/newline-below()
      (end-of-line)
      (newline-and-indent))
    
    
    (global-set-key (kbd "C-S-<return>")  'rgr/complete-line)


## Lazy Language Learning, lazy-lang-learn

My own hack for popping up text to learn

    (setq load-path (cons (expand-file-name "lazy-lang-learn" rgr/emacs-project-dir ) load-path))
    (use-package lazy-lang-learn
      :ensure  nil
      :bind
      ("C-c L" . lazy-lang-learn-mode)
      ("<f12>" . lazy-lang-learn-translate)
      ("S-<f12>" . lazy-lang-learn-translate-from-history))


## Minibuffer Enrichment (search, sudo edit&#x2026;)

Various plugins for minibuffer enrichment


### [sudo-edit](https://github.com/nflath/sudo-edit) Priviliged file editing

    (use-package sudo-edit)


### find file at point

    (use-package ffap
      :ensure nil
      :custom
      (ffap-require-prefix nil)
      :init
      (ffap-bindings)
      (defun rgr/ffap()
        (interactive)
        (let ((url (ffap-url-at-point)))
          (if (and url current-prefix-arg)
              (progn
                (notifications-notify
                 :title "Emacs"
                 :body "opening url in generic browser")
                (browse-url-generic url))
            (call-interactively 'find-file-at-point))))
      :bind
      ( "C-x C-f" . rgr/ffap))


### eldoc

    (unload-feature 'eldoc t)
    (setq custom-delayed-init-variables '())
    (elpaca eldoc
      (require 'eldoc)
      (global-eldoc-mode)
      (defun rgr/eldoc-at-point()
        (interactive)
        (if eldoc-mode
            (eldoc-box-help-at-point)
          (message "eldoc not active")))
      (global-set-key (kbd "C-.")  'rgr/eldoc-at-point))
    (use-package eldoc-box
      :after eldoc)


### Consult

1.  Consult Core

    Consult]] Provides various commands based on the Emacs completion function completing-read
    
    :ID:       ec5375c7-4387-42a1-8938-5fad532be79b
    
        ;; Example configuration for Consult
        ;; Example configuration for Consult
        (use-package consult
          ;; Replace bindings. Lazily loaded by `use-package'.
          :bind (
                 ;; C-c bindings in `mode-specific-map'
                 ("C-c M-x" . consult-mode-command)
                 ("C-c h" . consult-history)
                 ("C-c k" . consult-kmacro)
                 ("C-c m" . consult-man)
                 ("C-c i" . consult-info)
                 ([remap Info-search] . consult-info)
                 ;; C-x bindings in `ctl-x-map'
                 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
                 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
                 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
                 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
                 ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
                 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
                 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
                 ;; Custom M-# bindings for fast register access
                 ("M-#" . consult-register-load)
                 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
                 ("C-M-#" . consult-register)
                 ;; Other custom bindings
                 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
                 ;; M-g bindings in `goto-map'
                 ("M-g e" . consult-compile-error)
                 ("M-g f" . consult-flymake)               ;; Alternative: consult-flymake
                 ("M-g g" . consult-goto-line)             ;; orig. goto-line
                 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
                 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
                 ("M-g m" . consult-mark)
                 ("M-g k" . consult-global-mark)
                 ("M-g i" . consult-imenu)
                 ("M-g I" . consult-imenu-multi)
                 ;; M-s bindings in `search-map'
                 ("M-s d" . consult-find)                  ;; Alternative: consult-fd
                 ("M-s c" . consult-locate)
                 ("M-s i" . consult-info)
                 ("M-s g" . consult-grep)
                 ("M-s G" . consult-git-grep)
                 ("M-s r" . consult-ripgrep)
                 ("M-s l" . consult-line)
                 ("M-s L" . consult-line-multi)
                 ("M-s k" . consult-keep-lines)
                 ("M-s u" . consult-focus-lines)
                 ;; Isearch integration
                 ("M-s e" . consult-isearch-history)
                 :map isearch-mode-map
                 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
                 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
                 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
                 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
                 ;; Minibuffer history
                 :map minibuffer-local-map
                 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
                 ("M-r" . consult-history))
          ;; orig. previous-matching-history-element
        
          ;; Enable automatic preview at point in the *Completions* buffer. This is
          ;; relevant when you use the default completion UI.
          :hook (completion-list-mode . consult-preview-at-point-mode)
        
          ;; The :init configuration is always executed (Not lazy)
          :init
        
          ;; Optionally configure the register formatting. This improves the register
          ;; preview for `consult-register', `consult-register-load',
          ;; `consult-register-store' and the Emacs built-ins.
          (setq register-preview-delay 0.5
                register-preview-function #'consult-register-format)
        
          ;; Optionally tweak the register preview window.
          ;; This adds thin lines, sorting and hides the mode line of the window.
          (advice-add #'register-preview :override #'consult-register-window)
        
          ;; Use Consult to select xref locations with preview
          (setq xref-show-xrefs-function #'consult-xref
                xref-show-definitions-function #'consult-xref)
        
        
          ;; Configure other variables and modes in the :config section,
          ;; after lazily loading the package.
          :config
        
          ;; Optionally configure preview. The default value
          ;; is 'any, such that any key triggers the preview.
          ;; (setq consult-preview-key 'any)
          ;; (setq consult-preview-key "M-.")
          ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
          ;; For some commands and buffer sources it is useful to configure the
          ;; :preview-key on a per-command basis using the `consult-customize' macro.
          (consult-customize
           consult-theme :preview-key '(:debounce 0.2 any)
           consult-ripgrep consult-git-grep consult-grep
           consult-bookmark consult-recent-file consult-xref
           consult--source-bookmark consult--source-file-register
           consult--source-recent-file consult--source-project-recent-file
           ;; :preview-key "M-."
           :preview-key '(:debounce 0.4 any))
        
          ;; Optionally configure the narrowing key.
          ;; Both < and C-+ work reasonably well.
          (setq consult-narrow-key "<") ;; "C-+"
        
          ;; Optionally make narrowing help available in the minibuffer.
          ;; You may want to use `embark-prefix-help-command' or which-key instead.
          ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
          )

2.  consult-omni

        (use-package consult-omni
          :disabled t ;; https://github.com/armindarvish/consult-omni/issues/47
              :ensure  (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el"))
              :after consult)


### stillness mode

    (use-package stillness-mode
      :ensure '( :host github :repo "neeasade/stillness-mode.el" :branch "main")
      :config
      (stillness-mode 1)) 


### [Marginalia](https://en.wikipedia.org/wiki/Marginalia) margin annotations for info on line

are marks or annotations placed at the margin of the page of a book or in this case helpful colorful annotations placed at the margin of the minibuffer for your completion candidates

:ID:       f3802197-c745-40b2-ac0d-72d7da291aaf

    ;; Enable rich annotations using the Marginalia package
    (use-package marginalia
      ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
      ;; available in the *Completions* buffer, add it to the
      ;; `completion-list-mode-map'.
      :bind (:map minibuffer-local-map
             ("M-m" . marginalia-cycle))
    
      ;; The :init section is always executed.
      :init
      ;; Marginalia must be activated in the :init section of use-package such that
      ;; the mode gets enabled right away. Note that this forces loading the
      ;; package.
      (marginalia-mode))


### embark

    (use-package embark
      :ensure t
    
      :bind
      (("C-," . embark-act)         ;; pick some comfortable binding
       ("C-:" . embark-act-all)
       ("C-;" . embark-dwim)        ;; good alternative: M-.
       ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
    
      :init
    
      ;; Optionally replace the key help with a completing-read interface
      (setq prefix-help-command #'embark-prefix-help-command)
    
      ;; Show the Embark target at point via Eldoc. You may adjust the
      ;; Eldoc strategy, if you want to see the documentation from
      ;; multiple providers. Beware that using this can be a little
      ;; jarring since the message shown in the minibuffer can be more
      ;; than one line, causing the modeline to move up and down:
    
      ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
      ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
    
      :config
    
      ;; Hide the mode line of the Embark live/completions buffers
      (add-to-list 'display-buffer-alist
                   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                     nil
                     (window-parameters (mode-line-format . none)))))
    
    ;; Consult users will also want the embark-consult package.
    (use-package embark-consult
      :demand t ; only need to install it, embark loads it after consult if found
      :hook
      (embark-collect-mode . consult-preview-at-point-mode))


### all-the-icons

Remember to run **all-the-icons-install-fonts**.

    (use-package all-the-icons
      :demand
      :after (marginalia)
      :config
      (use-package all-the-icons-completion
        :init (all-the-icons-completion-mode))
      (use-package all-the-icons-dired
        :config
        (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
      :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))


## Completion

Let emacs suggest completions


### corfu

    (use-package corfu
      ;;:disabled t
      ;; Optional customizations
      :custom
      ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
      (corfu-auto t)                 ;; Enable auto completion
      ;;(corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
      (corfu-quit-no-match t)      ;; Never quit, even if there is no match
      (corfu-popupinfo-delay (cons nil 1.0))
      (corfu-preview-current t)    ;; Disable current candidate preview
      ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
      ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    
      ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
      ;; :hook ((prog-mode . corfu-mode)
      ;;        (shell-mode . corfu-mode)
      ;;        (eshell-mode . corfu-mode))
    
      ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
      ;; be used globally (M-/).  See also the customization variable
      ;; `global-corfu-modes' to exclude certain modes.
      ;; A few more useful configurations...
      ;;TAB cycle if there are only few candidates
      ;; (completion-cycle-threshold 3)
    
      ;; Enable indentation+completion using the TAB key.
      ;; `completion-at-point' is often bound to M-TAB.
      (tab-always-indent 'complete)
    
      ;; Emacs 30 and newer: Disable Ispell completion function.
      ;; Try `cape-dict' as an alternative.
      (text-mode-ispell-word-completion nil)
    
      ;; Hide commands in M-x which do not apply to the current mode.  Corfu
      ;; commands are hidden, since they are not used via M-x. This setting is
      ;; useful beyond Corfu.
      (read-extended-command-predicate #'command-completion-default-include-p)
    :init
    (global-corfu-mode)
    (corfu-popupinfo-mode))
    
    ;; Optionally use the `orderless' completion style.
    (use-package orderless
      :custom
      ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
      ;; (orderless-component-separator #'orderless-escapable-split-on-space)
      (completion-styles '(orderless basic))
      (completion-category-defaults nil)
      (completion-category-overrides '((file (styles partial-completion)))))
    
    
    ;; Use Dabbrev with Corfu!
    (use-package dabbrev
      :ensure nil
      ;; Swap M-/ and C-M-/
      :bind (("M-/" . dabbrev-completion)
             ("C-M-/" . dabbrev-expand))
      :config
      (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
      ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
      (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
      (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
      (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))
    
    ;; Add extensions
    (use-package cape
      ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
      ;; Press C-c p ? to for help.
      :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
      ;; Alternatively bind Cape commands individually.
      ;; :bind (("C-c p d" . cape-dabbrev)
      ;;        ("C-c p h" . cape-history)
      ;;        ("C-c p f" . cape-file)
      ;;        ...)
      :init
      ;; Add to the global default value of `completion-at-point-functions' which is
      ;; used by `completion-at-point'.  The order of the functions matters, the
      ;; first function returning a result wins.  Note that the list of buffer-local
      ;; completion functions takes precedence over the global list.
      (add-hook 'completion-at-point-functions #'cape-dabbrev)
      (add-hook 'completion-at-point-functions #'cape-file)
      (add-hook 'completion-at-point-functions #'cape-elisp-block)
      ;; (add-hook 'completion-at-point-functions #'cape-history)
      ;; ...
      )


### Which Key

[which-key](https://github.com/justbur/emacs-which-key) shows you what further key options you have if you pause on a multi key command.

    (use-package  which-key
      :ensure nil
      :demand t
      :config (which-key-mode))


### Yasnippet

[YASnippet](https://github.com/joaotavora/yasnippet)  is a template system for Emacs.
Note that eglot 1.4 auto enables snippets so no need to yas-minor or global mode

    (use-package yasnippet
      :config
      (use-package yasnippet-snippets)
      :init
      (yas-global-mode)
      )


### Abbrev Mode

[Abbrev Mode](https://www.emacswiki.org/emacs/AbbrevMode#toc4) is very useful for expanding small text snippets

    (setq-default abbrev-mode 1)
    ;;  (load-file abbrev-file-name)
    (defadvice expand-abbrev (after my-expand-abbrev activate)
      ;; if there was an expansion
      (if ad-return-value
          ;; start idle timer to ensure insertion of abbrev activator
          ;; character (e.g. space) is finished
          (run-with-idle-timer 0 nil
                               (lambda ()
                                 ;; if there is the string "%CHANGEME%" in the
                                 ;; expansion then move cursor there and
                                 ;; delete the string
                                 (let ((cursor "%CHANGEME%"))
                                   (when (search-backward  cursor last-abbrev-location t)
                                     (goto-char  last-abbrev-location)
                                     (search-forward cursor)
                                     (backward-word)
                                     (highlight-symbol-at-point)
                                     (delete-char (length cursor))
                                     ))))))


### company

    (use-package company
      :disabled t
      :config
      (use-package company-box
        :config
        (setf (alist-get 'internal-border-width company-box-doc-frame-parameters) 1)
        :hook (company-mode . company-box-mode))
      :hook
      (prog-mode . company-mode)
      :bind( :map company-mode-map
             ("<tab>" .  company-indent-or-complete-common)))


### vertical interactive completion

<https://github.com/minad/vertico>

    ;; Enable vertico
    (use-package vertico
      ;;:disabled
      :custom
      (vertico-cycle t)
      :config
      (vertico-mode))


## Typesetting

Need to install dvipng, texlive, texlive-latex-extra


### auctex

      (use-package auctex
        :disabled t
        :init
        (require 'ox-latex)
        ;;(use-package lsp-latex)
        :custom
        (TeX-auto-save t)
        (TeX-parse-self t)
        (TeX-master nil)
        (TeX-PDF-mode t)
    ;;    (org-preview-latex-default-process 'dvipng)
        :config
        (defun rgr/latex-mode-hook()
          ;; buggy as this then gets savd to the global abbrevs
          (load-file (expand-file-name  "etc/abbrev/latex-songbook-chords.el" user-emacs-directory ))
          (setq abbrevs-changed nil)
          (turn-on-reftex)
          (visual-line-mode)
          (LaTeX-math-mode)
          )
    
        :hook
        (TeX-mode .
                  (lambda () (rgr/latex-mode-hook)(TeX-fold-mode 1)))); Automatically activate TeX-fold-mode.


### lilypond

<https://lilypond.org/doc/v2.23/Documentation/usage/text-editor-support>

    (use-package
      lilypond
      :disabled)

1.  example


## Reference/Lookup/Media

lookup and reference uilities and config


### chatgpt

1.  GPTel

    <https://github.com/karthink/gptel>
    
        (use-package gptel
          :bind
          ("C-c q" . #'gptel-send))
    
    1.  secret openAPI key
    
        &#x2013;&#x2014;BEGIN PGP MESSAGE&#x2013;&#x2014;
        
        hQEMA7IjL5SkHG4iAQf+N61j3/62smGNrhRPOrE3U8JMpJlcDf7d0gukqvXXYD3r
        A0t7kLn3T4t0zfwDq/TvXRXhdB+IMkkB5vK+A4HGQv9jPuVGzoalYvzGlJpSGw2O
        /Q6EHYmtms31r71LCFYMDg+8FsVFN+l43QRCOeFmJNv+paEbB0xH9xzrpaIqUQV3
        LwBrriEfRo0PPwq3164HT/cjdzGLGLchSsBbfbXfAzN/JnN2olaHxiz+6kL+IWvZ
        kI9IRBhOFnPigbQ+QXzni0JimSavFE/qTaPpiFIh+PUp8ETt2TcjarzrpLetk8lA
        fIhRyWuu3+GTDuc5iASXGcGKHMusNE+xC20FNcwWDdKbAd2N2BRg2gwO/jzlm7Gc
        l8/YEKMfbaubRX/hG2qOV8OnS+Io8cZx9cN3L7X6QbCzkW0KPwMt2O+CNYvKADFU
        F4czehuTpGF0+QVLOpzgvOzPHZdbtiFiH47PdN/rXlHpwAX0cqhpe8WPAYBzNd22
        DvWoHsRLGhCVC8YTZrhVs48WbzJxHKBEnoswqlEk4534cIiL7NbwhlJPWg8=
        =UHS1
        &#x2013;&#x2014;END PGP MESSAGE&#x2013;&#x2014;


### Ellama

<https://github.com/s-kostyaev/ellama>
<https://github.com/s-kostyaev/ellama>
<https://ollama.ai/library/zephyr>

    (use-package ellama
      :custom
      (ellama-sessions-directory (expand-file-name  "var/ellama-sessions/" user-emacs-directory))
      :init
      (setopt ellama-language "German")
      (require 'llm-ollama))


### web lookup/view

1.  eww

        (use-package eww
          :ensure nil
          :config
          ;; Advice EWW to launch certain URLs using the generic launcher rather than EWW.
          (defcustom rgr/eww-external-launch-url-chunks '("youtube")
            "If any component of this list is contained in an EWW url then it will use `browse-url-generic to launch that url instead of `eww"
            :type '(repeat string))
          (defadvice eww (around rgr/eww-extern-advise activate)
            "Use `browse-url-generic if any part of URL is contained in `rgr/eww-external-launch-url-chunks"
            (if (string-match-p (regexp-opt rgr/eww-external-launch-url-chunks) url)
                (progn
                  (call-process-shell-command "swaymsg workspace number 2" nil 0)
                  (browse-url-generic url))
              ad-do-it))
          (defun rgr/eww-after-render ()
            ;;move point line to top
            (condition-case err
                (dotimes (_ 2)
                  (recenter-top-bottom))
              (error nil)))
          (defun rgr/eww-launch-external-browser-from-buffer()
            (interactive)
            (emacs-alert "Launching external browser")
            (call-process-shell-command "swaymsg workspace number 2" nil 0)
            (eww-browse-with-external-browser)
            (quit-window))
          :custom
          (eww-history-limit 256)
          :hook (eww-after-render . rgr/eww-after-render)
          :bind
          (:map eww-mode-map
                ( "&" . rgr/eww-launch-external-browser-from-buffer)))

2.  go-translate

    This is a translation framework for emacs, and is flexible and powerful.
    
        (use-package go-translate
          ;;:disabled
          :custom
          (gt-langs '("en" "de"))
          (gt-default-translator
           (gt-translator
            :taker   (gt-taker :text 'buffer :pick 'paragraph)  ; config the Taker
            :engines (list (gt-bing-engine) (gt-google-engine)) ; specify the Engines
            :render  (gt-buffer-render)))                       ; config the Render
          :bind
          ("C-c T" . gt-do-translate))
        
        ;; This configuration means:
        ;; Initialize the default translator, let it send all paragraphs in the buffer to Bing and Google,
        ;; and output the results with a new Buffer.

3.  utility funcs

    A bit of a dogs dinner.


### rgr/open-buffer-file-in external-browser

    (defun rgr/open-buffer-file-in-external-browser()
      (interactive)
      (eww-browse-with-external-browser buffer-file-name)
      (quit-window))


### use browser for docs

    (defcustom rgr/browser-doc-url "https://www.google.com/search?q=%s" "format url variable used for function `rgr/browser-doc-search'")
    (defun rgr/browser-doc-search(&optional sym)
      "call function `browse-url' with a url variable `rgr/browser-doc-url' formatted with variable `sym'"
      (interactive
       (list
        (let((sym (replace-regexp-in-string  "^\\." "" (kill-dwim) )))
          (read-string (format "search(%s):" sym)
                       nil nil sym))))
      (browse-url (format rgr/browser-doc-url sym)))


### natural vernacular reference

1.  Define word

        (use-package define-word 
          :bind
          ("C-<f6>" . define-word-at-point))

2.  Dictionary dictd

    The more emacsy [Dictionary](https://melpa.org/#/dictionary) .
    
        (use-package
          dictionary
          :ensure nil
          :custom
          ;;(dictionary-server "dict.org")
          (dictionary-server "localhost")
          :bind
          ("<f6>" . dictionary-lookup-definition)) 

3.  Thesaurus

    1.  mw-thesaurus
    
            (use-package mw-thesaurus
              :bind
              ("S-<f7>" . mw-thesaurus-lookup-dwim))
    
    2.  powerthesaurus
    
            (use-package powerthesaurus
              :bind
              ("S-<f6>" . powerthesaurus-lookup-word-dwim))


### GoldenDict - external lookup and reference

When using goldendict-dwim why not add your program to the wonderful
[GoldenDict](http://goldendict.org/)? A call to [trans-shell](https://github.com/soimort/translate-shell) in the dictionary  programs tab
gives us google translate:-

    trans -e google -s de -t en -show-original y -show-original-phonetics n -show-translation y -no-ansi -show-translation-phonetics n -show-prompt-message n -show-languages y -show-original-dictionary n -show-dictionary n -show-alternatives n "%GDWORD%"

    (use-package
      goldendict
      :disabled t
      :commands (goldendict-dwim)
      :config
      (defun goldendict-dwim
          (&optional
           w)
        "lookup word at region, thing at point or prompt for something, in goldendict.  Use a prefix to force prompting. "
        (interactive)
        (let ((w (if w w (kill-dwim))))
          (call-process-shell-command (format  "goldendict \"%s\"" w ) nil 0)))
      :bind (("C-x G" . goldendict-dwim)))


### API docs

    (defun rgr/devdocs()
      "If in an emacs-lisp buffer or bable block use `rgr/elisp-lookup-reference' else devdocs."
      (interactive)
      (if (or (derived-mode-p  'emacs-lisp-mode) (and (eq
                                                       major-mode 'org-mode) (string= "emacs-lisp" (car (org-babel-get-src-block-info)))))
          (helpful-at-point)
        (let ((s (symbol-at-point)))
          (message "symbol-at-point: %s" s)
          (if (fboundp 'devdocs-browser-open)
              (devdocs-browser-open)
            (call-interactively 'devdocs-lookup (vector 't s ))))))
    (global-set-key (kbd "C-q")  'rgr/devdocs)

1.  devdocs-browser

    <https://github.com/blahgeek/emacs-devdocs-browser> :
    
    Browse devdocs.io documents inside Emacs!
    
        (use-package devdocs-browser
          ;;:disabled t
          :custom
          (devdocs-data-dir (expand-file-name  "var/devdocs-browser"  user-emacs-directory))
          (devdocs-browser-cache-directory (expand-file-name  "var/devdocs-browser/cache"  user-emacs-directory))
          (devdocs-browser-data-directory (expand-file-name  "var/devdocs-browser/data" user-emacs-directory))
          :hook
          (c-ts-mode . (lambda()(setq-local devdocs-browser-active-docs '("c"))))
          (c++-ts-mode . (lambda()(setq-local devdocs-browser-active-docs '("cpp")))))


### pdf-tools

[pdf-tools](https://github.com/politza/pdf-tools) is, among other things, a replacement of DocView for PDF
files : faster, more otions and you can annotatePDFs. 

    (use-package pdf-tools
      :demand t
      :config
      (pdf-loader-install)
      :bind
      (:map pdf-view-mode-map
            ("&" . rgr/open-buffer-file-in-external-browser)))

1.  requirements

        sudo apt install libpng-dev zlib1g-dev libpoppler-glib-dev libpoppler-private-dev imagemagick


### epub

    (use-package nov
      :demand t
      :config
      (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
    
    ;; (use-package nov-xwidget
    ;;   :after nov
    ;;   :config
    ;;   (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
    ;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))


### impatient-showdow, markdown view live

Preview markdown buffer live over HTTP using showdown.
<https://github.com/jcs-elpa/impatient-showdown>

    (use-package impatient-showdown
      :disabled
      :hook (markdown-mode . impatient-showdown-mode))


## Shells and Terminals

lookup and reference uilities and config


### VTERM

    (use-package vterm
      :ensure t)
    (use-package  multi-vterm
      :ensure t
      :commands (multi-vterm-project)
      :bind
      ("M-g t" . multi-vterm-project))


## Programming Language related


### tag matching

    (use-package evil-matchit
      :bind
      ("M-(" . evilmi-jump-items-native))


### consult xref stack

    (use-package consult-xref-stack :ensure (:host github :repo "brett-lempereur/consult-xref-stack")
    :bind
    (("M-," . consult-xref-stack-backward)))

1.  rmsbolt

    RMSbolt is a compiler output viewer in Emacs.
    <https://github.com/emacsmirror/rmsbolt>
    
        (use-package rmsbolt
          :demand t
          :init
          (defun rgr/rmsbolt()
            (interactive)
            (if (not rmsbolt-mode)
                (rmsbolt)
              (progn
                (rmsbolt-mode -1)
                (when (get-buffer rmsbolt-output-buffer)
                    (with-current-buffer  rmsbolt-output-buffer
                      (kill-buffer-and-window))))))
          :bind
          (:map prog-mode-map
                ("C-c d" . rgr/rmsbolt)))

2.  parrot

        (use-package parrot
          :config
          (defun my/parrot-animate-when-compile-success (buffer result)
            (if (string-match "^finished" result)
                (parrot-start-animation)))    (parrot-mode)
          (add-to-list 'compilation-finish-functions 'my/parrot-animate-when-compile-success))


### compilation

    (global-set-key (kbd "C-c C-r") 'recompile)
    (global-set-key (kbd "<f9>")
      '(lambda () (interactive)
          (condition-case nil (next-error)
             (error (next-error 1 t)))))


### indent bars

    (use-package indent-bars
      :disabled
      :ensure t
      :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
      :hook
      (prog-mode . indent-bars-mode))


### JSON

    (use-package json-mode)
    (use-package jsonrpc)


### Treemacs

    (use-package
      treemacs
      :init
      (add-to-list 'image-types 'svg)
      :custom
      (treemacs-follow-after-init t) ; hello
      :config
      (treemacs-follow-mode +1)
      (treemacs-fringe-indicator-mode)
      (treemacs-git-mode 'deferred)
      :bind
      ("M-9"   . 'treemacs-select-window)
      (:map treemacs-mode-map
            ("<right>" . treemacs-peek)))
    (use-package treemacs-magit
      :after treemacs)


### HideShow

    (use-package hideshow
      :ensure nil
      :hook
      (emacs-lisp-mode . hs-minor-mode)
      :bind
      (:map hs-minor-mode-map
            (("C-h t" . hs-toggle-hiding))))


### duplicate thing

    (use-package duplicate-thing
      :demand t
      :bind   ("C-S-d" . 'duplicate-thing))


### Breadcrumbs

    (use-package breadcrumb)


### prog-mode hack

    (unless (fboundp 'prog-mode)
      (defalias 'prog-mode 'fundamental-mode))


### Show Line numbers

    (global-set-key (kbd "S-<f2>") 'display-line-numbers-mode)
    (add-hook 'prog-mode-hook (lambda() (display-line-numbers-mode t)))


### code format

    ;; auto-format different source code files extremely intelligently
    ;; https://github.com/radian-software/apheleia
    (use-package apheleia
      :disabled
      :ensure t
      :config
      (apheleia-global-mode +1))


### BASH

1.  Navigating Bash set -x output

        ;; try to work with next-error for bash's "set -x" output
        (use-package compile
          :ensure nil
          :config
          (add-to-list 'compilation-error-regexp-alist
                       'bash-set-x)
          (add-to-list 'compilation-error-regexp-alist-alist
                       '(pascal
                         "\\(.+?\\)\\(\\([0-9]+\\),\\([0-9]+\\)\\).*" 1 2 3)))


### PHP

    (use-package php-mode)


### JSON, YAML Configuration files

1.  YAML

        (use-package yaml-mode)

2.  json

        (use-package json-reformat)
        (use-package hydra)


### Version Control

1.  It's [Magit](Https://github.com/magit/magit)! A Git porcelain inside Emacs

        (use-package transient)
        (use-package
          magit
          :after transient
          :bind
          ("C-x g" . magit-status))

2.  [Forge](https://github.com/magit/forge) ahead with Pull Requests

    always causes issues. disabled.
    
    :ID:       ed290161-c9c1-455d-ac1a-927c906ab599
    
        (straight-use-package 'sqlite3)
        (use-package forge
          :disabled
          :after magit)
    
    1.  token
    
        &#x2013;&#x2014;BEGIN PGP MESSAGE&#x2013;&#x2014;
        
        hQEMA7IjL5SkHG4iAQgAhMUIMTYGMMJOxJT9Cpd4yXSe7D3nYO1JLdyFFADgiHDq
        1D68ig/iJdH5aPZNmKOOqSeI3zObJjDOnQ95+PK+DBDaDHlwJ/LWYdR/A4eWAh5G
        WcRqCn+diQ/amAXuaISDLBCpEa/GKS3kHObVf41VwL43INMAwssDI2rOFnhOEUWF
        jAWKyhzcS/D+1BqxV5XcloY8tn8qCZbLHOi1+YKr+ZeefFtJtmaqQtyjt6P1Z+H/
        eCvdX0P4JdMm9Lp/fbzDQOPo8QVuWLKAWphMSfgCqetR5gfz9W/3+fcqzOP3amnt
        qTM3K7LywQOeiJhLgyhw5bemiEXIKuNygb7Wmmv5xNKEAc5/QrR0YnZIuEnDN4Gb
        aPjOOiu73X7xh76DX+5CxBXhQZzYtnqD8ctuenIMgvbD9AY1+H4fU0Wv6qGi9uNh
        Y0lSPvXH7dBCTWu2rSWhIvgCQfv6Nihs3Jc455oZgajN7G/rgZNb0ER8YTYncZjY
        gtVhIJfSArVFXuCGLnZMoMHSC4rV
        =MsQY
        &#x2013;&#x2014;END PGP MESSAGE&#x2013;&#x2014;

3.  Git Window Gutter Support

    1.  diff-hl mode
    
            (use-package diff-hl
              :demand t
              :hook
              (elpaca-after-init .  global-diff-hl-mode)
              (magit-post-refresh . diff-hl-magit-post-refresh))


### Tree Sitter

Automatically install and use tree-sitter major modes in Emacs 29+. If the tree-sitter version can’t be used, fall back to the original major mode.

    (use-package treesit-auto
      :custom
      (treesit-auto-install t)
      :config
      (treesit-auto-add-to-auto-mode-alist 'all)
      (global-treesit-auto-mode))


### Javascript

    (use-package js
      :ensure nil
      :demand t
      :init
      (add-to-list 'auto-mode-alist '("\\.mjs" . javascript-mode)) ;; js module file
      (defun rgr/javascript-typescript-common-mode-hook ()
        (electric-pair-mode 1)
        (setq-local rgr/complete-line-f 'rgr/complete-c-line)
        )
      :config
      (defun rgr/js-ts-mode-hook ()
        )
      :hook
      (js-ts-mode . rgr/javascript-typescript-common-mode-hook)
      (js-ts-mode . rgr/js-ts-mode-hook))


### Typescript

    (use-package typescript-ts-mode
      :ensure nil
      :demand t
      :init
      (defun rgr/typescript-ts-mode-hook ()
        )
      :hook
      (typescript-ts-mode .  rgr/javascript-typescript-common-mode-hook)
      (typescript-ts-mode .  rgr/typescript-ts-mode-hook))


### Language Server Protocol (LSP)

[Emacs-lsp](https://github.com/emacs-lsp) : Language Server Protocol client for Emacs

1.  eglot

    Emacs lsp client
    <https://github.com/joaotavora/eglot>
    
        (use-package eglot
          :custom
          (eglot-autoshutdown t)
          (eglot-send-changes-idle-time 0.5)
          (eglot-ignored-server-capabilities '( :documentHighlightProvider));; dont let eglot/eldoc show doc, rather flymake.
          :config
          ;;(add-hook  'eglot-stay-out-of 'yasnippet)
          (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
          (defun rgr/eglot-on-save()
            (when eglot--managed-mode
              (eglot-format-buffer)
              )
            )
          (defun rgr/eglot-managed-mode-hook()
            (hs-minor-mode t)
            (auto-fill-mode t)
            (if (featurep 'breadcrumb)
                (breadcrumb-local-mode)))
          :hook
          (eglot-stay-out-of . company)
          (before-save . rgr/eglot-on-save)
          (eglot-managed-mode . rgr/eglot-managed-mode-hook)
          ((js-ts-mode c-ts-mode c++-ts-mode php-mode auctex-mode) . #'eglot-ensure)
          :bind
          (:map eglot-mode-map (
                ("C-<return>" . eglot-code-actions))))
        (use-package eglot-booster :ensure (:host github :repo "jdtsmith/eglot-booster")
          :after eglot
          :config	(eglot-booster-mode))

2.  dape

        (use-package dape
          :demand t
          :custom
          (dape-default-breakpoints-file (expand-file-name  "var/dape/dape-breakpoints" user-emacs-directory ))
          (dape-buffer-window-arrangement 'right)
          (dape-info-hide-mode-line nil)
          (dape-inlay-hints t)
          ;;(dape-cwd-fn 'projectile-project-root)
          :hook
          ;;(dape-start . dape-breakpoint-load)
          (dape-display-source . pulsar-pulse-line)
          (dape-compile .  kill-buffer)
        
          :config
          ;; Turn on global bindings for setting breakpoints with mouse
          ;; (advice-add 'dape-quit :after (lambda(&rest r)(dape-breakpoint-save dape-default-breakpoints-file)))
          (add-to-list 'recentf-exclude "dape-breakpoints")
          (add-to-list 'recentf-exclude "var/org")
          (dape-breakpoint-global-mode)
          (add-hook 'dape-info-parent-mode-hook
                    (defun dape--info-rescale ()
                      (face-remap-add-relative 'default :height 0.8))))


### Serial Port

    (defgroup rgr/serial-ports nil
      "serial port customization"
      :group 'rgr)
    
    (defcustom rgr/serialIOPort "/dev/ttyACM0"
      "Serial device for emacs to display"
      :type 'string
      :group 'rgr/serial-ports)
    
    (defcustom rgr/serialIOPortBaud 9600
      "Default serial baud rate"
      :type 'integer
      :group 'rgr/serial-ports)
    
    (defun selectSerialPortBuffer()
      (setq ser (get-buffer rgr/serialIOPort))
      (if ser (switch-to-buffer ser)
        (serial-term rgr/serialIOPort rgr/serialIOPortBaud)))
    
    (global-set-key (kbd "C-c s")
                    (lambda()
                      (interactive)
                      (selectSerialPortBuffer)))


### PlatformIO

[platformio-mode](https://github.com/emacsmirror/platformio-mode) is an Emacs minor mode which allows quick building and uploading of PlatformIO projects with a few short key sequences.
The build and install process id documented [here](https://docs.platformio.org/en/latest/ide/emacs.html).

    (use-package platformio-mode
      :disabled t
      :custom
    
      (platformio-mode-silent nil)
      :init
      (require 'ansi-color)
      (defun rgr/platformio-compilation-mode-filter (buf _)
        (interactive)
        (with-current-buffer buf
          (when (derived-mode-p 'platformio-compilation-mode)
            (let ((inhibit-read-only t))
              (ansi-color-apply-on-region (point-min) (point-max))))))
    
      (add-hook 'compilation-finish-functions
                'rgr/platformio-compilation-mode-filter))


### Python

1.  ipython

        (setq python-shell-interpreter "ipython")
        (setq python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

2.  PET

        (use-package pet
          :config
          (add-hook 'python-base-mode-hook 'pet-mode -10))


### Haskell

1.  haskell-mode

        ;; I'm typically confused when it comes to haskell. Note that the interactive stuff I cribbed doesnt work.
        (use-package haskell-mode
          :disabled t
          :config
          (eval-after-load "haskell-mode"
            '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
          (eval-after-load "haskell-cabal"
            '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
          (add-hook 'haskell-mode-hook 'interactive-haskell-mode))


### lldb debugging in emacs

1.  voltron

        (use-package lldb-voltron :disabled t
          :straight (lldb-voltron :local-repo "~/development/emacs/emacs-lldb-voltron" :type git :host github :repo "rileyrg/emacs-lldb-voltron" )
          ;;:config
          ;; (breadcrumb-mode t)
          )


### rust

    
    (use-package rust-mode
      :disabled t
      :ensure t
      :init
      (defcustom rgr/rust-browser-doc-url (concat (format "file://%s/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/index.html?search=" (getenv "HOME")) "%s") "used to format variable `rgr/browser-doc-url'")
      (setq rust-mode-treesitter-derive t))
    
    (use-package rustic
      :disabled t
      :ensure t
      :after (rust-mode)
      :custom
      (rustic-cargo-use-last-stored-arguments t)
      :config
      (setq rustic-format-on-save t)
      (add-to-list 'rgr/eww-external-launch-url-chunks "doc/rust")
      (defun rgr/rust-mode-hook ()
        (message "rgr/rust-mode-hook")
        (setq-local rgr/browser-doc-url rgr/rust-browser-doc-url)
        (setq-local rgr/complete-line-f 'rgr/c-complete-line)
        (setq indent-tabs-mode nil)
        (prettify-symbols-mode))
      :hook
      (rustic-mode . rgr/rust-mode-hook)
      :bind
      (:map rustic-mode-map
            ("C-q" . rgr/browser-doc-search)))


### C and CPP

1.  c-mode-common-hook

        (use-package c-ts-mode
          :ensure nil
          :config
          (defun rgr/c-ts-mode-common-hook ()
            (message "rgr/c-ts-mode-common-hook")
            (yas-minor-mode t) ;; This SHOULD be done by eglot but it doesn't work
            (setq-local rgr/complete-line-f 'rgr/c-complete-line)
            (setq-local c-ts-mode-indent-offset 4))
          :hook
          ((c-ts-mode c++-ts-mode) . rgr/c-ts-mode-common-hook))

2.  cmake

        (setq auto-mode-alist
                (append
                 '(("CMakeLists\\.txt\\'" . cmake-mode))
                 '(("\\.cmake\\'" . cmake-mode))
                 auto-mode-alist))


### Linux tools

1.  [logview](https://github.com/doublep/logview) - view system logfiles

        (use-package logview
          :demand t
          :init
          (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
          (add-to-list 'auto-mode-alist '("log\\'" . logview-mode)))


### Assembler

1.  [x86Lookup](https://nullprogram.com/blog/2015/11/21/)

        (use-package strace-mode)


### Web,Symfony and Twig


### elf-mode - view the symbol list in a binary

[https://oremacs.com/2016/08/28/elf-mode/](https://oremacs.com/2016/08/28/elf-mode/)

    (use-package elf-mode
      :demand t
      :config
      (elf-setup-default))


## Emacs Lisp, ELisp Utils


### helpful, enriched elisp help

    (use-package helpful
      :bind
      ("C-h f" . #'helpful-callable)
      ("C-h v" . #'helpful-variable)
      ("C-h k" . #'helpful-key)
      ("C-h x" . #'helpful-command))


### bracket matching

    (show-paren-mode 1)
    (setq blink-matching-delay 2.5)
    
    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate
          (lambda (c)
            (if (char-equal c ?\") t (electric-pair-default-inhibit c))))
    
    ;; stolen from emacs info
    (defun rgr/match-paren (arg)
      "Go to the matching paren if on a paren; otherwise insert %."
      (interactive "p")
      (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
            ((looking-at "\\s)") (forward-char 1) (backward-list 1))
            (t (self-insert-command (or arg 1)))))
    
    (global-set-key (kbd "%") 'rgr/match-paren)


### kill-dwim

    (setq load-path (cons (expand-file-name "kill-dwim" rgr/emacs-project-dir ) load-path))
    (use-package kill-dwim
      :ensure nil
      :bind
      ("M-w" . kill-dwim))


### Flymake

    (use-package flymake
      :custom
      (flymake-show-diagnostics-at-end-of-line nil)
      (flymake-no-changes-timeout 1.5)
      :bind
      ("M-n" . flymake-goto-next-error)
      ("M-p" . flymake-goto-prev-error))

1.  shellcheck

        (use-package flymake-shellcheck
          :disabled t
          :commands flymake-shellcheck-load
          :init
          (defun rgr/sh-mode-hook()
            (flymake-shellcheck-load)
            (flymake-mode +1))
          :hook (sh-mode . rgr/sh-mode-hook))
    
    1.  external info files
    
            (require 'info)
            (add-to-list 'Info-directory-list (expand-file-name  "var/info" user-emacs-directory))

2.  elisp checks

        (defun rgr/elisp-edit-mode()
          "return non nil if this buffer edits elisp"
          (member major-mode '(emacs-lisp-mode lisp-interaction-mode)))

3.  linting

    [package-lint](https://github.com/purcell/package-lint) provides a linter for the metadata in Emacs Lisp files which are intended to be packages. You can integrate it into your build process.
    
        (use-package package-lint)

4.  elisp popup context help

    Display a poup containing docstring at point
    
        (setq load-path (cons (expand-file-name "el-docstring-sap" rgr/emacs-project-dir ) load-path))
        (use-package el-docstring-sap
          :after (eldoc posframe)
          :ensure nil
          :hook
          (emacs-lisp-mode . el-docstring-sap-mode)
          :bind
          ("M-<f2>" . el-docstring-sap-display)
          ("M-<f1>" . el-docstring-sap-mode))

5.  Elisp debugging

    1.  edebug
    
            (use-package edebug-x
              :custom
              (debugger-stack-frame-as-list t)
              ;;(edebug-trace nil)
              :config
              (defun rgr/instrumentForDebugging() "use the universal prefix arg (C-u) to remove instrumentation"
                     (interactive)
                     (if current-prefix-arg (eval-defun nil)
                       (eval-defun 0)))
              :bind
              ("C-S-<f9>" . toggle-debug-on-error)
              ("C-<f9>" . rgr/instrumentForDebugging))
    
    2.  display value at point in edebug
    
            
            ;; bit more convoluted than it needs to be
            ;; but I fancied using thing-at-point
            (defun rgr/edebug-display-thing-at-point()
              (let ((tap (thing-at-point 'var)))
                (if tap
                    (message "%s: %s" (nth 0 tap) (nth 1 tap)))))
            
            (defun rgr/edebug-thing-at-point()
              "message display the vale of the symbol at point"
              (let((tap (thing-at-point 'symbol)))
                (if tap
                    (let((sym (if (symbolp tap) tap (intern-soft tap))))
                      (condition-case nil
                          (list sym (edebug-eval  sym))
                        (error nil))))))
            
            (defun rgr/edebug-mode-hook()
              (setq-local thing-at-point-provider-alist
                          (append thing-at-point-provider-alist
                                  '((var . rgr/edebug-thing-at-point))))
              "add a call to display the value at point when debugging with edebug"
              (add-hook 'post-command-hook #'rgr/edebug-display-thing-at-point nil :local))
            
            (add-hook 'edebug-mode-hook  #'rgr/edebug-mode-hook)

6.  Formatting

        (use-package
          elisp-format
          :bind
          (:map emacs-lisp-mode-map
                ("C-c f" . elisp-format-region)))

7.  popup query symbol

        (use-package popup
          :config
          (defun rgr/show-symbol-details ()
            (interactive)
            (popup-tip (format "intern-soft thing-at-point: %s, symbolp: %s, symbol-name:%s"
                               (setq-local sym (intern-soft (thing-at-point 'symbol)))
                               (symbolp sym)
                               (symbol-name sym))))
          :bind
          (:map emacs-lisp-mode-map (("M-6" . #'rgr/show-symbol-details))))


## General configuration


### general ui

    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    
    (winner-mode 1)
    
    (repeat-mode)
    
    (global-auto-revert-mode 1)
    ;; Also auto refresh dired, but be quiet about it
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)
    (setq auto-revert-use-notify nil)
    
    ;; Window layout
    ;;(add-to-list 'display-buffer-alist  `((or ,(rx (or "*eww*" "*info*" "*helpful*" "*help*"))) (display-buffer-in-side-window) (window-sides-vertical . t)(side . right)(slot . -1) (window-width . 0.5)) )
    ;; popper
    
    (require 'windmove)
    (windmove-default-keybindings)
    
    (global-visual-line-mode 1)
    
    (setq column-number-mode t)
    
    (delete-selection-mode 1)
    
    ;; (setq frame-title-format (if (member "-chat" command-line-args)  "Chat: %b" '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " — Emacs")))
    
    (defalias 'yes-or-no-p 'y-or-n-p)
    
    (setq disabled-command-function nil)
    
    (global-hl-line-mode t)
    
    (use-package delsel
      :ensure nil
      :hook (after-init . delete-selection-mode))
    
    (defun prot/keyboard-quit-dwim ()
      "Do-What-I-Mean behaviour for a general `keyboard-quit'.
    
    The generic `keyboard-quit' does not do the expected thing when
    the minibuffer is open.  Whereas we want it to close the
    minibuffer, even without explicitly focusing it.
    
    The DWIM behaviour of this command is as follows:
    
    - When the region is active, disable it.
    - When a minibuffer is open, but not focused, close the minibuffer.
    - When the Completions buffer is selected, close it.
    - In every other case use the regular `keyboard-quit'."
      (interactive)
      (cond
       ((region-active-p)
        (keyboard-quit))
       ((derived-mode-p 'completion-list-mode)
        (delete-completion-window))
       ((> (minibuffer-depth) 0)
        (abort-recursive-edit))
       (t
        (keyboard-quit))))
    
    (define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)
    
    ;; display dir name when core name clashes
    (require 'uniquify)
    
    (defun rgr/kill-current-buffer()
      (interactive)
      (if (member (buffer-name) '("*Messages*" "*scratch*"))
          (progn
            (message "Can't delete %s. Are you mad? Closing window instead." (buffer-name))
            (delete-window))
        (kill-current-buffer)
        (delete-window)))
    
    (bind-keys
     ("C-x C-q" . view-mode)
     ( "C-x C-b" . ibuffer)
     ( "C-x C-i" . imenu)
     ( "C-x k" . rgr/kill-current-buffer)
     ( "M-0" . delete-window)
     ( "M-1" . delete-other-windows)
     ( "S-<f1>" . describe-face)
     (  "M-m"  . manual-entry)
     ( "S-<f10>" . menu-bar-open))


### taming windows

    (use-package popper
      :ensure t
      :custom
      (popper-window-height 'rgr/popper-window-height)
      :config
      (defun rgr/popper-window-height(win)
        (interactive)
        (/ (frame-height) 3))
      :init
      (popper-mode +1)
      (popper-echo-mode +1)
      :bind (("C-´"   . popper-toggle)
             ("M-´"   . popper-toggle-type)
             ("C-M-´" . popper-cycle)))


### posframe

[Posframe](https://github.com/tumashu/posframe)
can pop up a frame at point, this posframe is a child-frame connected to its root window's buffer.

    (use-package posframe
      :demand t)


### ACE utilities

1.  [Ace-Window](https://github.com/abo-abo/ace-window) provides better window switching.

        (use-package ace-window
          :bind
          ("M-o" . ace-window)
          ("M-d" . ace-delete-window))

2.  hopping around links

    Quickly follow [links](https://github.com/abo-abo/ace-link) in Emacs.
    
        (use-package ace-link
          :demand
          :config
          (ace-link-setup-default))


### pulsar

visual feedback as to cursor position
<https://protesilaos.com/emacs/pulsar>

    (use-package pulsar
      :custom
      (pulsar-pulse t)
      (pulsar-delay 0.2)
      (pulsar-iterations 15)
      (pulsar-face 'isearch)
      (pulsar-highlight-face 'pulsar-green)
      :init
      (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)
      (add-hook 'consult-after-jump-hook #'pulsar-recenter-middle)
      (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
      :config
      (
       pulsar-global-mode 1))


### boxquote

    (use-package boxquote
      ;;:straight (:branch "main")
      :bind
      ("C-S-r" . boxquote-region))


### volatile-highlights

brings visual feedback to some operations by highlighting portions relating to the operations.

    (use-package
      volatile-highlights
      :init (volatile-highlights-mode 1))


### web pasting

    (use-package
      dpaste
      :init
      :bind ("C-c y" . dpaste-region-or-buffer))


### reading / concentration aids

1.  Darkroom

    Zoom in and center using [darkroom](https://github.com/joaotavora/darkroom).
    
        (use-package
          darkroom
          :bind
          ( "<C-f7>" . 'darkroom-mode))

2.  Olivetti Mode

        (use-package olivetti
          :disabled t
          :bind 
          ("C-S-<f7>" . olivetti-mode))

3.  WriteRoomMode

        (use-package writeroom-mode
          :bind 
          ("C-S-<f7>" . writeroom-mode))


### Ansi colour

[Ansi colour hooks](https://www.emacswiki.org/emacs/AnsiColor) to enable emacs buffers to handle ansi.

    (require 'ansi-color)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


### Tabs

    
    (defun consult-buffer-other-tab ()
      "Variant of `consult-buffer' which opens in other tab."
      (interactive)
      (let ((consult--buffer-display #'switch-to-buffer-other-tab))
        (consult-buffer)))

1.  emacs-modern-tab-bar

        (use-package modern-tab-bar
        :ensure (:host github :repo "aaronjensen/emacs-modern-tab-bar")
        :custom
        (tab-bar-show t)
        (tab-bar-new-button nil)
        (tab-bar-close-button-show nil)
        (tab-bar-select-tab-modifiers '(control))
        :init
        (modern-tab-bar-mode))


### buffer utlities

    (defun get-buffers-with-minor-mode (minor-mode)
      "Get a list of buffers in which minor-mode is active"
      (interactive)
      (let ((minor-mode-buffers))
        (dolist (buf (buffer-list) minor-mode-buffers)
          (with-current-buffer buf
            (when (memq minor-mode (manage-minor-mode--active-list))
              (push buf minor-mode-buffers))))))
    
    
    (defun get-buffers-with-major-mode (m-mode)
      "Get a list of buffers in which minor-mode is active"
      (interactive)
      (let ((major-mode-buffers)) 
        (dolist (buf (buffer-list) major-mode-buffers)
          (with-current-buffer buf
            (when (string= major-mode m-mode)
              (push buf major-mode-buffers))))))   ;; (nth 0 (get-buffers-with-major-mode "dired-mod"))
    
    ;; Taken from https://github.com/ShingoFukuyama/manage-minor-mode
    (defun manage-minor-mode--active-list ()
      "Get a list of which minor modes are enabled in the current buffer."
      (let ($list)
        (mapc (lambda ($mode)
                (condition-case nil
                    (if (and (symbolp $mode) (symbol-value $mode))
                        (setq $list (cons $mode $list)))
                  (error nil)))
              minor-mode-list)
        (sort $list 'string<)))


### switch to dired buffer else open it

    (defvar rgr/last-dired nil "last dired buffer opened")
    (add-hook 'dired-after-readin-hook (lambda()(setq rgr/last-dired (buffer-name))))
    (defun switch-to-dired(&optional d)
      (if (and rgr/last-dired (get-buffer rgr/last-dired))
          (switch-to-buffer rgr/last-dired)
        (let ((db (nth 0 (last (get-buffers-with-major-mode "dired-mode")))))
          (if db (switch-to-buffer db)
            (dired (if d d "~"))))))


### multiple-cursors

<https://github.com/magnars/multiple-cursors.el>

    (use-package multiple-cursors
      :bind
      ("C-<mouse-1>" . add-cursor-on-click)
      ("C-S-n" . mc/mark-next-like-this)
      ("C-S-p" . mc/mark-previous-like-this)
      ("C-S-w" . mc/mark-all-dwim)
      ("C-S-a" . mc/mark-all-like-this)
      ("C-S-SPC" . mc/edit-lines)
      )


### jinx : the enchanted spell checker

    (use-package jinx
      :hook (emacs-startup . global-jinx-mode)
      :bind
      ("M-$" . jinx-correct)
      ("<f8>" . jinx-correct)
      ("S-<f8>" . jinx-correct-word)
      ("C-<f8>" . jinx-languages))

\#+end\_src 


## Late load-dir

    (add-hook 'elpaca-after-init-hook (lambda()(load-el-gpg (expand-file-name "etc/late-load" user-emacs-directory))))


# DIRED Daemon Init File

This tangles to its own init file [init-dired.el](etc/elisp/init-dired.el).

    (add-hook 'elpaca-after-init-hook (lambda()(run-with-idle-timer 1 nil  (lambda()(dired "~")))))
    (add-hook 'server-after-make-frame-hook (lambda(&optional f)(run-with-idle-timer 1 nil  (lambda()(switch-to-buffer (other-buffer))))))


# Email Init File

This tangles to its own init file [init-email.el](etc/elisp/init-email.el).


## mu4e

    (emacs-alert "Starting Email - MU4E")
    (use-package mu4e :ensure ( :host github
                                :branch "release/1.10"
                                :repo "djcb/mu"
                                :files ("mu4e/*.el" "build/mu4e/mu4e-meta.el" "build/mu4e/mu4e-config.el" "build/mu4e/mu4e.info")
                                :main "mu4e/mu4e.el"
                                :pre-build (("./autogen.sh")
                                            ("ninja" "-C" "build")
                                            (make-symbolic-link (expand-file-name "./build/mu/mu")
                                                                (expand-file-name "~/bin/mu") 'ok-if-exists)))
      :commands (mu4e mu4e-update-index)
      :custom
      ( mayil-user-agent 'mu4e-user-agent )
      ( mail-user-agent 'mu4e-user-agent )
      ( message-send-mail-function 'smtpmail-send-it )
      ( mu4e-attachment-dir "~/Downloads" )
      ( mu4e-change-filenames-when-moving t )
      ( mu4e-compose-context-policy 'ask )
      ( mu4e-confirm-quit nil )
      ( mu4e-context-policy 'pick-first )
      ( mu4e-compose-reply-recipients 'sender )
      ( mu4e-headers-include-related nil )
      ( mu4e-headers-show-threads nil ) ; Use "P" to toggle threading
      ( mu4e-decryption-policy 'ask )
      ( mu4e-hide-index-messages t )
      ( mu4e-mu-binary (expand-file-name "mu/build/mu/mu" elpaca-repos-directory) )
      ( mu4e-update-interval nil )
      ( mu4e-use-fancy-chars t )
      ( mu4e-view-prefer-html nil )
      ( mu4e-view-show-addresses t )
      ( smtpmail-smtp-service 587 )
      ( user-full-name "Richard G.Riley" )
      :config
    
      (defcustom email-gmail-email "foo@bar.bing" "gmail email address" :type '(string)
           :group 'rgr/email)
      (defcustom email-gmail-full-name "Mr Foo" "gmail full name" :type '(string)
           :group 'rgr/email)
      (defcustom email-gmail-signature "Bye" "gmail sign off signature" :type '(string)
           :group 'rgr/email)
      (defcustom email-gmx-email "wong@foo.bar" "gmx email address" :type '(string)
           :group 'rgr/email)
      (defcustom email-gmx-full-name "Mr Wong" "gmx full name" :type '(string)
           :group 'rgr/email)
      (defcustom email-gmx-signature "my most humble contrafibularities" "gmx sign off signature" :type '(string)
           :group 'rgr/email)
    
      (use-package mu4e-alert
        :init
        (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))
    
      (use-package mu4e-column-faces
        :after mu4e
        :config (mu4e-column-faces-mode))
    
      (setq mu4e-contexts
            `( ,(make-mu4e-context
                 :name "aGmx"
                 :enter-func (lambda () (mu4e-message "gmx context")(mu4e-update-index))
                 :match-func (lambda (msg)
                               (when msg
                                 (string-match-p "^/gmx" (mu4e-message-field msg :maildir))))
                 :vars `( ( user-mail-address . ,email-gmx-email )
                          ( mu4e-get-mail-command . "getmails gmx gmx-special-interest")
                          ( user-full-name . ,email-gmx-full-name )
                          ( smtpmail-smtp-server . "smtp.gmail.com")
                          ( mu4e-refile-folder . "/gmx/Archive" )
                          ( mu4e-sent-folder . "/gmx/Sent" )
                          ( mu4e-sent-messages-behavior . sent)
                          ( mu4e-trash-folder . "/gmx/Bin" )
                          ( mu4e-drafts-folder . "/gmx/Drafts" )
                          ;; (mu4e-maildir-shortcuts .
                          ;;  (("/gmx/INBOX"             . ?i)
                          ;;    ("/gmx/Sent" . ?s)
                          ;;    ("/gmx/Bin"     . ?b)
                          ;;    ("/gmx/Drafts"    . ?d)
                          ;;    ("/gmx/Spam"    . ?p)
                          ;;    ("/gmx/Archive"  . ?a)))
                          ( mu4e-bookmarks . ((:name "Inbox" :query "maildir:/gmx/INBOX and flag:unread" :key ?i)
                                              (:name "Learning" :query "maildir:/gmx/Learning* and flag:unread" :key ?l)
                                              (:name "All Today's messages" :query "maildir:/gmx/*  AND NOT (maildir:/gmx/Spam  OR  maildir:/gmx/Sent) AND date:today..now " :key ?t)
                                              (:name "Last 7 days" :query "maildir:/gmx/* AND NOT (maildir:/gmx/Spam  OR  maildir:/gmx/Sent)  AND date:7d..now" :hide-unread t :key ?w)
                                              (:name "All" :query "maildir:/gmx/* and not (maildir:/gmx/Spam or maildir:/gmx/Bin)" :key ?a)
                                              (:name "Bin" :query "maildir:/gmx/Bin" :key ?b)
                                              ;;                      (:name "Messages with images" :query "maildir:/gmx/* AND  NOT maildir:/gmx/Spam  AND  NOT maildir:/gmx/Sent" :key ?m)
                                              (:name "Spam" :query "maildir:/gmx/Spam AND date:7d..now" :hide-unread t :key ?p)))
                          ( mu4e-compose-signature  .
                            ,email-gmx-signature
                            )))
               ,(make-mu4e-context
                 :name "bGmail"
                 :enter-func (lambda () (mu4e-message "gmail context") (mu4e-update-index))
                 ;; no leave-func
                 ;; we match based on the maildir of the message
                 ;; this matches maildir /Arkham and its sub-directories
                 :match-func (lambda (msg)
                               (when msg
                                 (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
                 :vars `( ( user-mail-address . ,email-gmail-email  )
                          ( user-full-name . ,email-gmail-full-name )
                          ( smtpmail-smtp-server . "smtp.gmail.com")
                          ( mu4e-get-mail-command . "getmails gmail")
                          ( mu4e-refile-folder . "/gmail/Archive" )
                          ( mu4e-sent-folder . "/gmail/Sent" )
                          ( mu4e-sent-messages-behavior . delete)
                          ( mu4e-trash-folder . "/gmail/Bin" )
                          ( mu4e-drafts-folder . "/gmail/Drafts" )
                          ;; (mu4e-maildir-shortcuts .
                          ;;   (("/gmail/INBOX"             . ?i)
                          ;;    ("/gmail/Sent" . ?s)
                          ;;    ("/gmail/Bin"     . ?b)
                          ;;    ("/gmail/Drafts"    . ?d)
                          ;;    ("/gmail/Spam"    . ?p)
                          ;;    ("/gmail/Archive"  . ?a)))
                          ( mu4e-bookmarks . ((:name "Inbox" :query "maildir:/gmail/INBOX and flag:unread" :key ?i)
                                              (:name "All Today's messages" :query "maildir:/gmail/* AND NOT (maildir:/gmail/Spam  OR  maildir:/gmail/Sent) AND date:today..now " :key ?t)
                                              (:name "Last 7 days" :query "maildir:/gmail/* AND NOT (maildir:/gmail/Spam  OR  maildir:/gmail/Sent) AND date:7d..now" :hide-unread t :key ?w)
                                              (:name "All" :query "maildir:/gmail/* and not (maildir:/gmail/Spam or maildir:/gmail/Bin)" :key ?a)
                                              (:name "Bin" :query "maildir:/gmail/Bin" :key ?b)
                                              ;;                    (:name "Messages with images" :query "maildir:/gmail/* AND  NOT maildir:/gmail/Spam  AND  NOT maildir:/gmail/Sent" :key ?m)
                                              (:name "Spam" :query "maildir:/gmail/Spam AND date:7d..now" :hide-unread t :key ?p)))
                          ( mu4e-compose-signature . ,email-gmail-signature)))))
    
      (defun mu4e-smarter-compose ()
        "My settings for message composition."
        (set-fill-column 72)
        )
    
      (defun rgr/mu4e-default-context()
        (interactive)
        (mu4e)
        (mu4e-context-switch nil "bGmail"))
    
      (add-to-list 'mu4e-view-actions
                   '("ViewInBrowser" . mu4e-action-view-in-browser) t)
      (add-to-list 'mu4e-view-actions
                   '("XWidget View" . mu4e-action-view-with-xwidget) t)
      (add-to-list 'mu4e-view-actions
                   '("Markall as read" . mu4e-headers-mark-all-unread-read) t)
      (require 'mu4e-contrib)
      :hook ((mu4e-view-rendered . visual-line-mode)
             (mu4e-compose-mode . mu4e-smarter-compose)
             (mu4e-view-rendered .
                                 (lambda()
                                   ;; try to emulate some of the eww key-bindings
                                   (local-set-key (kbd "<tab>") 'shr-next-link)
                                   (local-set-key (kbd "<backtab>") 'shr-previous-link))))
      :bind	  (("C-c u" .  rgr/mu4e-default-context)
               (:map mu4e-main-mode-map
                     ("m" . mu4e-compose-new))
               (:map mu4e-main-mode-map
                     ("u" . mu4e-update-index))
               (:map mu4e-headers-mode-map
                     ("v" . mu4e-view-action)
                     ("C-c u" . mu4e-headers-mark-all-unread-read))))
    ;;:map mu4e-view-mode-map
    ;;   ("V" . '(lambda()(message "%s" (mu4e-message-at-point))))))) ;; mu4e-action-view-in-browser))))
    (add-hook 'server-after-make-frame-hook (lambda()(run-with-idle-timer 1 nil  (lambda()(rgr/mu4e-default-context)))))


# IRC daemon init file

This tangles to its own init file [init-irc.el](etc/elisp/init-irc.el) and locks down emacs a little. This is so I can run an erc "session" without interfering with other emacsy things.


## erc

    ;; generally loaded from init-erc.el ins a kiosk like mode
    (require 'erc)
    
    (add-hook 'find-file-hook (lambda()(read-only-mode 1)))
    (set-face-background 'mode-line "orangered")
    (defun my/erc-buffer-connected-p (buffer)
      "Check if ERC BUFFER is connected."
      (with-current-buffer buffer
        (and (erc-server-process-alive)
             erc-server-connected)))
    
    ;;https://emacs.stackexchange.com/questions/64361/how-to-check-if-erc-is-running
    (defun my/erc-start-or-switch (&optional rest)
      (interactive)
      (let ((erc-buffers '("Libera.Chat" "irc.libera.chat" "irc.libera.chat:6667"))
            (connected nil))
        (dolist (buffer erc-buffers)
          (when (and (get-buffer buffer)
                     (my/erc-buffer-connected-p buffer))
            (setq connected t)))
        (if connected
            (rgr/erc-switch-to-channel)
          (progn
            (emacs-alert "Connecting to IRC")
            (erc :server "irc.libera.chat" :port 6667)
            (global-set-key (kbd "C-c C-q") 'rgr/irc-quit )))))
    
    (defun rgr/erc-switch-to-channel(&optional frame channel)
      (let ((c (or channel "#emacs")))
        (if (get-buffer c)
            (switch-to-buffer c))))
    
    (defun rgr/irc-quit()
      (interactive)
      (erc-quit-server "leaving emacs session")
      (when (functionp 'ement-disconnect)
        (let ((current-prefix-arg '(4)))
          (call-interactively 'ement-disconnect))))
    
    (setq kill-emacs-hook '(rgr/irc-quit))


## matrix client ement

    (use-package ement
      :disabled t
      :demand t
      :bind ("M-g M-l" . 'ement-room-list))


## starting irc/chats

    (defun rgr/irc-reconnect(&optional f)
      (interactive)
      (my/erc-start-or-switch)
      (if (featurep 'ement)
          (if ement-sessions
          (ement-room-list)
        (call-interactively 'ement-connect))))
    
    (add-hook 'server-after-make-frame-hook #'rgr/irc-reconnect)
    ;;  (add-hook 'elpaca-after-init-hook (lambda()(run-with-idle-timer 1 nil #'rgr/irc-reconnect)))


# Associated emacs things


## building emacs


### configure

    #!/usr/bin/env bash
    # Maintained in emacs-config.org
    cd ~/development/emacs/emacs/
    ./configure --with-x-toolkit=lucid -with-tree-sitter --with-native-compilation --prefix="/usr/local/" 


### make

    #!/usr/bin/env bash
    # Maintained in emacs-config.org
    cd ~/development/emacs/emacs/
    make -j$(nproc) && sudo make install


## Project version control with  git


### .gitignore

An exclusionary .gitignore. You need to specfically add in things you wish
to add to version control.

    *
    *.*
    
    !.gitignore
    !.gitattributes
    !.ignore
    
    !emacs-config.org
    !early-init.el
    !init.el
    !README.md
    !custom.el
    
    !elpaca-versions.lock
    
    !info
    !info/*
    
    !etc
    !var
    !var/secrets
    !var/secrets/bmkp
    !var/secrets/bmkp/*
    !var/secrets/ement-sessions.el
    
    
    !etc/abbrev
    !etc/abbrev/*
    
    !etc/early-load
    !etc/early-load/*
    
    !etc/elfeed
    !etc/elfeed/elfeed.org
    
    
    !etc/org
    !etc/org/agenda-files.txt
    !etc/org/template.org
    
    !etc/eshell
    !etc/eshell/alias
    
    !etc/elisp
    !etc/elisp/*.el
    !etc/elisp/README
    
    
    !etc/hosts
    !etc/hosts/thinkpadx270
    !etc/hosts/thinkpadx270/custom.el
    !etc/hosts/thinkpadx60
    !etc/hosts/thinkpadx60/custom.el
    !etc/hosts/intelnuc
    !etc/hosts/intelnuc/custom.el
    !etc/hosts/xmgneo
    !etc/hosts/x13amdg4
    !etc/hosts/x13amdg4/faces.el
    !etc/hosts/xmgneo/custom.el
    
    !editor-config
    !editor-config/*
    
    !bin
    !bin/*
    
    !images
    !images/*
    
    !straight
    !straight/versions
    !straight/versions/*


## Setting up emacs as a default editor using a dot desktop file and associated protocol handler


### [php.ini](editor-config/php.ini) changes e.g /etc/php/7.3/php.ini

`xdebug.file_link_format` is used by compliant apps to format a protocol uri. This is handled on my Linux system as a result of [emacsclient.desktop](#orgcff91a6) documented below.

    xdebug.file_link_format = "emacsclient://%f@%l"
    
    xdebug.remote_enable = 1
    xdebug.remote_host = localhost
    xdebug.remote_port = 9000

    xdebug.file_link_format = "emacsclient://%f@%l"
    
    xdebug.remote_enable = 1
    xdebug.remote_host = localhost
    xdebug.remote_port = 9000


### [emacsclient-linenumber](bin/emacsclient-linenumber) script to correctly parse the protocol in order to start emacs frame at correct line

    #!/usr/bin/env bash
    # Maintained in emacs-config.org
    export GDK_NATIVE_WINDOWS=1
    file=`echo "$@" | sed 's/.*://;s/@.*$//'`
    line=`echo "$@" | sed 's/.*@//'`
    echo $file
    echo $line
    
    re='^[0-9]+$'
    if ! [[ $line =~ $re ]] ; then
        exec emacsclient -c -a ""  $file
    else
        exec emacsclient -c -a "" +$line  $file
    fi


<a id="orgcff91a6"></a>

### Gnome protocol handler desktop file

Copy [emacsclient.desktop](editor-config/emacsclient.desktop) to  ~/.local/share/applications (Debian & Gnome - your mileage may vary&#x2026;)

    [Desktop Entry]
    Name=Emacs (Client)
    GenericName=Text Editor
    Comment=Edit text
    MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
    Exec=emacsclient -c -a "" %F
    Icon=emacs
    Type=Application
    Terminal=false
    Categories=Development;TextEditor;Utility;
    StartupWMClass=Emacs

Update the desktop database:

    update-desktop-database ~/.local/share/applications

now allows emacsclient to be called.

note: symlink the deprecated [~/.local/share/applications/mimeapps.lst](https://wiki.archlinux.org/index.php/XDG_MIME_Applications) to the one in ~/.config

    ln -sf  ~/config/mimeapps.lst ~/.local/share/applications/


### Always using emacsclient

Set up a zshrc alias so that "emacs" actually invokes emacs client. In my .zshrc:

    alias emacs='emacsclient --create-frame --alternate-editor=""'


## [bin/emacs-same-frame](bin/emacs-same-frame) open emacs in the same frame

    #!/bin/bash
    if ! pidof "emacs"; then
        emacsclient -n -c -a "" "$@"
    else
        emacsclient -e "(if (> (length (frame-list)) 1) 't)" | grep -q t
        if [ "$?" = 1 ]; then
            emacsclient -n -c -a "" "$@"
        else
            emacsclient -n -a "" "$@"
            pop-window "Emacs"
        fi
    fi

