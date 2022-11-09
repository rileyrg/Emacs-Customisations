# Introduction

Emacs customisation generates [init.el](init.el) and other [emacs elisp utility files](etc/elisp/) using [org-babel-tangle](https://orgmode.org/manual/Extracting-Source-Code.html).


## scratch

```emacs-lisp
(let ((foo "foo")
      (bar "bar"))
  (concat bar foo))
```


## Own libraries

These libraries are seperate stand alone github libraries.


### el-docstring-sap

Provides docstring help for symbol at point.

[https://github.com/rileyrg/el-docstring-sap](https://github.com/rileyrg/el-docstring-sap)


### lazy-lang-learn

A small "game" like utility that displays snippets to glance at. You can then invoke google translate on them. Stores history.

<https://github.com/rileyrg/lazy-lang-learn>


# early stuff


## debug init utility function

```emacs-lisp
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
```


## early-init.el

```emacs-lisp
;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
;; Maintained in emacs-config.org
(setq package-enabled-at-startup nil)
(setq max-specpdl-size 13000)
;;; early-init.el ends here
```


### look into removing those repos paths added in early-init. cant remember why they are there.


## custom.el

```emacs-lisp
(setq custom-file  (expand-file-name  "custom.el" user-emacs-directory)) ;;
(load custom-file 'noerror)
```


## debug init before straight

```emacs-lisp
(debug-init)
```


# package management


## straight.el package management

[straight.el](https://github.com/raxod502/straight.el#features): next-generation, purely functional package manager for the Emacs hacker.


### bootstrap

```emacs-lisp
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-vc-git-default-protocol 'ssh))

```


### Updating packages

```emacs-lisp
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))
```


# config


## post straight debug init

Here can load a "bare bones" init. When hit debug can "c" to continue or "q" to abort.

```emacs-lisp
;; look for a debug init file and load, trigger the debugger
(debug-init "debug-init-straight.el")
```


## Paths, clutter


### [no-littering](https://github.com/emacscollective/no-littering) aims to keep our undies folded.

```emacs-lisp
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
```


### Path to our own elisp

```emacs-lisp
(defvar elisp-dir (expand-file-name "elisp" no-littering-etc-directory) "my elisp directory. directories are recursively added to path.")
(add-to-list 'load-path elisp-dir)
(let ((default-directory elisp-dir))
  (normal-top-level-add-subdirs-to-load-path))
```


## Load early stuff / gpg

Load all files in certain directories.

```emacs-lisp
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
```


## host specific

Stick a custom in here. eg my thinkpad [custom file](./etc/hosts/thinkpadx270/custom.el).

```emacs-lisp
(load-el-gpg (expand-file-name (system-name)  (no-littering-expand-etc-file-name "hosts")))
```


## Security

```emacs-lisp
(require 'rgr/security "rgr-security" 'NOERROR)
```


### rgr-security library

1.  Auth-Sources, get-auth-info

    Let emacs take care of security things automagically. example:

    ```emacs-lisp
    (setq passw (get-auth-info "licenses" "my-auth-token"))
    ```

    ```emacs-lisp
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
    ```

2.  Pass

    Uses the unix command line `pass` utility. Can be used via `process-lines` e.g

    ```emacs-lisp
    (car (process-lines "pass" "Chat/slack-api-token"))
    ```

    ```emacs-lisp
    (use-package pass)
    ```

3.  provide

    ```emacs-lisp
    (provide 'rgr/security)
    ```


## Utilities

Raw: [rgr-utils](etc/elisp/rgr-utils.el).

```emacs-lisp
(require 'rgr/utils "rgr-utils" 'NOERROR)
```


### rgr-utils library

1.  thing at point or region or input

    ```emacs-lisp
    (defun rgr/region-symbol-query()
      "if a prefix argument (4)(C-u) read from input, else if we have a region select then return that and deselect the region, else try symbol-at-point and finally fallback to input"
      (let* ((w (if (or  (not current-prefix-arg) (not (listp current-prefix-arg)))
                    (if(use-region-p)
                        (let ((sel-text
                               (buffer-substring-no-properties
                                (mark)
                                (point))))
                          sel-text)
                      (thing-at-point 'symbol)) nil))
             (result (if w w (read-string "lookup:"))))
        result))

    ```

2.  read and write elisp vars to file

    ```emacs-lisp

    (defun rgr/elisp-write-var (f v)
      (with-temp-file f
        (prin1 v (current-buffer))))

    (defun rgr/elisp-read-var (f)
      (with-temp-buffer
        (insert-file-contents f)
        (cl-assert (eq (point) (point-min)))
        (read (current-buffer))))
    ```

3.  provide

    ```emacs-lisp
    (provide 'rgr/utils)
    ```


## Emacs daemon & startup

Load up the daemon if not loaded, amongst other things.

Raw: [rgr/startup](etc/elisp/rgr-startup.el)

```emacs-lisp
(require 'rgr/startup "rgr-startup" 'NOERROR)
```


### rgr-startup library

1.  desktop-save

    This has frequently caused problems. The docs seems slightly off and the setting of variables a little confusing. Google seems to confirm this. Anyways, this is working.

    ```emacs-lisp
    (use-package emacs
      :custom
      (desktop-path '("~/.emacs.d/var/desktop"))
      (desktop-save t)
      (desktop-load-locked-desktop t)
      :init
      (defun rgr/restore-desktop()
        (when (fboundp 'alert)
          (alert (message (format "loading desktop from %s" desktop-path))))
        ;;I include the run-with-timer despite being able to get this to work without as it's a timing
        ;;issue and a little delay does no one any harm
        (run-at-time "1" nil (lambda()
                               (desktop-read)
                               (desktop-save-mode 1))))
      ;; (add-hook 'emacs-startup-hook
      ;;           (lambda()
      ;;             (if (daemonp)
      ;;                 (add-hook 'server-after-make-frame-hook 'rgr/restore-desktop)
      ;;               (rgr/restore-desktop))))
      )
    ```

2.  emacs server

    If not already deamonised

    ```emacs-lisp
    ;; start emacs-server if not running
    (unless(daemonp)
      (add-hook 'after-init-hook
                (lambda ()
                  (require 'server)
                  (unless (server-running-p)
                    (message "Starting EmacsServer from init as not already running.")
                    (server-start))
                  )))
    ```

3.  rest of startup

    ```emacs-lisp
    (defun quit-or-close-emacs(&optional kill)
      (interactive)
      (if (or current-prefix-arg kill)
          (server-shutdown)
        (delete-frame)))

    (defun server-shutdown ()
      "Save buffers, Quit, and Shutdown (kill) server"
      (interactive)
      (save-some-buffers)
      (kill-emacs))

    (global-set-key (kbd "C-c x") 'quit-or-close-emacs)
    (global-set-key (kbd "C-x C-c") 'nil)

    (use-package alert
      :init
      (let ((alert-fade-time 5))
        (alert "Emacs is starting..." :title "Emacs")))

    (provide 'rgr/startup)
    ```


## Minibuffer Enrichment (search, sudo edit&#x2026;)

Various plugins for minibuffer enrichment

Raw: [rgr/minibuffer](etc/elisp/rgr-minibuffer.el)

```emacs-lisp
(require 'rgr/minibuffer "rgr-minibuffer" 'NOERROR)
```


### library

1.  [TRAMP](https://www.emacswiki.org/emacs/TrampMode) (Transparent Remote Access, Multiple Protocols)

    is a package for editing remote files, similar to AngeFtp or efs. Whereas the others use FTP to connect to the remote host and to transfer the files, TRAMP uses a remote shell connection (rlogin, telnet, ssh). It can transfer the files using rcp or a similar program, or it can encode the file contents (using uuencode or base64) and transfer them right through the shell connection.

    ```emacs-lisp
    ;;(require 'tramp)
    (use-package tramp
      :custom
      (tramp-default-method "ssh")
      )
    ```

2.  [ctrlf searching](https://github.com/raxod502/ctrlf)

    ```emacs-lisp
    (use-package ctrlf
      :custom
      (ctrlf-auto-recenter t)
      :init
      (ctrlf-mode +1))
    ```

3.  file opening

    1.  read only by default

        Increasingly editing by mistake. Can use [read-only-mode](read-only-mode) to edit it.

        ```emacs-lisp
        (defun maybe-read-only-mode()
          (when (cond ((eq major-mode 'org-mode) t))
            (message "Setting readonly mode for %s buffer" major-mode)
            (read-only-mode +1)))
                                                ;(add-hook 'find-file-hook 'maybe-read-only-mode)
        ```

    2.  [sudo-edit](https://github.com/nflath/sudo-edit) Priviliged file editing

        ```emacs-lisp
        (use-package sudo-edit)
        ```

    3.  find file at point

        ```emacs-lisp
        (use-package ffap
          :custom
          (ffap-require-prefix nil)
          :init
          (ffap-bindings)
          (defun rgr/ffap()
            (interactive)
            (let ((url (ffap-url-at-point)))
              (if (and url current-prefix-arg)
                  (browse-url-generic url)
                (call-interactively 'find-file-at-point))))
          :bind
          ( "C-x C-f" . rgr/ffap))
        ```

4.  [Consult](https://github.com/minad/consult)

    [Consult](https://github.com/minad/consult) Provides various commands based on the Emacs completion function completing-read

    :ID: ec5375c7-4387-42a1-8938-5fad532be79b

    ```emacs-lisp
    ;; Example configuration for Consult
    (use-package consult
      ;; Replace bindings. Lazily loaded due by `use-package'.
      :bind (;; C-c bindings (mode-specific-map)
             ("C-c h" . consult-history)
             ("C-c m" . consult-mode-command)
             ("C-c k" . consult-kmacro)
             ;; C-x bindings (ctl-x-map)
             ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
             ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
             ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
             ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
             ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
             ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
             ;; Custom M-# bindings for fast register access
             ("M-#" . consult-register-load)
             ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
             ("C-M-#" . consult-register)
             ;; Other custom bindings
             ("M-y" . consult-yank-pop)                ;; orig. yank-pop
             ("<help> a" . consult-apropos)            ;; orig. apropos-command
             ;; M-g bindings (goto-map)
             ("M-g e" . consult-compile-error)
             ("M-g f" . consult-flycheck)               ;; Alternative: consult-flymake
             ("M-g g" . consult-goto-line)             ;; orig. goto-line
             ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
             ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
             ("M-g m" . consult-mark)
             ("M-g k" . consult-global-mark)
             ("M-g i" . consult-imenu)
             ("M-g I" . consult-imenu-multi)
             ;; M-s bindings (search-map)
             ("M-s d" . consult-find)
             ("M-s D" . consult-locate)
             ("M-s g" . consult-grep)
             ("M-s G" . consult-git-grep)
             ("M-s r" . consult-ripgrep)
             ("M-s l" . consult-line)
             ("M-s L" . consult-line-multi)
             ("M-s m" . consult-multi-occur)
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
             ("M-r" . consult-history))                ;; orig. previous-matching-history-element

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
      ;; (setq consult-preview-key (kbd "M-."))
      ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
      ;; For some commands and buffer sources it is useful to configure the
      ;; :preview-key on a per-command basis using the `consult-customize' macro.
      (consult-customize
       consult-theme :preview-key '(:debounce 0.2 any)
       consult-ripgrep consult-git-grep consult-grep
       consult-bookmark consult-recent-file consult-xref
       consult--source-bookmark consult--source-file-register
       consult--source-recent-file consult--source-project-recent-file
       ;; :preview-key (kbd "M-.")
       :preview-key '(:debounce 0.4 any))

      ;; Optionally configure the narrowing key.
      ;; Both < and C-+ work reasonably well.
      (setq consult-narrow-key "<") ;; (kbd "C-+")

      ;; Optionally make narrowing help available in the minibuffer.
      ;; You may want to use `embark-prefix-help-command' or which-key instead.
      ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

      ;; By default `consult-project-function' uses `project-root' from project.el.
      ;; Optionally configure a different project root function.
      ;; There are multiple reasonable alternatives to chose from.
      ;;;; 1. project.el (the default)
      ;; (setq consult-project-function #'consult--default-project--function)
      ;;;; 2. projectile.el (projectile-project-root)
      (autoload 'projectile-project-root "projectile")
      (setq consult-project-function (lambda (_) (projectile-project-root)))
      ;;;; 3. vc.el (vc-root-dir)
      ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
      ;;;; 4. locate-dominating-file
      ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
      )
    ```

5.  [Embark](https://github.com/oantolin/embark) Emacs Mini-Buffer Actions Rooted in Keymaps

    ```emacs-lisp
    (use-package embark
      :config
      (setq embark-action-indicator
            (lambda (map _target)
              (which-key--show-keymap "Embark" map nil nil 'no-paging)
              #'which-key--hide-popup-ignore-command)
            embark-become-indicator embark-action-indicator)
      ;; Optionally replace the key help with a completing-read interface
      (setq prefix-help-command #'embark-prefix-help-command)
      ;; Hide the mode line of the Embark live/completions buffers
      (add-to-list 'display-buffer-alist
                   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                     nil
                     (window-parameters (mode-line-format . none))))
      :bind
      ("M-e" . embark-act)       ;; pick some comfortable binding
      ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
    ```

    1.  embark-consult

        ```emacs-lisp
        (use-package embark-consult
          :after (embark consult)
          ;; if you want to have consult previews as you move around an
          ;; auto-updating embark collect buffer
          :hook
          (embark-collect-mode . embark-consult-preview-minor-mode))

        ```

6.  [Marginalia](https://en.wikipedia.org/wiki/Marginalia) margin annotations for info on line

    are marks or annotations placed at the margin of the page of a book or in this case helpful colorful annotations placed at the margin of the minibuffer for your completion candidates

    :ID: f3802197-c745-40b2-ac0d-72d7da291aaf

    The [marginalia](https://github.com/minad/marginalia) pckage in emacs is very helpful.

    ```emacs-lisp
    (use-package marginalia
      :custom
      (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
      :init
      (marginalia-mode)
      (advice-add #'marginalia-cycle :after
                  (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))
    ```

7.  [affe](https://github.com/minad/affe) Asynchronous Fuzzy Finder for Emacs

    ```emacs-lisp
    (use-package affe
      :after orderless
      :config
      ;; Configure Orderless
      (setq affe-regexp-function #'orderless-pattern-compiler
            affe-highlight-function #'orderless--highlight)

      ;; Manual preview key for `affe-grep'
      (consult-customize affe-grep :preview-key (kbd "M-.")))
    ```

8.  provide

    ```emacs-lisp
    (provide 'rgr/minibuffer)
    ```


## Completion

Let emacs suggest completions

Raw:[rgr/completion](etc/elisp/rgr-completion.el)

```emacs-lisp
(require 'rgr/completion "rgr-completion" 'NOERROR)
```


### rgr-completion library

1.  Which Key

    [which-key](https://github.com/justbur/emacs-which-key) shows you what further key options you have if you pause on a multi key command.

    ```emacs-lisp
    (use-package
      which-key
      :demand t
      :config (which-key-mode))
    ```

2.  Yasnippet

    [YASnippet](https://github.com/joaotavora/yasnippet) is a template system for Emacs.

    ```emacs-lisp
    (use-package yasnippet
      :config
      (use-package yasnippet-snippets)
      (yas-global-mode))
    ```

3.  Abbrev Mode

    [Abbrev Mode](https://www.emacswiki.org/emacs/AbbrevMode#toc4) is very useful for expanding small text snippets

    ```emacs-lisp
    (setq-default abbrev-mode 1)
    ```

4.  [Orderless](https://github.com/oantolin/orderless) provides an orderless completion style that divides the pattern into space-separated components

    ```emacs-lisp
    (use-package orderless
      :init
      (setq completion-styles '(orderless)
            completion-category-defaults nil
            completion-category-overrides '((file (styles . (partial-completion))))))
    ```

5.  vertico , vertical interactive completion

    ```emacs-lisp
    ;; Enable vertico
    (use-package vertico
      :custom
      (vertico-cycle t)
      :init
      ;; Use `consult-completion-in-region' if Vertico is enabled
      (when (not (featurep 'corfu))
        (add-hook 'vertico-mode-hook (lambda ()
                                       (setq completion-in-region-function
                                             (if vertico-mode
                                                 #'consult-completion-in-region
                                               #'completion--in-region)))))
      (vertico-mode)
      :config
      (advice-add #'completing-read-multiple
                  :override #'consult-completing-read-multiple)
      (defun disable-selection ()
        (when (eq minibuffer-completion-table #'org-tags-completion-function)
          (setq-local vertico-map minibuffer-local-completion-map
                      completion-cycle-threshold nil
                      completion-styles '(basic))))
      (advice-add #'vertico--setup :before #'disable-selection))

    ```

6.  Abbrev Mode

    [Abbrev Mode](https://www.emacswiki.org/emacs/AbbrevMode#toc4) is very useful for expanding small text snippets

    ```emacs-lisp
    (setq-default abbrev-mode 1)
    ```

7.  provide

    ```emacs-lisp
    (provide 'rgr/completion)
    ```


## bookmarks


### bookmark+

```emacs-lisp
(use-package bookmark+
  :custom
  (bmkp-last-as-first-bookmark-file (no-littering-expand-var-file-name "bmkp/current-bookmark.el.gpg"))
  :demand)
```

1.  look into why bmkp store org link doesnt work


## Lazy Language Learning, lazy-lang-learn

```emacs-lisp
(use-package lazy-lang-learn
  :straight (lazy-lang-learn :local-repo "~/development/projects/emacs/lazy-lang-learn" :type git :host github :repo "rileyrg/lazy-lang-learn" )
  :bind
  ("C-c L" . lazy-lang-learn-mode)
  ("<f12>" . lazy-lang-learn-translate)
  ("S-<f12>" . lazy-lang-learn-translate-from-history))
```


## General configuration

Raw: [rgr/general-config](etc/elisp/rgr-general-config.el).

```emacs-lisp
(require  'rgr/general-config "rgr-general-config" 'NOERROR)
```


### library

1.  General

    1.  a - unfiled

        ```emacs-lisp
        (require 'iso-transl) ;; supposed to cure deadkeys when my external kbd is plugged into my thinkpad T44460.  It doesnt.
                                                ; t60
        (scroll-bar-mode -1)
        (tool-bar-mode -1)
        (menu-bar-mode -1)
        (show-paren-mode 1)
        (winner-mode 1)

        (global-auto-revert-mode t)
        ;; Also auto refresh dired, but be quiet about it
        (setq global-auto-revert-non-file-buffers t)
        (setq auto-revert-verbose nil)

        (global-visual-line-mode 1)

        (setq column-number-mode t)

        (delete-selection-mode 1)

        (global-set-key (kbd "S-<f1>") 'describe-face)
        (global-set-key (kbd "M-m") 'manual-entry)

        (global-set-key (kbd "S-<f10>") #'menu-bar-open)
                                                ;          (global-set-key (kbd "<f10>") #'imenu)


        (setq frame-title-format (if (member "-chat" command-line-args)  "Chat: %b" '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " — Emacs")))

        (defalias 'yes-or-no-p 'y-or-n-p)

        ;; ;; restore desktop
        (setq desktop-dirname (expand-file-name "desktop" user-emacs-directory))
        ;; (desktop-save-mode 1)

        (setq disabled-command-function nil)

        (global-hl-line-mode t)

        (use-package
          browse-url-dwim)

        ;; display dir name when core name clashes
        (require 'uniquify)

        (add-to-list 'Info-directory-list (expand-file-name "info" user-emacs-directory)) ;; https://www.emacswiki.org/emacs/ExternalDocumentation


        (global-set-key (kbd "C-c r") 'query-replace-regexp)

        ```

    2.  beacon

        visual feedback as to cursor position

        ```emacs-lisp
        (use-package beacon
          :custom
          (beacon-blink-delay 1)
          (beacon-size 10)
          (beacon-color "orange" nil nil "Customized with use-package beacon")
          (beacon-blink-when-point-moves-horizontally 32)
          (beacon-blink-when-point-moves-vertically 8)
          :config
          (beacon-mode 1))

        ```

    3.  blackout modeline

        Blackout is a package which allows you to hide or customize the display of major and minor modes in the mode line.

        ```emacs-lisp
        (straight-use-package
         '(blackout :host github :repo "raxod502/blackout"))
        ```

    4.  boxquote

        ```emacs-lisp
        (use-package boxquote
          :straight (:branch "main")
          :bind
          ("C-S-r" . boxquote-region))

        ```

    5.  volatile-highlights

        brings visual feedback to some operations by highlighting portions relating to the operations.

        ```emacs-lisp
        (use-package
          volatile-highlights
          :init (volatile-highlights-mode 1))
        ```

    6.  webpaste

        ```emacs-lisp
        (use-package
          webpaste
          :bind ("C-c y" . (lambda()(interactive)(call-interactively 'webpaste-paste-region)(deactivate-mark)))
          ("C-c Y" . webpaste-paste-buffer))

        ```

2.  Accessibility

    1.  fonts

        JetBrains fonts are nice. See [nerd-fonts](https://github.com/ryanoasis/nerd-fonts)

        ```emacs-lisp
        ;;(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :foundry "JB")
        ```

    2.  Darkroom

        Zoom in and center using [darkroom](https://github.com/joaotavora/darkroom).

        ```emacs-lisp
        (use-package
          darkroom
          :bind
          ( "<f7>" . 'darkroom-mode))
        ```

3.  Ansi colour

    [Ansi colour hooks](https://www.emacswiki.org/emacs/AnsiColor) to enable emacs buffers to handle ansi.

    ```emacs-lisp
    (require 'ansi-color)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
    ```

4.  Tabs

    1.  Tab Bar Mode

        ```emacs-lisp

        (defun consult-buffer-other-tab ()
          "Variant of `consult-buffer' which opens in other tab."
          (interactive)
          (let ((consult--buffer-display #'switch-to-buffer-other-tab))
            (consult-buffer)))

        (use-package tab-bar
          :defer t
          :custom
          (tab-bar-show t)
          (tab-bar-close-button-show nil)
          (tab-bar-new-button-show nil)
          (tab-bar-tab-hints t)
          (tab-bar-new-tab-choice "*scratch*")
          (tab-bar-select-tab-modifiers '(control))
          :custom-face
          (tab-bar ((t (:background "gray24" :foreground "#ffffff"))))
          (tab-bar-tab-inactive ((t (:background "gray24" :foreground "#ffffff"))))
          (tab-bar-tab ((t (:background "black" :foreground "#ffffff"))))
          :bind (:map tab-prefix-map
                      (("x" . tab-close)
                       ("b" . consult-buffer-other-tab)
                       ("p" . tab-previous)
                       ("n" . tab-next)
                       ("c" . tab-bar-new-tab)
                       ("s" . tab-bar-switch-to-tab))))
        ```

5.  Memory

    1.  save-place-mode, remember position in files

        ```emacs-lisp
        (save-place-mode +1)
        ```

    2.  save-hist-mode, save history

        ```emacs-lisp
        (savehist-mode 1)
        (add-to-list 'savehist-additional-variables 'kill-ring)
        (add-to-list 'savehist-additional-variables 'global-mark-ring)
        ;; (add-hook 'kill-emacs-hook 'rgr/unpropertize-kill-ring)
        ;; (defun rgr/unpropertize-kill-ring ()
        ;; (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

        ```

    3.  recentf-mode, remember recent files

        ```emacs-lisp
        (use-package recentf-ext
          :config
          (recentf-mode 1)
          ;;(setq savehist-minibuffer-history-variables (remove 'file-name-history savehist-minibuffer-history-variables))
          (if (featurep 'savehist)
              (add-to-list 'savehist-ignored-variables 'file-name-history))
          (if (featurep 'no-littering)
              (add-to-list 'recentf-exclude no-littering-var-directory)
            (add-to-list 'recentf-exclude no-littering-etc-directory)))
        ```

6.  provide

    ```emacs-lisp
    (provide 'rgr/general-config)
    ```


## Org functionality

General org-mode config

Raw: [rgr/org](etc/elisp/rgr-org.el)

```emacs-lisp
(require 'rgr/org "rgr-org" 'NOERROR)
```


### org library

1.  Org Mode, org-mode

    ```emacs-lisp

    (use-package org
      :demand t
      :custom
      (org-agenda-files (no-littering-expand-etc-file-name "org/agenda-files.txt"))
      (org-fontify-done-headline t)
      (org-fontify-todo-headline t)
      (org-babel-default-header-args:python
       '((:results  . "output")))
      (org-refile-use-outline-path 'file)
      (org-outline-path-complete-in-steps nil)
      :config
      (use-package org-contrib)
      (set-face-attribute 'org-headline-done nil :strike-through t)
      (defun rgr/org-agenda (&optional arg)
        (interactive "P")
        (let ((org-agenda-tag-filter-preset '("-trash")))
          (org-agenda arg "a")))
      :bind
      ("C-c a" . org-agenda)
      ("C-c A" . rgr/org-agenda)
      ("C-c c" . org-capture)
      ("C-c l" . org-store-link)
      ("C-c C-l" . org-insert-link)
      ("C-c C-s" . org-schedule)
      ("C-c C-t" . org-todo)
      (:map org-mode-map  ("M-." . find-function-at-point)
            ("<f11>" . org-edit-special))
      (:map org-src-mode-map ("<f11>" . org-edit-src-exit)))


    ```

    1.  org-id

        create unique link IDs when sharing a link to an org section

        ```emacs-lisp
        (require 'org-id)
        ```

    2.  crypt

        ```emacs-lisp
        (require 'org-crypt)
        (org-crypt-use-before-save-magic)
        ```

    3.  async babel blocks

        ```emacs-lisp
        (use-package ob-async)
        ```

    4.  org-super-agenda

        ```emacs-lisp
        (use-package org-super-agenda
          :custom
          (org-super-agenda-groups
               '(;; Each group has an implicit boolean OR operator between its selectors.
                 (:name "Today"  ; Optionally specify section name
                        :time-grid t  ; Items that appear on the time grid
                        :todo "TODAY")  ; Items that have this TODO keyword
                 (:name "Important"
                        ;; Single arguments given alone
                        :tag "bills"
                        :priority "A")
                 ;; Set order of multiple groups at once
                 (:order-multi (2 (:name "home"
                                         ;; Boolean AND group matches items that match all subgroups
                                         :and (:tag "@home"))
                                  (:name "caravan"
                                         ;; Boolean AND group matches items that match all subgroups
                                         :and (:tag "@caravan"))
                                  (:name "shopping all"
                                         ;; Boolean AND group matches items that match all subgroups
                                         :and (:tag "shopping" :not (:tag "@home @caravan")))
                                  (:name "shopping"
                                         ;; Boolean AND group matches items that match all subgroups
                                         :and (:tag "shopping" :not (:tag "@home @caravan")))
                                  (:name "Emacs related"
                                         ;; Boolean AND group matches items that match all subgroups
                                         :tag ("emacs"))
                                  (:name "Programming related"
                                         :and (:tag ("programming") :not (:tag "emacs")))
                                  (:name "Food-related"
                                         ;; Multiple args given in list with implicit OR
                                         :tag ("food" "dinner" "lunch" "breakfast"))
                                  (:name "Personal"
                                         :habit t
                                         :tag "personal")
                                  ))
                 ;; Groups supply their own section names when none are given
                 (:todo "WAITING" :order 8)  ; Set order of this section
                 (:todo "STARTED" :order 8)
                 (:todo ("SOMEDAY" "TOREAD" "CHECK" "TO-WATCH" "WATCHING")
                        ;; Show this group at the end of the agenda (since it has the
                        ;; highest number). If you specified this group last, items
                        ;; with these todo keywords that e.g. have priority A would be
                        ;; displayed in that group instead, because items are grouped
                        ;; out in the order the groups are listed.
                        :order 9)
                 (:priority<= "B"
                              ;; Show this section after "Today" and "Important", because
                              ;; their order is unspecified, defaulting to 0. Sections
                              ;; are displayed lowest-number-first.
                              :order 1)
                 ;; After the last group, the agenda will display items that didn't
                 ;; match any of these groups, with the default order position of 99
                 ))
          :init
          (org-super-agenda-mode))
        ```

    5.  org-roam

        Inspired by another System Crafters [video](https://systemcrafters.cc/build-a-second-brain-in-emacs/getting-started-with-org-roam/).

        ```emacs-lisp
        (use-package org-roam
          :demand
          :custom
          (epa-file-encrypt-to "rileyrg")
          (epa-file-select-keys "auto")
          (org-roam-dailies-directory "daily/")
          (org-roam-capture-templates
           '(("d" "default" plain "%?" :if-new
              (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}
        ")
              :unnarrowed t)))
          (org-roam-dailies-capture-templates
           '(("d" "default" entry
              "* %?"
              :if-new (file+head "%<%Y-%m-%d>.org.gpg"
                                 "#+title: %<%Y-%m-%d>\n"))))
          (org-roam-directory (no-littering-expand-var-file-name "org/org-roam"))
          :bind (("C-c n l" . org-roam-buffer-toggle)
                 ("C-c n f" . org-roam-node-find)
                 ("C-c n a" . org-roam-alias-add)
                 ("C-c n r" . org-roam-add-ref)
                 ("C-c n g" . org-roam-graph)
                 ("C-c n i" . org-roam-node-insert)
                 ("C-c n c" . org-roam-capture)
                 ("C-c n C" . org-roam-capture-tomorrow)
                 :map org-mode-map
                 ("C_M-i" . completion-at-point)
                 :map org-roam-dailies-map
                 ("Y" . org-roam-dailies-capture-yesterday)
                 ("T" . org-roam-dailies-capture-tomorrow))
          :bind-keymap
          ("C-c n d" . org-roam-dailies-map)
          :config
          (org-roam-setup)
          (require 'org-roam-dailies)
          ;; If using org-roam-protocol
          (require 'org-roam-protocol))
        ```

    6.  github compliant markup

        ```emacs-lisp
        (use-package
          ox-gfm
          :demand)
        ```

2.  provide

    ```emacs-lisp
    (provide 'rgr/org)
    ```


### org agenda files

See `org-agenda-files` [org-agenda-files](#org80c0328) maintain a file pointing to agenda sources : NOTE, NOT tangled. ((no-littering-expand-etc-file-name "org/agenda-files.txt"))

```conf
~/.emacs.d/var/org/orgfiles
~/.emacs.d/var/org/orgfiles/journals
~/.emacs.d/var/org/orgfiles/projects
~/development/education/lessons
~/development/education/lessons/bash
~/development/education/lessons/python
~/development/education/lessons/python/coreyschafer
~/development/education/lessons/python/python-lernen.de
~/development/education/lessons/elisp
```


## Text tools


### emjois

<https://github.com/iqbalansari/emacs-emojify>

```emacs-lisp
(use-package emojify
  :init
  (global-emojify-mode))
```


### Cursor/Region related

1.  General

    ```emacs-lisp
    (defun centreCursorLineOn()
      "set properties to keep current line approx at centre of screen height. Useful for debugging."
      ;; a faster more concise alternative to MELPA's centered-cursor-mode
      (interactive)
      (setq  scroll-preserve-screen-position_t scroll-preserve-screen-position scroll-conservatively_t
             scroll-conservatively maximum-scroll-margin_t maximum-scroll-margin scroll-margin_t
             scroll-margin)
      (setq scroll-preserve-screen-position t scroll-conservatively 0 maximum-scroll-margin 0.5
            scroll-margin 99999))

    (defun centreCursorLineOff()
      (interactive)
      (setq  scroll-preserve-screen-position scroll-preserve-screen-position_t scroll-conservatively
             scroll-conservatively_t maximum-scroll-margin maximum-scroll-margin_t scroll-margin
             scroll-margin_t))

    ```


### Folding/Hide Show

[hs-minor-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html) allows hiding and showing different blocks of text/code (folding).

```emacs-lisp
(use-package hideshow
  :config
  (defun toggle-selective-display (column)
    (interactive "P")
    (set-selective-display
     (or column
         (unless selective-display
           (1+ (current-column))))))
  (defun toggle-hiding (column)
    (interactive "P")
    (if hs-minor-mode
        (if (condition-case nil
                (hs-toggle-hiding)
              (error t))
            (hs-show-all))
      (toggle-selective-display column)))
  (add-hook 'prog-mode-hook (lambda()(hs-minor-mode t)))
  :bind ( "C-+" . toggle-hiding)
  ("C-\\" . toggle-selective-display))

```

\#+end\_src


### flyspell

```emacs-lisp
(use-package flyspell
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word)
    )

  :bind (("C-<f8>" . flyspell-mode)
         ("S-<f8>" . flyspell-check-previous-highlighted-word)
         ("C-S-<f8>" . flyspell-buffer)
         ("M-<f8>" . flyspell-word)
         ))


```


### rg, ripgrep

rg is pretty quick

```emacs-lisp
(use-package
  ripgrep)
```


## Reference/Lookup/Media

lookup and reference uilities and config

Raw: [rgr/reference](etc/elisp/rgr-reference.el)

```emacs-lisp
(require 'rgr/reference "rgr-reference" 'NOERROR)
```


### reference library

1.  web browsing

    Set up default emacs browsing - I like eww but palming off some URLs to external browser.

    ```emacs-lisp
    (custom-set-variables
     '(eww-search-prefix "https://google.com/search?q=")
     '(browse-url-browser-function 'eww-browse-url)
     '(browse-url-generic-program "google-chrome")
     '(browse-url-secondary-browser-function 'browse-url-default-browser))
    ```

    1.  eww

        Advice EWW to launch certain URLs using the generic launcher rather than EWW.

        ```emacs-lisp
        (defcustom rgr/eww-external-launch-url-chunks '("youtube")
          "If any component of this list is contained in an EWW url then it will use `browse-url-generic to launch that url instead of `eww"
          :type '(repeat string))

        (defadvice eww (around rgr/eww-extern-advise activate)
          "Use `browse-url-generic if any part of URL is contained in `rgr/eww-external-launch-url-chunks"
          (if (string-match-p (regexp-opt rgr/eww-external-launch-url-chunks) url)
              (browse-url-generic url)
          ad-do-it))
        ```

2.  Google related

    Raw:[rgr/google](etc/elisp/rgr-google.el)

    ```emacs-lisp
    (require 'rgr/google "rgr-google")
    ```

    1.  google utils code

        1.  Google This

            [google-this](https://melpa.org/#/google-this) includes an interface to [google translate](https://translate.google.com/).

            ```emacs-lisp
            (use-package
              google-this
              :after org
              :custom
              (google-this-keybind "g")
              :config
              (google-this-mode 1))
            ```

        2.  Google translate

            ```emacs-lisp
            (use-package google-translate

              :init
              (require 'google-translate)

              :custom
              (google-translate-backend-method 'curl)
              (google-translate-pop-up-buffer-set-focus t)
              :config

              (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

              (defun google-translate-swap-default-languages()
                "swap google-translate default languages"
                (setq google-translate-default-source-language  (prog1 google-translate-default-target-language (setq google-translate-default-target-language  google-translate-default-source-language))))

              (defun rgr/google-translate-in-history-buffer(&optional phrase)
                (interactive)
                (when current-prefix-arg
                  ;;swap source and dest languages
                  (google-translate-swap-default-languages))
                (let  ((phrase (if phrase phrase (rgr/region-symbol-query))))
                  (switch-to-buffer "*Google Translations*")
                  ;; need to make aminor mode and do this properly based on file - org-mode?
                  (local-set-key (kbd "<return>") (lambda() (interactive)
                                                    (goto-char(point-max))
                                                    (newline)))
                  (unless (= (current-column) 0)
                    (end-of-line)
                    (newline))
                  (insert  (format "<%s>: %s" (format-time-string "%Y-%m-%d %T") phrase))
                  (rgr/google-translate-at-point)))

              (defun rgr/google-translate-at-point()
                "reverse translate word/region if prefix"
                (interactive)
                (when current-prefix-arg
                  ;;swap source and dest languages
                  (google-translate-swap-default-languages))
                (google-translate-at-point)
                (if google-translate-pop-up-buffer-set-focus
                    (select-window (display-buffer "*Google Translate*"))))

              (defun rgr/google-translate-query-translate()
                "reverse translate input if prefix"
                (interactive)
                (when current-prefix-arg
                  ;;swap source and dest languages
                  (google-translate-swap-default-languages))
                (google-translate-query-translate)
                (if google-translate-pop-up-buffer-set-focus
                    (select-window (display-buffer "*Google Translate*"))))

              :bind
              ("C-c T" . rgr/google-translate-at-point)
              ("C-c t" . rgr/google-translate-query-translate)
              ("C-c b" . rgr/google-translate-in-history-buffer))


            ```

        3.  provide

            ```emacs-lisp
            (provide 'rgr/google)
            ```

3.  Reference and dictionary

    The aim here is to link to different reference sources and have a sensible default for different modes. eg elisp mode would use internal doc sources, whereas javascript uses Dash/Zeal or even a straight URL search to lookup help. On top of that provide a list of other sources you can call by prefixing the core lookup-reference-dwim call. But if you lookup internal docs and it doesnt exist then why not farm it out to something like Goldendict which you can configure to look wherever you want? Examples here show Goldendict plugged into google translate amonst other things. The world's your oyster.

    1.  utility funcs

        ```emacs-lisp

        (defgroup rgr/lookup-reference nil
          "Define functions to be used for lookup"
          :group 'rgr)

        (defcustom mode-lookup-reference-functions-alist '(
                                                           (nil (goldendict-dwim goldendict-dwim))
                                                           (c-mode  (rgr/devdocs rgr/devdocs))
                                                           (c++-mode  (rgr/devdocs rgr/devdocs))
                                                           (gdscript-mode  (rgr/devdocs rgr/devdocs))
                                                           ;;                                                         (gdscript-mode  (rgr/gdscript-docs-browse-symbol-at-point rgr/devdocs))
                                                           (php-mode  (rgr/devdocs rgr/devdocs))
                                                           (web-mode  (rgr/devdocs rgr/devdocs))
                                                           (org-mode (rgr/elisp-lookup-reference-dwim))
                                                           (Info-mode (rgr/elisp-lookup-reference-dwim))
                                                           (js2-mode (rgr/devdocs rgr/devdocs))
                                                           (python-mode (rgr/devdocs rgr/devdocs))
                                                           (js-mode (rgr/devdocs rgr/devdocs))
                                                           (rjsx-mode (rgr/devdocs rgr/devdocs))
                                                           (typescript-mode (rgr/devdocs rgr/devdocs))
                                                           (lisp-interaction-mode (rgr/elisp-lookup-reference-dwim rgr/devdocs))
                                                           (emacs-lisp-mode (rgr/elisp-lookup-reference-dwim rgr/devdocs)))
          "mode lookup functions"
          :group 'rgr/lookup-reference)

        (defun get-mode-lookup-reference-functions(&optional m)
          (let* ((m (if m m major-mode))
                 (default-funcs (copy-tree(cadr (assoc nil mode-lookup-reference-functions-alist))))
                 (mode-funcs (cadr (assoc m mode-lookup-reference-functions-alist))))
            (if mode-funcs (progn
                             (setcar default-funcs (car mode-funcs))
                             (if (cadr mode-funcs)
                                 (setcdr default-funcs (cdr mode-funcs)))))
            default-funcs)) ;; (get-mode-lookup-reference-functions 'org-mode)

        (defcustom linguee-url-template "https://www.linguee.com/english-german/search?source=auto&query=%S%"
          "linguee url search template"
          :type 'string
          :group 'rgr/lookup-reference)

        (defcustom php-api-url-template "https://www.google.com/search?q=php[%S%]"
          "php api url search template"
          :type 'string
          :group 'rgr/lookup-reference)

        (defcustom jquery-url-template "https://api.jquery.com/?s=%S%"
          "jquery url search template"
          :type 'string
          :group 'rgr/lookup-reference)

        (defcustom  lookup-reference-functions '(rgr/describe-symbol goldendict-dwim rgr/linguee-lookup rgr/dictionary-search google-this-search)
          "list of functions to be called via C-n prefix call to lookup-reference-dwim"
          :type 'hook
          :group 'rgr/lookup-reference)

        (defun sys-browser-lookup(w template)
          (interactive)
          (browse-url-xdg-open (replace-regexp-in-string "%S%" (if w w (rgr/region-symbol-query)) template)))

        (defun rgr/describe-symbol(w)
          (interactive (cons (rgr/region-symbol-query) nil))
          (let ((s (if (symbolp w) w (intern-soft w))))
            (if s (describe-symbol s)
              (message "No such symbol: %s" w))))

        (defun rgr/linguee-lookup(w)
          (interactive (cons (rgr/region-symbol-query) nil))
          (sys-browser-lookup w linguee-url-template))

        (defun rgr/gdscript-docs-browse-symbol-at-point(&optional w)
          (gdscript-docs-browse-symbol-at-point))

        (defun lookup-reference-dwim(&optional secondary)
          "if we have a numeric prefix then index into lookup-reference functions"
          (interactive)
          (let((w (rgr/region-symbol-query))
               ;; PREFIX integer including 4... eg C-2 lookup-reference-dwim
               (idx (if (and  current-prefix-arg (not (listp current-prefix-arg)))
                        (- current-prefix-arg 1)
                      nil)))
            (if idx (let((f (nth idx lookup-reference-functions)))
                      (funcall (if f f (car lookup-reference-functions)) w))
              (let* ((funcs (get-mode-lookup-reference-functions))
                     (p (car funcs))
                     (s (cadr funcs)))
                (if (not secondary)
                    (unless (funcall p w)
                      (if s (funcall s w)))
                  (if s (funcall s w)))))))

        (defun lookup-reference-dwim-secondary()
          (interactive)
          (lookup-reference-dwim t))

        (bind-key* "C-q" 'lookup-reference-dwim) ;; overrides major mode bindings
        (bind-key* "C-S-q" 'lookup-reference-dwim-secondary)

        ```

    2.  Dictionary

        The more emacsy [Dictionary](https://melpa.org/#/dictionary) .

        ```emacs-lisp
        (use-package
          dictionary
          :commands (rgr/dictionary-search)
          :config
          (use-package mw-thesaurus)
          (defun rgr/dictionary-search(&optional w)
            (interactive)
            (dictionary-search (if w w (rgr/region-symbol-query))))
          :bind
          ("<f6>" . rgr/dictionary-search)
          ("S-<f6>" . mw-thesaurus-lookup-at-point))
        ```

        1.  Requires dictd.

            ```bash
            sudo apt install dictd
            sudo apt install dict-de-en
            ```

    3.  Elisp reference

        1.  quick help for function etc at point

            If an elisp object is there it brings up the internal docs:

            ![img](./images/lookup-internal-doc.png "lookup using internal docs")

            else it palms it off to goldendict.

            ![img](./images/lookup-goldendict.png "lookup using goldendict")

            ```emacs-lisp
            (defun rgr/elisp-lookup-reference-dwim (&optional sym)
              "Checks to see if the 'thing' is known to elisp and, if so, use internal docs and return symbol else return nil to signal maybe fallback"
              (interactive)
              (let* ((sym (if sym sym (rgr/region-symbol-query)))
                     (sym (if (symbolp sym) sym (intern-soft sym))))
                (when sym
                  (if (fboundp sym)
                      (if (featurep 'helpful)
                          (helpful-function sym)
                        (describe-function sym))
                    (if (boundp sym)
                        (if (featurep 'helpful)
                            (helpful-variable sym)
                          (describe-variable sym))
                      (progn
                        (let ((msg (format "No elisp help for '%s" sym)))
                          (if (featurep 'alert)
                              (alert msg)
                            (message msg)))
                        (setq sym nil)))))
                sym))
            ```

    4.  GoldenDict - external lookup and reference

        When using goldendict-dwim why not add your program to the wonderful [GoldenDict](http://goldendict.org/)? A call to [trans-shell](https://github.com/soimort/translate-shell) in the dictionary programs tab gives us google translate:-

        ```bash
        trans -e google -s de -t en -show-original y -show-original-phonetics n -show-translation y -no-ansi -show-translation-phonetics n -show-prompt-message n -show-languages y -show-original-dictionary n -show-dictionary n -show-alternatives n "%GDWORD%"
        ```

        ```emacs-lisp
        (use-package
          goldendict
          :commands (goldendict-dwim)
          :config
          (defun goldendict-dwim
              (&optional
               w)
            "lookup word at region, thing at point or prompt for something, in goldendict. Use a prefix to force prompting."
            (interactive)
            (let ((w (if w w (rgr/region-symbol-query))))
              (call-process-shell-command (format  "goldendict \"%s\"" w ) nil 0)))
          :bind (("C-x G" . goldendict-dwim)))
        ```

    5.  emacs-devdocs-browser

        <https://github.com/blahgeek/emacs-devdocs-browser> : Browse devdocs.io documents inside Emacs!

        ```emacs-lisp
        (use-package devdocs-browser
          :custom
          (devdocs-browser-cache-directory (no-littering-expand-var-file-name  "devdocs-browser"))
          :config
          (defun rgr/devdocs(&optional i)
            (interactive)
            (if current-prefix-arg
                (call-interactively 'devdocs-browser-open-in)
              (devdocs-browser-open))))
        ```

4.  Man Pages

    Use emacsclient if it's running. Might consider an alias

    ```conf
    alias man="eman"
    ```

    ```bash
    #!/usr/bin/bash
    # Maintained in emacs-config.org
    mp=${1:-"man"}
    pgrep -x emacs > /dev/null && emacsclient -c -e "(manual-entry \"-a ${mp}\"))" &> /dev/null || man "$@"
    ```

5.  Elfeed

    [Elfeed](https://github.com/skeeto/elfeed) is an extensible web feed reader for Emacs, supporting both Atom and RSS.

    ```emacs-lisp
    (use-package elfeed
      :config
      (use-package elfeed-org
        :ensure t
        :custom
        (rmh-elfeed-org-files (list (no-littering-expand-etc-file-name "elfeed/elfeed.org")))
        :config
        (elfeed-org))
      (use-package elfeed-goodies
        :disabled
        :config
        (elfeed-goodies/setup))
      (run-at-time nil (* 8 60 60) #'elfeed-update)
      :bind
      ( "C-c w" . elfeed)
      (:map elfeed-show-mode-map
            ("&" . (lambda()(interactive)(message "opening in eternal browser")(elfeed-show-visit t))))
      (:map elfeed-search-mode-map
            ("&" . (lambda()(interactive)(message "opening in eternal browser")(elfeed-search-browse-url t)))))
    ```

    1.  elfeed-org

        ```emacs-lisp

        ```

6.  impatient-showdow, markdown view live

    Preview markdown buffer live over HTTP using showdown. <https://github.com/jcs-elpa/impatient-showdown>

    ```emacs-lisp
    (use-package impatient-showdown
      :hook (markdown-mode . impatient-showdown-mode))
    ```

7.  provide

    ```emacs-lisp
    (provide 'rgr/reference)
    ```


## EMMS

[Emms](https://github.com/skeeto/elfeed) is the Emacs Multimedia System. Emms displays and plays multimedia from within GNU/Emacs using a variety of external players and from different sources.

Raw:[rgr/emms](./etc/elisp/rgr-emms.el)

```emacs-lisp
(require 'rgr/emms "rgr-emms" 'NOERROR)
```


### rgr/emms library

```emacs-lisp
(use-package
  emms
  :disabled t
  :custom
  (emms-source-file-default-directory "~/Music" emms-info-asynchronously t emms-show-format "♪ %s")
  (emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  (emms-history-start-playing nil)
  :config
  (defun rgr/emms-play-url()
    (interactive)
    (let* ((url (thing-at-point-url-at-point))
           (url (if (and (not current-prefix-arg)
                         url) url (read-string (format "URL to play %s: " (if url url "")) nil
                         nil url))))
      (message "Playing: %s" url)
      (kill-new url)
      (emms-play-url url)))
  (defun rgr/emms-play-playlist()
    (interactive)
    (let(( emms-source-file-default-directory (expand-file-name "Playlists/" emms-source-file-default-directory)))
      (call-interactively 'emms-play-playlist)))
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (require 'emms-history)
  (emms-history-load)
  :bind ("C-c e e" . #'emms-smart-browse)
  ("C-c e j" . #'emms-seek-backward)
  ("C-c e l" . #'emms-seek-forward)
  ("C-c e p" . #'rgr/emms-play-playlist)
  ;;        ("C-c e p" . #'emms-play-playlist)
  ("C-c e <SPC>" . #'emms-pause)
  ("C-c e o" . #'rgr/emms-play-url)
  (:map emms-playlist-mode-map
        ("<SPC>" . #'emms-pause)
        ("j" . #'emms-seek-backward)
        ("l" . #'emms-seek-forward)
        ("k" . #'emms-pause)))
(provide 'rgr/emms)
```


## Shells and Terminals


### Eshell

[EShell](https://www.masteringemacs.org/article/complete-guide-mastering-eshell) is, amongst other things, convenient for cat/console debugging in Symfony etc to have all output in easily browsed Emacs buffers via [EShell redirection](https://www.emacswiki.org/emacs/EshellRedirection).

1.  Eshell functions

    1.  Bootstrap  clean emacs

        ```emacs-lisp
        (defun eshell/emacs-clean (&rest args)
          "run a clean emacs"
          (interactive)
          (message "args are %s" args)
          (save-window-excursion
            (shell-command "emacs -Q -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el &")))
        ```

        1.  ftrace - debugging the kernel utility funtions

            1.  run a function trace

                ```emacs-lisp
                (defun eshell/_ftrace_fn (&rest args)
                  "useage: _ftrace_fn &optional function-name(def:printf)  depth(def:1)
                creates a report in function-name.ftrace and opens it in a buffer"
                  (interactive)
                  (let ((fn (or (nth 2 args) "printf"))
                        (depth (or (nth 3 args) 1)))
                    (shell-command (format "sudo trace-cmd record -p function_graph --max-graph-depth %s -e syscalls -F %s && trace-cmd report | tee %s.ftrace" depth fn fn))
                    (switch-to-buffer (find-file-noselect (format "%s.ftrace" fn) ))))
                ```

2.  EShell Aliases

    Be sure to check out [Aliases](http://www.howardism.org/Technical/Emacs/eshell.html). Aliases are very powerful allowing you to mix up shell script, elisp raw and elisp library function. My current [alias file](eshell/alias) (subject to change&#x2026;) is currently, at this time of discovery:-

    ```bash
    alias HOME $*
    alias god cd ~/bin/thirdparty/godot
    alias in ssh intel-nuc
    alias prj cd ~/development/Symfony/the_spacebar/
    alias gs git status
    alias clconf find ~/Dropbox/ -path "*(*s conflicted copy [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]*" -exec rm -f {} \;
    alias twigs grep -i "twig.*"$1 *
    alias rgr rg --color=never $* > #<*ripgrep*>; switch-to-buffer "*ripgrep*"
    alias to httplug.client.app.http_methods
    alias grep grep --color=always --exclude="*.lock" --exclude-dir=log --exclude-dir=cache -iR $*
    alias gg *grep -C 2 -iR $*
    alias awg aw|*grep -C 2 -i $*
    alias dconp co debug:container  --show-private $*
    alias dcon co debug:container  $*
    alias dc co debug:config $*
    alias cc co cache:clear
    alias ll ls -l $*
    alias aw co debug:autowiring
    alias cdu co config:dump $1
    alias co bin/console --no-ansi $*
    alias em cd ~/.emacs.d
    alias dcg dc $1 |*grep -C 5 -i $2
    alias coenv co about
    alias R/W multiple sector transfer: Max = 1 Current = 1
    alias mlg ag -o -i --no-color -U --smart-case "(?=(?:.|\n)*?$1)(?:.|\n)*?$2" . > #<*mlg*> && switch-to-buffer "*mlg*"
    alias csr console server:run
    alias dr co debug:router
    alias dconparm dcon --parameters
    alias cr composer recipes $*
    alias property: $*
    alias gds cd ~/.emacs.d/straight/repos/emacs-gdscript-mode
    alias tcfr trace-cmd report > $1
    ```

3.  EShell Config

    ```emacs-lisp
    (use-package
      eshell
      :init
      (require 'em-hist)
      (require 'em-tramp)
      (require 'em-smart)
      :config
      (defun eshell-mode-hook-func ()
        ;; (setq eshell-path-env (concat "/home/rgr/bin:" eshell-path-env))
        ;; (setenv "PATH" (concat "/home/rgr/bin:" (getenv "PATH")))
        (setq pcomplete-cycle-completions nil))
      (add-to-list 'eshell-modules-list 'eshell-tramp)
      (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
      (setq eshell-review-quick-commands nil)
      (setq eshell-smart-space-goes-to-end t)

      (use-package
        eshell-git-prompt
        :config
        (eshell-git-prompt-use-theme 'powerline)
        (define-advice
            eshell-git-prompt-powerline-dir
            (:override ()
                       short)
          "Show only last directory."
          (file-name-nondirectory (directory-file-name default-directory)))))
    ```


### vterm

<https://github.com/akermu/emacs-libvterm>

```emacs-lisp
(use-package vterm
  :custom
  (vterm-shell "/usr/bin/zsh")
  (vterm-max-scrollback 100000)
  :bind
  ("M-g v" . vterm))
```


### Docker

A general interface to [docker](https://github.com/Silex/docker.el/tree/a2092b3b170214587127b6c05f386504cae6981b).

```emacs-lisp
(use-package docker)
```


## Buffers and Windows


### toggle buffer

```emacs-lisp
(defun rgr/toggle-buffer(n)
  "jump to or from buffer named n else default to *Messages*"
  (interactive "bbuffer:")
  (let ((n (or n
               "*Messages*")))
    (switch-to-buffer (if (string= (buffer-name) n)
                          (other-buffer) n))))
```


### General

1.  perspective

    project aware buffer handling <https://www.youtube.com/watch?v=uyMdDzjQFMU&ab_channel=SystemCrafters>

    ```emacs-lisp
    (use-package perspective
      :custom
      (persp-state-default-file (no-littering-expand-var-file-name "perspective/perspectile.el"))
      :config
      (persp-mode)
      (add-hook 'kill-emacs-hook  #'persp-state-save)
      :bind
      ("C-x C-b" . persp-list-buffers))
    ```

2.  buffer deletion - but keep scratch and messages!

    ```emacs-lisp
    (use-package emacs
      :demand
      :config
      (defun rgr/kill-current-buffer()
        (interactive)
        (if (member (buffer-name) '("*Messages*" "*scratch*"))
            (progn
              (message "Can't delete %s. Are you mad? Closing window instead." (buffer-name))
              (delete-window))
          (kill-current-buffer)))
      (add-hook 'before-save-hook 'delete-trailing-whitespace)
      :bind
      ("C-x k" . rgr/kill-current-buffer)
      ("M-0" . 'delete-window)
      ("M-1" . 'delete-other-windows))
    ```


### dired - emacs file management

1.  dired icons

    ```emacs-lisp
    (use-package all-the-icons-dired
      :init
      (add-hook 'dired-mode-hook  #'all-the-icons-dired-mode))
    ```

2.  Dired Git Info

    ```emacs-lisp
    (use-package dired-git
      :config
      :hook (dired-mode . dired-git-mode))
    ```

3.  dired hacks

    Collection of useful dired additions found on github [here](https://github.com/Fuco1/dired-hacks). Found out about it at the useful emacs resource [**Pragmatic Emacs**](http://pragmaticemacs.com/category/dired/).

    1.  dired subtree

        ```emacs-lisp
        (use-package dired-subtree
          :bind (:map dired-mode-map
                      ("i" . dired-subtree-insert)
                      (";" . dired-subtree-remove)))
        ```

    2.  dired filter

        More dired based filtering see [dired-filter-prefix](dired-filter-prefix)

        ```emacs-lisp
        (use-package dired-filter
          :init
          (define-key dired-mode-map (kbd "/") dired-filter-map))

        ```


### PopUp Utilities

1.  posframe

    [Posframe](https://github.com/tumashu/posframe) can pop up a frame at point, this posframe is a child-frame connected to its root window's buffer.

    ```emacs-lisp
    (use-package posframe)
    ```

2.  popper

    [Popper](https://github.com/karthink/popper) is a minor-mode to tame the flood of ephemeral windows Emacs produces, while still keeping them within arm’s reach. Designate any buffer to “popup” status, and it will stay out of your way.

    ```emacs-lisp
    (use-package popper
      :ensure t
      :init
      (use-package posframe)
      ;;(setq popper-display-function 'rgr/popper-display-posframe)
      (setq popper-reference-buffers
            '(
              "\\*Messages\\*"
              magit-mode
              ;;      help-mode
              helpful-mode
              inferior-python-mode
              dictionary-mode
              compilation-mode))
      (popper-mode +1)
      :bind (("C-`"   . popper-toggle-latest)
             ("M-`"   . popper-cycle)
             ("C-M-`" . popper-toggle-type)))
    ```


### Transpose windows, transpose-frame

```emacs-lisp
(use-package transpose-frame
  :disabled t
  :config
  (defun window-split-toggle ()
    "Toggle between horizontal and vertical split with two windows."
    (interactive)
    (if (> (length (window-list)) 2)
        (error "Can't toggle with more than 2 windows!")
      (let ((func (if (window-full-height-p)
                      #'split-window-vertically
                    #'split-window-horizontally)))
        (delete-other-windows)
        (funcall func)
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))))
  :bind
  ("C-M-t" . transpose-frame)
  ("C-c T" . window-split-toggle)
  )
```


### Hyperbole

[Hyperbole](https://www.emacswiki.org/emacs/Hyperbole) is more a window management system from what I can see. Need to explore it.

```emacs-lisp
(use-package
  hyperbole
  :disabled t)
```


### Undo utilities

1.  undohist

    [undo-hist](https://melpa.org/#/undohist) provides persistent undo across sessions.

    ```emacs-lisp
    (use-package undohist
      :disabled t
      :config
      (undohist-initialize))
    ```

2.  undo-tree

    [undo-tree](https://github.com/apchamberlain/undo-tree.el) visualises the sometimes complex undo ring and allow stepping along the timeline

    ```emacs-lisp
    (use-package undo-tree
      :disabled t
      :config
      (global-undo-tree-mode))
    ```

3.  undo-fu

    ```emacs-lisp
    (use-package undo-fu
      :disabled t
      :init
      (global-unset-key (kbd "C-z"))
      (global-set-key (kbd "C-z")   'undo-fu-only-undo)
      (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))
    ```


### Navigation

1.  Back Button

    [Back-Button](https://github.com/rolandwalker/back-button) provides better navigation on the [local and global mark rings](https://www.gnu.org/software/emacs/manual/html_node/emacs/Mark-Ring.html). The jury is still out on this one.

    ```emacs-lisp
    (use-package back-button
      :disabled t
      :config
      (back-button-mode 1)
      :bind
      ("M-<left>" . previous-buffer)
      ("M-<right>" . next-buffer))
    ```

2.  Window hopping

    1.  [Ace-Window](https://github.com/abo-abo/ace-window) provides better window switching.

        ```emacs-lisp
        (use-package ace-window
          :init
          (defalias 'other-window 'ace-window)
          :bind*
          ("M-o" . ace-window)
          ("M-S o" . ace-delete-window))
        ```

3.  hopping around links

    Quickly follow [links](https://github.com/abo-abo/ace-link) in Emacs.

    ```emacs-lisp
    (use-package ace-link
      :demand t
      :config
      (ace-link-setup-default)
      :bind*
      (:map emacs-lisp-mode-map
            ("C-c o" . ace-link-addr))
      ("C-c o" . ace-link)
      )
    ```

4.  hopping around in the buffer

    Allows word, char and line hopping. The [wiki](https://github.com/winterTTr/ace-jump-mode/wiki) is a food source of info.

    ```emacs-lisp
    (use-package ace-jump-mode
      :bind
      ("M-s c" . ace-jump-mode)
      )
    ```


## System


### htop interface

```emacs-lisp
(defun htop-regexp()
  (interactive)
  (let ((s (completing-read (format "HTtop filter (%s): " (symbol-at-point)) minibuffer-history nil nil (symbol-at-point))))
    (condition-case nil
        (shell-command (format "htop-regexp %s" s))
      (error nil))))
(global-set-key (kbd "C-S-p") 'htop-regexp)
```


### explain-pause-mode

[explain-pause-mode](https://github.com/lastquestion/explain-pause-mode) is like an htop for emacs itself

```emacs-lisp
(use-package explain-pause-mode
  :disabled
  :config
  (explain-pause-mode))
```


## Treemacs

```emacs-lisp
(use-package
  treemacs
  :custom
  (treemacs-follow-after-init t)
  :config
  (treemacs-follow-mode +1)
  (treemacs-fringe-indicator-mode)
  (treemacs-git-mode 'deferred)
  (use-package treemacs-magit)
  :bind
  ("M-9"   . 'treemacs-select-window)
  (:map treemacs-mode-map
        ("<right>" . treemacs-peek)))

```


## Online Chats


### irc/erc

```emacs-lisp

(defun rgr/erc-switch-to-channel(&optional channel)
  (when (string= (or channel "#emacs") (buffer-name (current-buffer)))
    (switch-to-buffer (current-buffer))))

(defun rgr/erc-start()
  (interactive)
  (if (not (get-buffer "irc.libera.chat:6697"))
    (progn
      (erc-tls :server "irc.libera.chat" :port "6697")
      ;;(erc-tls :server "irc.freenode.net" :port "6667")
      (erc-tls :server "irc.oftc.net" :port "6697")
      (add-hook 'erc-join-hook 'rgr/erc-switch-to-channel))
    (erc-switch-to-buffer)))

(require 'erc)
(global-set-key (kbd  "C-c e") #'rgr/erc-start)
```


## Email


### mu4e

```emacs-lisp
(use-package mu4e
  :disabled
  :straight ( :host github :files ("mu4e/*") :repo "djcb/mu" :branch "master" :pre-build (("./autogen.sh") ("make")) )
  :commands (mu4e mu4e-update-index)
  :custom
  ( mail-user-agent 'mu4e-user-agent )
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
  ( mu4e-mu-binary (expand-file-name "mu/mu" (straight--repos-dir "mu")) )
  ( mu4e-update-interval nil )
  ( mu4e-use-fancy-chars t )
  ( mu4e-view-prefer-html nil )
  ( mu4e-view-show-addresses t )
  ( smtpmail-smtp-service 587 )
  ( user-full-name "Richard G.Riley" )
  :config


  (use-package mu4e-maildirs-extension
    :custom
    (mu4e-maildirs-extension-hide-empty-maildirs t)
    :config
    (mu4e-maildirs-extension))

  (use-package mu4e-column-faces
    :after mu4e
    :config (mu4e-column-faces-mode))

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "aGmx"
             :enter-func (lambda () (mu4e-message "gmx context")(rgr/mu4e-refresh))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/gmx" (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address . "rileyrg@gmx.de" )
                      ( user-full-name . "Richard G. Riley" )
                      ( smtpmail-smtp-server . "mail.gmx.net")
                      ( mu4e-get-mail-command . "getmails gmx gmx-special-interest")
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
                        (concat
                         "Richard G. Riley\n"
                         "Ein bier, ein Helbing.\n"))))
           ,(make-mu4e-context
             :name "bGmail"
             :enter-func (lambda () (mu4e-message "gmail context") (rgr/mu4e-refresh))
             ;; no leave-func
             ;; we match based on the maildir of the message
             ;; this matches maildir /Arkham and its sub-directories
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
             :vars '( ( user-mail-address . "rileyrg@gmail.com"  )
                      ( user-full-name . "Richie" )
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
                      ( mu4e-compose-signature . "Please change my email to 'rileyrg@gmx.de'.")))))

  (defun mu4e-smarter-compose ()
    "My settings for message composition."
    (set-fill-column 72)
    (flyspell-mode))

  (defun rgr/mu4e-refresh()
    (interactive)
    (when (featurep 'alert)
      (alert "refreshing mu4e indexes"))
    (call-interactively #'(lambda () (interactive)(mu4e-update-mail-and-index t))))

  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions
               '("XWidget View" . mu4e-action-view-with-xwidget) t)
  (add-to-list 'mu4e-view-actions
               '("Markall as read" . mu4e-headers-mark-all-unread-read) t)
  (require 'mu4e-contrib)
  :hook ((mu4e-view-mode . visual-line-mode)
         (mu4e-compose-mode . mu4e-smarter-compose)
         (mu4e-view-mode-hook .
                              (lambda()
                                ;; try to emulate some of the eww key-bindings
                                (local-set-key (kbd "<tab>") 'shr-next-link)
                                (local-set-key (kbd "<backtab>") 'shr-previous-link))))
  :bind	  (("C-c u".  'mu4e)
           (:map mu4e-main-mode-map
                 ("m" . mu4e-compose-new))
           (:map mu4e-main-mode-map
                 ("g" . rgr/mu4e-refresh))
           (:map mu4e-headers-mode-map
                 ("C-c u" . mu4e-headers-mark-all-unread-read))))
;;(
;;:map mu4e-view-mode-map
;;   ("V" . '(lambda()(message "%s" (mu4e-message-at-point))))))) ;; mu4e-action-view-in-browser))))
```


## Screen recording


### Emacs screencasts

Package [keycast](https://github.com/tarsius/keycast) shows the keys pressed

```emacs-lisp
(use-package keycast
  )

```


## Programming Language related

```emacs-lisp
(require 'rgr/programming "rgr-programming" 'NOERROR)
```


### programming library

1.  compilation

    ```emacs-lisp
    (global-set-key (kbd "C-c C-r") 'recompile)
    ```

    1.  rmsbolt

        RMSbolt is a compiler output viewer in Emacs. <https://github.com/emacsmirror/rmsbolt>

        ```emacs-lisp
        (use-package rmsbolt
          :config
          (defun rgr/rmsbolt-toggle()
            (interactive)
            (if rmsbolt-mode
                (progn
                  (when (get-buffer
                         rmsbolt-output-buffer)
                    (with-current-buffer rmsbolt-output-buffer
                      (kill-buffer-and-window)))
                  (rmsbolt-mode -1))
              (progn
                (rmsbolt-mode +1)
                (rmsbolt-compile))))
          :bind
          (:map prog-mode-map
                ("C-c d" . rgr/rmsbolt-toggle)))

        ```

    2.  parrot

        ```emacs-lisp
        (defun my/parrot-animate-when-compile-success (buffer result)
          (if (string-match "^finished" result)
              (parrot-start-animation)))

        (use-package parrot
          :ensure t
          :config
          (parrot-mode)
          (add-to-list 'compilation-finish-functions 'my/parrot-animate-when-compile-success))
        ```

2.  Emacs Lisp, ELisp Utils

    Load this relatively early in order to have utils available if there's a faied load Raw: [rgr/elisp-utils](etc/elisp/rgr-elisp-utils.el)

    ```emacs-lisp
    (require 'rgr/elisp-utils (expand-file-name "rgr-elisp-utils" elisp-dir))
    ```

    1.  scratch,messages

        ```emacs-lisp
        (use-package scratch
          :bind ("<f2>" . (lambda()
                            (interactive)
                            (switch-to-buffer(scratch--create 'emacs-lisp-mode "*scratch*"))))
          ("C-<f2>" . (lambda()
                        (interactive)
                        (switch-to-buffer(messages-buffer)))))
        ```

    2.  rgr/elisp-utils library

        1.  elisp checks

            ```emacs-lisp
            (defun rgr/elisp-edit-mode()
              "return non nil if this buffer edits elisp"
              (member major-mode '(emacs-lisp-mode lisp-interaction-mode)))
            ```

        2.  linting

            [package-lint](https://github.com/purcell/package-lint) provides a linter for the metadata in Emacs Lisp files which are intended to be packages. You can integrate it into your build process.

            ```emacs-lisp
            (use-package package-lint)
            ```

        3.  helpful, enriched elisp help

            ```emacs-lisp
            (use-package helpful
              :config
              ;; Note that the built-in `describe-function' includes both functions
              ;; and macros. `helpful-function' is functions only, so we provide
              ;; `helpful-callable' as a drop-in replacement.
              (global-set-key (kbd "C-h e")
                              (lambda()
                                (interactive)
                                (if(get-buffer "*info*")
                                    (switch-to-buffer "*info*")
                                  (info "elisp"))))
              (global-set-key (kbd "C-h f") #'helpful-callable)

              (global-set-key (kbd "C-h v") #'helpful-variable)
              (global-set-key (kbd "C-h k") #'helpful-key)
              ;;I also recommend the following keybindings to get the most out of helpful:
              ;; Lookup the current symbol at point. C-c C-d is a common keybinding
              ;; for this in lisp modes.
              (global-set-key (kbd "C-h SPC") #'helpful-at-point)
              ;; Look up *F*unctions (excludes macros).
              ;;
              ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
              ;; already links to the manual, if a function is referenced there.
              (global-set-key (kbd "C-h F") #'helpful-function)

              ;; Look up *C*ommands.
              ;;
              ;; By default, C-h C is bound to describe `describe-coding-system'. I
              ;; don't find this very useful, but it's frequently useful to only
              ;; look at interactive functions.
              (global-set-key (kbd "C-h C") #'helpful-command))
            ```

        4.  elisp popup context help

            Display a poup containing docstring at point

            ```emacs-lisp
            (use-package el-docstring-sap-
              :straight (el-docstring-sap :local-repo "~/development/projects/emacs/el-docstring-sap" :type git :host github :repo "rileyrg/el-docstring-sap" )
              :init
              (use-package quick-peek)
              :hook
              (emacs-lisp-mode . el-docstring-sap-mode)
              :bind
              ("M-<f2>" . el-docstring-sap-display)
              ("M-<f1>" . el-docstring-sap-mode))

            ```

        5.  Elisp debugging

            ```emacs-lisp
            (use-package
              edebug-x
              :demand t
              :init
              (global-set-key (kbd "C-S-<f9>") 'toggle-debug-on-error)
              ;;(edebug-trace nil)
              :config
              (require 'edebug)
              (defun instrumentForDebugging()
                "use the universal prefix arg (C-u) to remove instrumentation"
                (interactive)
                (if current-prefix-arg (eval-defun nil) (eval-defun 0)))
              )
            ```

        6.  Formatting

            ```emacs-lisp
            (use-package
              elisp-format
              :bind
              (:map emacs-lisp-mode-map
                    ("C-c f" . elisp-format-region)))
            ```

        7.  popup query symbol

            ```emacs-lisp
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
            ```

        8.  provide

            ```emacs-lisp
            (provide 'rgr/elisp-utils)
            ```

3.  prog-mode hack

    ```emacs-lisp
    (unless (fboundp 'prog-mode)
      (defalias 'prog-mode 'fundamental-mode))
    ```

4.  Show Line numbers

    ```emacs-lisp
    (global-set-key (kbd "S-<f2>") 'display-line-numbers-mode)
    (add-hook 'prog-mode-hook (lambda() (display-line-numbers-mode t)))
    ```

5.  tree-sitter

    <https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/>

    ```emacs-lisp
    (use-package tree-sitter
      :ensure t
      :config
      ;; activate tree-sitter on any buffer containing code for which it has a parser available
      (global-tree-sitter-mode)
      ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
      ;; by switching on and off
      (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

    (use-package tree-sitter-langs
      :ensure t
      :after tree-sitter)
    ```

6.  code format

    ```emacs-lisp
      ;; auto-format different source code files extremely intelligently
    ;; https://github.com/radian-software/apheleia
    (use-package apheleia
      :ensure t
      :config
      (apheleia-global-mode +1))
    ```

7.  rainbow delimiters

    ```emacs-lisp
    (use-package rainbow-delimiters
          :config
          (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

        (use-package rainbow-identifiers
          :config
          (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

    ```

8.  Project Management

    1.  project

        ```emacs-lisp
        (require 'project)
        ```

    2.  projectile

        ```emacs-lisp
        (use-package projectile
          :demand
          :init
          (projectile-mode +1)
          (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))
        ```

9.  BASH

    1.  Navigating Bash set -x output

        ```emacs-lisp
        ;; try to work with next-error for bash's "set -x" output
        (use-package compile
          :config
          (add-to-list 'compilation-error-regexp-alist
                       'bash-set-x)
          (add-to-list 'compilation-error-regexp-alist-alist
                       '(pascal
                         "\\(.+?\\)\\(\\([0-9]+\\),\\([0-9]+\\)\\).*" 1 2 3)))
        ```

10. JSON, YAML Configuration files

    1.  YAML

        ```emacs-lisp
        (use-package
          yaml-mode
          :config
          (add-to-list 'auto-mode-alist '("\\.yml\\.yaml\\'" . yaml-mode))
          )

        ```

11. Flycheck

    On the fly [syntax checking](https://github.com/flycheck/flycheck) for GNU Emacs

    ```emacs-lisp
    (use-package
      flycheck
      :demand
      :custom
      (flycheck-global-modes '(not org-mode org-src-mode))
      (flycheck-emacs-lisp-load-path 'inherit)
      ;;(flycheck-check-syntax-automatically '(save))
      :config (use-package
                flycheck-pos-tip
                :config
                (flycheck-pos-tip-mode))
      (global-flycheck-mode +1)
      :bind ("<f8>" . (lambda()
                        (interactive)
                        (flycheck-mode 'toggle)
                        (let((s (if flycheck-mode "on" "off")))
                          (message "flycheck %s" s)))))
    ```

12. Flymake

    1.  diagnostic-at-point

        ```emacs-lisp
        (use-package flymake-diagnostic-at-point
          :after flymake
          :config
          (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
        ```

    2.  shellcheck

        ```emacs-lisp
        (use-package flymake-shellcheck
          :commands flymake-shellcheck-load
          :init
          (defun rgr/sh-mode-hook()
              (flymake-shellcheck-load)
              (flymake-mode +1))
          :hook (sh-mode . rgr/sh-mode-hook))
        ```

13. Version Control

    1.  It's [Magit](//github.com/magit/magit)! A Git porcelain inside Emacs

        magit

        ```emacs-lisp
        (use-package
          magit
          :custom
          (vc-handled-backends '(git))
          :config
          (add-hook 'magit-post-commit-hook 'magit-mode-bury-buffer)
          (require 'magit-extras)
          :bind
          ("C-x g" . magit-status))
        ```

        1.  [Orgit](https://github.com/magit/orgit) allows us to link to Magit buffers from Org documents

            ```emacs-lisp
            (use-package orgit
              :after magit)
            ```

    2.  EDiff - comparing files in Emacs

        ```emacs-lisp
        (use-package ediff+
          :custom
          (ediff-window-setup-function 'ediff-setup-windows-plain)
          (ediff-split-window-function 'split-window-horizontally)
          :config
          (when (fboundp 'winnder-undo)
            (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
          :bind (:map prog-mode-map ("C-c C-d" . 'ediff-files)))
        ```

    3.  [Forge](https://github.com/magit/forge) ahead with Pull Requests

        ```emacs-lisp
        (use-package forge
          :after magit
          :config
          (use-package orgit-forge))
        ```

        1.  token

            &#x2013;&#x2014;BEGIN PGP MESSAGE&#x2013;&#x2014;

            hQEMA7IjL5SkHG4iAQgAnyC1NPS3sWV9r7kriSvL4IF53g076qwmEHvmPjBaUp9R QeQyyp8ek1MOSQP4zPbn0s5gAALMdP/UbEjocudP0e3bY2BYLJstTC7av1LS5Vzq NpoecGRrlUCISRAMCRx/2MpE6o2E3RdAd0P2RQ4vGmaIEJ0vHkfP8RnYd8M+wacy y58rcuxmGBaLNGEOOywb2icYVrjKXxSdRDXL6/LTjkQHXuPjpD27WIA2ASExh64t 1XIA6Gs0vXc8CF+ppt9tb48TwCyONhH9PtE1CURhH6FRPSKkxXD/eq/BhoVzjT// 9lu269+Q2H6QHQAh0CeT9wuqpSHxXu/fKDNrG/DGmNLpAfSrO4bepZGOAWKnDQFP Dkr1FEpb43SRZgyP3KDEad5F7uZzYDf7FVLxfNlWzhuFErzLVTHlUxWBAcY22a8R 2AuS6D+0vpJUSXqXEYQJ+R/GqHe1h+6mBnAyloz9eSU7X9kYPUv3cEAkWYrkGLt0 Or7cfbtFL+GUQpVVNELOZftK1h4S3StfJJerc5YluQBqHUQkCIZMa6AS48uV958b JC9WHZIgGzkb/3GLV/rAEwgOhlmfWavmP/MXIEl5YBOpyOSkpVm4CKqNOx7+Sby+ 2Gh45i5qQhRfBW6880zrgnRSa6rlXHrzd4gL32sSKGON6YEhngJRzZzZhf+0IgpR QXRk7H0novz5DBUHAcecOGqNikTVvwBXI12sFjh6YMbVD0FhkokSjHqipJqXCwDc In6uBDyBjOjcJa0M7KzEN5MsN9RJK00vrBao+b8mpROUCpVAF/ZL5ofMTu72qJ2P Plpk2ab+ZpAxm+B7am9r2CojjDDz/D8aFFR+bLx/0c1AnCUDnvqjBmNKxCHD2jIR B4ghhzUfYFDgnm7u2vg3ycTxuP1ys74Z82Ufw3YZeiroG+uM/h90eyXJsEHv6pmj mXO4USgtApYLqNUfSptcjw1nDnUnSus2/DjIZZTg0GNMQi013kHkrodKmAs2V3/o NmmXwjbGwdqpZzbwiG2yrw5BwkdKPQQ4PRsyUVuyWfrYAFLLtfXuFGIIfoQ2DiSl EplOgD7H7V1KIc888MR51uk6/tPDhpROmupKMr8+Hh/WooY= =FmGT &#x2013;&#x2014;END PGP MESSAGE&#x2013;&#x2014;

    4.  Git Gutter Mode

        [git-gutter.el](https://github.com/emacsorphanage/git-gutter) is an Emacs port of the Sublime Text plugin GitGutter.

        ```emacs-lisp
        (use-package git-gutter
          :config
          (global-git-gutter-mode +1)
          :bind
          ("C-x v ="  . git-gutter:popup-hunk))
        ```

14. Javascript

    ```emacs-lisp

    (use-package rjsx-mode
      :disabled t
      :config
      (use-package npm-mode)
      (defun rgr/js2-mode-hook ()
        ;;         (setq-local zeal-at-point-docset '("JavaScript" "jQuery"))
        (npm-mode t)
        (setq-local dash-docs-docsets '("React" "JavaScript" "jQuery")))
      (add-hook 'js2-mode-hook 'rgr/js2-mode-hook)
      :init
      ;; (add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))
      ;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
      (add-to-list 'auto-mode-alist '("\\.js?\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
      )

    (defun rgr/js-mode-hook ()
      (when (featurep 'lsp-mode)
        (lsp))
      (setq-local dash-docs-docsets '("React" "JavaScript" "jQuery")))

    (add-hook 'js-mode-hook 'rgr/js-mode-hook)



    ;;(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))
    ```

15. RJSX

    [rjsx-mode](https://github.com/felipeochoa/rjsx-mode) extends js2-mode to include jsx parsing.

    ```emacs-lisp
    (use-package rjsx-mode
      :disabled t
      :init
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
      )
    ```

16. Typescript

    ```emacs-lisp
    ;; sudo npm i -g typescript-language-server
    (use-package typescript-mode
      :after tree-sitter
      :config
      ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
      ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
      (define-derived-mode typescriptreact-mode typescript-mode
        "TypeScript TSX")

      ;; use our derived mode for tsx files
      (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
      ;; by default, typescript-mode is mapped to the treesitter typescript parser
      ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
      (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))
      (defun rgr/ts-mode-hook ()
        (when (featurep 'lsp-mode)
          (lsp))
        (setq-local dash-docs-docsets '("React" "JavaScript")))
      (add-hook 'typescript-mode-hook 'rgr/ts-mode-hook))

    ```

17. Language Server Protocol (LSP), lsp-mode

    [Emacs-lsp](https://github.com/emacs-lsp) : Language Server Protocol client for Emacs

    Raw: [rgr/lsp](etc/elisp/rgr-lsp.el)

    ```emacs-lisp
    (require 'rgr/lsp "rgr-lsp" 'NOERROR)
    ```

    1.  library

        1.  eglot

            Emacs lsp client <https://github.com/joaotavora/eglot>

            ```emacs-lisp
            (use-package eglot
              :disabled t
              :demand
              :bind
              (:map flymake-mode-map
                    ([remap next-error] . flymake-goto-next-error)
                    ([remap previous-error] . flymake-goto-prev-error)))
            ```

        2.  lsp

            ```emacs-lisp
            ;; if you want to change prefix for lsp-mode keybindings.
            (use-package lsp-mode
              :custom
              (lsp-auto-guess-root nil)
              (lsp-clients-clangd-args '("--header-insertion-decorators=0" "--fallback-style=Google"))
              (lsp-completion-enable  t)
              (lsp-completion-provider :none)
              (lsp-completion-show-kind t)
              (lsp-diagnostics-provider :none)
              (lsp-eldoc-enable-hover nil)
              (lsp-enable-on-type-formatting t)
              (lsp-enable-snippet nil)
              (lsp-enable-symbol-highlighting t)
              (lsp-headerline-breadcrumb-enable nil)
              (lsp-lens-enable nil)
              (lsp-modeline-code-actions-enable t)
              (lsp-modeline-diagnostics-enable nil)
              (lsp-signature-auto-activate t)
              :config
              (with-eval-after-load 'lsp-mode
                (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

            (use-package lsp-treemacs
              :config
              (lsp-treemacs-sync-mode 1))

            (use-package lsp-ui
              :custom
              (lsp-ui-doc-delay 2.5)
              (lsp-ui-doc-enable t)
              (lsp-ui-doc-position 'at-point)
              (lsp-ui-doc-show-with-mouse t)
              (lsp-ui-doc-show-with-cursor t)
              (lsp-ui-peek-enable t)
              (lsp-ui-peek-show-directory t)
              (lsp-ui-sideline-enable t)
              (lsp-ui-sideline-show-code-actions t)
              (lsp-ui-sideline-show-diagnostics t)
              :bind
              (:map lsp-ui-mode-map
                    ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
                    ([remap xref-find-references] . #'lsp-ui-peek-find-references)))

            (use-package dap-mode
              :disabled t
              :commands rgr/dap-debug
              :custom
              (dap-auto-configure-features '(locals  tooltip))
              :config
              (require 'dap-chrome)
              (setq dap-ui-buffer-configurations
                    `((,"*Dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.50))) ;; changed this to 0.50
                      (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.50)))
                      (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.50)))
                      (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
                      (,"*debug-window*" . ((side . bottom) (slot . 3) (window-width . 0.20)))))
              (defun rgr/dap-debug()
                (interactive)
                (if current-prefix-arg
                    (call-interactively 'dap-debug)
                  (dap-debug-last)))
              ;;(require 'dap-gdb-lldb)
              ;;(dap-gdb-lldb-setup)
              ;;(require 'dap-codelldb)
              ;;(dap-codelldb-setup)
              (require 'dap-cpptools)
              ;;(dap-cpptools-setup)
              ;; (require 'dap-lldb)
              (add-hook 'dap-stopped-hook
                        (lambda (arg)
                          (call-interactively #'dap-hydra)))
              :bind
              (:map lsp-mode-map
                    ("C-<f9>" . #'rgr/dap-debug))
              (:map dap-mode-map
                    ("<f8>" . dap-continue)
                    ("C-S-<f8>" . dap-delete-session)
                    ("<f9>" . dap-hydra)
                    ("<f10>" . dap-next)
                    ("<f11>" . dap-step-in)
                    ("S-<f11>" . dap-step-out)
                    ))
            ```

        3.  [.dir-local.el](file:///home/rgr/development/thirdparty/godot/bin) config for a debug template

            ```emacs-lisp
            ((c++-mode . ((dap-debug-template-configurations . (("Godot LLDB"
                                                                 :type "lldb"
                                                                 :request "launch"
                                                                 :target "/home/rgr/bin/godot")
                                                                ("Godot GDB"
                                                                 :type "gdb"
                                                                 :request "launch"
                                                                 :target "/home/rgr/bin/godot"))))))
            ```

        4.  provide

            ```emacs-lisp
            (provide 'rgr/lsp)
            ```

18. Serial Port

    ```emacs-lisp
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
    ```

19. PlatformIO

    [platformio-mode](https://github.com/emacsmirror/platformio-mode) is an Emacs minor mode which allows quick building and uploading of PlatformIO projects with a few short key sequences. The build and install process id documented [here](https://docs.platformio.org/en/latest/ide/emacs.html).

    ```emacs-lisp
    (use-package platformio-mode
      :demand t
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
    ```

    1.  get compilation errors to work and submit ansi color fix?

20. Python

    1.  python-mode

        ```emacs-lisp
        (use-package lsp-pyright
        :ensure t
        :hook (python-mode . (lambda ()
                                (require 'lsp-pyright)
                                (lsp-deferred))))  ; or lsp
        (use-package  python
          :disabled t
          :config
          (defun rgr/python-shell-send-buffer(orig-func &rest args)
            "create a python shell if there isnt one"
            (interactive)
            (save-selected-window
              (save-excursion(run-python))
              (apply orig-func current-prefix-arg)
              (unless (get-buffer-window (python-shell-get-buffer))
                (switch-to-buffer-other-window (python-shell-get-buffer)))))
          (advice-add 'python-shell-send-buffer :around #'rgr/python-shell-send-buffer))
        ```

    2.  ipython

        ```emacs-lisp
        (setq python-shell-interpreter "ipython")
        (setq python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
        ```

    3.  virtualenv

        ```emacs-lisp
        (use-package auto-virtualenv
          :config
          (add-hook 'python-mode-hook  #'auto-virtualenv-set-virtualenv))
        ```

    4.  blacken reformatting

        ```emacs-lisp
        (use-package blacken
          :disabled t
          :demand t
          :config
          (add-hook 'python-mode-hook  #'blacken-mode))
        ```

21. Haskell

    1.  haskell-mode

        ```emacs-lisp
        ;; I'm typically confused when it comes to haskell. Note that the interactive stuff I cribbed doesnt work.
        (use-package haskell-mode
          :config
          (use-package  lsp-haskell)
          (add-hook 'haskell-mode-hook #'lsp-deferred)
          (add-hook 'haskell-literate-mode-hook #'lsp-deferred)
          (eval-after-load "haskell-mode"
            '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
          (eval-after-load "haskell-cabal"
                      '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
          (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
        ```

22. lldb debugging in emacs

    1.  voltron

        ```emacs-lisp
        (use-package lldb-voltron
          :straight (lldb-voltron :local-repo "~/development/projects/emacs/emacs-lldb-voltron" :type git :host github :repo "rileyrg/emacs-lldb-voltron" ))
        ```

23. c-mode-common-hook

    ```emacs-lisp
    (use-package emacs
      :config
      (defun rgr/c-mode-common-save-hook()
        ;;    (lsp-format-buffer)
        ;;(eglot-format-buffer)
        )
      (defun rgr/c-mode-common-hook ()
        (add-hook 'before-save-hook #'rgr/c-mode-common-save-hook nil t)
        (if(featurep 'corfu)
            (setq completion-category-defaults nil))
        ;; (if(featurep 'eglot)
        ;;     (eglot-ensure))
        (if(featurep 'lsp-mode)
            (lsp-deferred))
        (if(featurep 'platformio-mode)
            (platformio-conditionally-enable))
        (if (featurep 'yasnippet)
            (yas-minor-mode)))
      :hook
      (c-mode-common . rgr/c-mode-common-hook)
      :bind  ( :map c-mode-base-map
               (("M-<return>" . rgr/c-complete-line)
                ("TAB" . rgr/c-indent-complete))))
    ```

24. C, c-mode

    ```emacs-lisp
    (defun rgr/c-mode-hook ()
      (setq-local dash-docs-docsets '("C")))
    (add-hook 'c-mode-hook 'rgr/c-mode-hook)
    ```

    1.  line utilities

        ```emacs-lisp
        (defun rgr/c-complete-line()
          (interactive)
          (end-of-line)
          (delete-trailing-whitespace)
          (unless (eql ?\; (char-before (point-at-eol)))
            (progn (insert ";")))
          (newline-and-indent))
        ;;(define-key c-mode-map (kbd "M-<return>") 'rgr/c-complete-line)
        (defun rgr/c-insert-previous-line()
          (interactive)
          (previous-line)
          (end-of-line)
          (newline-and-indent)
          (insert (string-trim (current-kill 0))))
        (defun rgr/c-newline-below()
          (interactive)
          (end-of-line)
          (newline-and-indent))
        ```

    2.  formatting

        ```emacs-lisp

        (defun rgr/c-indent-complete()
          (interactive)
          (let (( p (point)))
            (c-indent-line-or-region)
            (when (= p (point))
              (call-interactively 'complete-symbol))))

        ```

25. cc,cpp, C++, cc-mode

    ```emacs-lisp
    (defun rgr/c++-mode-hook ()
      (setq-local dash-docs-docsets '("C++")))
    (add-hook 'c++-mode-hook 'rgr/c++-mode-hook)
    ```

26. Linux tools

    1.  [logview](https://github.com/doublep/logview) - view system logfiles

        ```emacs-lisp
        (use-package logview
          :demand t
          :init
          (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
          (add-to-list 'auto-mode-alist '("log\\'" . logview-mode)))
        ```

27. Assembler

    1.  [x86Lookup](https://nullprogram.com/blog/2015/11/21/)

        ```emacs-lisp
        (use-package strace-mode)
        ```

28. Godot GDScript

    This [package](https://github.com/GDQuest/emacs-gdscript-mode) adds support for the GDScript programming language from the Godot game engine in Emacs. It gives syntax highlighting and indentations

    ```emacs-lisp
    (use-package gdscript-mode
      ;;       :disabled t
      :straight (gdscript-mode
                 :type git
                 :host github
                 :repo "rileyrg/emacs-gdscript-mode")
      :init
      (defun franco/godot-gdscript-lsp-ignore-error (original-function &rest args)
        "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
        (if (string-equal major-mode "gdscript-mode")
            (let ((json-data (nth 0 args)))
              (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                       (not (gethash "id" json-data nil))
                       (not (gethash "method" json-data nil)))
                  nil ; (message "Method not found")
                (apply original-function args)))
          (apply original-function args)))
      (advice-add #'lsp--get-message-type :around #'franco/godot-gdscript-lsp-ignore-error)
      )
    ```

29. Web,Symfony and Twig

    1.  Symfony

        1.  custom

            ```emacs-lisp
            (defgroup rgr/symfony nil
              "Symfony Development"
              :group 'rgr)

            (defcustom symfony-server-command "~/.symfony/bin/symfony server:start"
              "Start the symfony web server"
              :type 'string
              :group 'rgr/symfony)
            ```

        2.  Start a symfony web server when applicable

            ```emacs-lisp
            (use-package php-mode
              :custom
              (lsp-intelephense-licence-key (get-auth-info "licenses" "intelephense"))
              :config
              (add-to-list 'display-buffer-alist
                           (cons "\\*Symfony Web Server\\*.*" (cons #'display-buffer-no-window nil)))
              (defun start-symfony-web-server()
                (interactive)
                (let ((default-directory (project-root (project-current t))))
                  (if (and default-directory (file-exists-p "bin/console") (eq (length (shell-command-to-string "pgrep symfony")) 0) (yes-or-no-p "Start web server?"))
                      (async-shell-command symfony-server-command "*Symfony Web Server*"))))
              (defun php-mode-webserver-hook ()
                (interactive)
                (start-symfony-web-server)
                ))
            ```

            We can trigger it using a .dir-locals.el

            ```emacs-lisp
            ((php-mode
              (eval php-mode-webserver-hook)))
            ```

        3.  webmode

            ```emacs-lisp
            (use-package
              web-mode
              ;;:disabled t
              :demand t
              :config
              (defun rgr/web-mode-hook()
                (setq-local dash-docs-docsets '("Twig" "CSS" "HTML"))
                )
              (add-hook 'web-mode-hook 'rgr/web-mode-hook)
              ;; (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
              ;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))
            ```

30. elf-mode - view the symbol list in a binary

    [https://oremacs.com/2016/08/28/elf-mode/](https://oremacs.com/2016/08/28/elf-mode/)

    ```emacs-lisp
    (use-package elf-mode
      :demand t
      :config
      (add-to-list 'magic-mode-alist '("\dELF" . elf-mode))
      (add-to-list 'auto-mode-alist '("\\.\\(?:a\\|so\\)\\'" . elf-mode)))
    ```

31. provide

    ```emacs-lisp
    (provide 'rgr/programming)
    ```


## Themes


### modus themes

<https://github.com/protesilaos/modus-themes>

```emacs-lisp
(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi))
;; (modus-themes-load-vivendi))
```


### humanoid themes

<https://github.com/humanoid-colors/emacs-humanoid-themes>

```emacs-lisp
(use-package humanoid-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (load-theme 'humanoid-dark t))
```


## Late load

```emacs-lisp
(load-el-gpg (no-littering-expand-etc-file-name "late-load"))
```


# Associated emacs things


## Project version control with  git


### .gitignore

An exclusionary .gitignore. You need to specfically add in things you wish to add to version control.

```config
*
*.*

!.gitignore
!.ignore

!emacs-config.org
!early-init.el
!init.el
!README.md
!custom.el

!info
!info/*

!etc
!var

!var/bmkp
!var/bmkp/current-bookmark.el
!var/bmkp/current-bookmark.el.gpg

!etc/abbrev.el

!etc/early-load
!etc/early-load/*

!etc/elfeed
!etc/elfeed/elfeed.org


!etc/org
!etc/org/agenda-files.txt

!etc/eshell
!etc/eshell/alias

!etc/elisp
!etc/elisp/*.el

!etc/hosts
!etc/hosts/thinkpadx270
!etc/hosts/thinkpadx270/custom.el
!etc/hosts/thinkpadx60
!etc/hosts/thinkpadx60/custom.el
!etc/hosts/intelnuc
!etc/hosts/intelnuc/custom.el
!etc/hosts/xmgneo
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
```

1.  how to export tangle destination?


## Setting up emacs as a default editor using a dot desktop file and associated protocol handler


### [php.ini](editor-config/php.ini) changes e.g /etc/php/7.3/php.ini

`xdebug.file_link_format` is used by compliant apps to format a protocol uri. This is handled on my Linux system as a result of [emacsclient.desktop](#org8331a68) documented below.

```conf
xdebug.file_link_format = "emacsclient://%f@%l"

xdebug.remote_enable = 1
xdebug.remote_host = localhost
xdebug.remote_port = 9000
```

```eshell
xdebug.file_link_format = "emacsclient://%f@%l"

xdebug.remote_enable = 1
xdebug.remote_host = localhost
xdebug.remote_port = 9000
```


### [emacsclient-linenumber](bin/emacsclient-linenumber) script to correctly parse the protocol in order to start emacs frame at correct line

```bash
#!/usr/bin/bash
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
```


<a id="org8331a68"></a>

### Gnome protocol handler desktop file

Copy [emacsclient.desktop](editor-config/emacsclient.desktop) to ~/.local/share/applications (Debian & Gnome - your mileage may vary&#x2026;)

```conf
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
```

Update the desktop database:

```bash
update-desktop-database ~/.local/share/applications
```

now allows emacsclient to be called.

note: symlink the deprecated [~/.local/share/applications/mimeapps.lst](https://wiki.archlinux.org/index.php/XDG_MIME_Applications) to the one in ~/.config

```bash
ln -sf  ~/config/mimeapps.lst ~/.local/share/applications/
```


### Always using emacsclient

Set up a zshrc alias so that "emacs" actually invokes emacs client. In my .zshrc:

```shell
alias emacs='emacsclient --create-frame --alternate-editor=""'
```


## [bin/emacs-same-frame](bin/emacs-same-frame) open emacs in the same frame

```bash
#!/bin/bash
emacsclient -e "(if (> (length (frame-list)) 1) 't)" | grep -q t
if [ "$?" = "1" ]; then
    emacsclient -n -c -a "" "$@" #none found so create frame
else
    emacsclient -n -a "" "$@"
    pop-window "Emacs"
fi
```


## compiling emacs


### ~/bin/emacs-compile-opt

```bash
#!/usr/bin/bash
#Maintained in emacs-config.org
cd ~/development/projects/C/emacs/
mkdir -p emacs-build
./configure --prefix=`pwd`/emacs-build --with-x-toolkit=lucid
make && make install
```


### ~/bin/emacs-compile-dbg

```bash
#!/usr/bin/bash
#Maintained in emacs-config.org
cd ~/development/projects/C/emacs/
mkdir -p emacs-build-dbg
./configure --prefix=`pwd`/emacs-build-dbg --with-x-toolkit=lucid --enable-checking='yes,glyphs' --enable-check-lisp-object-type CFLAGS='-O0 -g3'
make && make install
```