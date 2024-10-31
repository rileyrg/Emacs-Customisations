

# Introduction

Emacs customisation generates [init.el](init.el) and other [emacs elisp utility files](etc/elisp/)  using [org-babel-tangle](https://orgmode.org/manual/Extracting-Source-Code.html).


## scratch

    (let ((foo "foo")
          (bar "bar"))
      (concat bar foo))


## Own libraries

These libraries are seperate stand alone github libraries.


### el-docstring-sap

Provides docstring help for symbol at point.

[https://github.com/rileyrg/el-docstring-sap](https://github.com/rileyrg/el-docstring-sap)


### lazy-lang-learn

A small "game" like utility that displays snippets to glance at. You can then
invoke google translate on them. Stores history.

<https://github.com/rileyrg/lazy-lang-learn>


# early stuff


## early-init.el

<https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html>

    ;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
    ;; Maintained in emacs-config.org
    (setq max-specpdl-size 13000)


### straight.el package management

[straight.el](https://github.com/raxod502/straight.el#features): next-generation, purely functional package manager for the Emacs hacker.

    
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


## custom.el

    (setq custom-file  (expand-file-name  "custom.el" user-emacs-directory)) ;;
    (load custom-file 'noerror)


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


# config


## post straight debug init

Here can load a "bare bones" init. When hit debug can "c" to continue or "q" to abort.

    ;; look for a debug init file and load, trigger the debugger
    (debug-init "debug-init-straight.el")


## Paths


### Path to our own elisp

    (defvar elisp-dir (expand-file-name "elisp" no-littering-etc-directory) "my elisp directory. directories are recursively added to path.")
    (add-to-list 'load-path elisp-dir)
    (let ((default-directory elisp-dir))
      (normal-top-level-add-subdirs-to-load-path))


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
    (load-el-gpg (no-littering-expand-etc-file-name "early-load"))


## host specific

:ID:       efe9afb5-3779-407d-a7a9-fd8968ea0f69

Stick a custom in here. eg my thinkpad [custom file](./etc/hosts/thinkpadx270/custom.el).

    (load-el-gpg (expand-file-name (system-name)  (no-littering-expand-etc-file-name "hosts")))


## Security

Raw: [rgr/security](etc/elisp/rgr-security.el)

    (require 'rgr/security "rgr-security" 'NOERROR)


### library

1.  Auth-Sources, get-auth-info

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

2.  Pass

    Uses the unix command line `pass` utility. Can be used via `process-lines`  e.g
    
        (car (process-lines "pass" "Chat/slack-api-token"))
    
        (use-package pass)

3.  provide

        (provide 'rgr/security)


## Utilities

Raw: [rgr-utils](etc/elisp/rgr-utils.el).

    (require 'rgr/utils "rgr-utils" 'NOERROR)


### library

1.  screencast gifs

    Not working with wayland
    
        (use-package gif-screencast
          :custom
          (gif-screencast-program "grim")
          (gif-screencast-screenshot-directory "~/tmp")
          (gif-screencast-output-directory "~/tmp"))

2.  toggle buffer

        (defun rgr/toggle-buffer(n)
          "jump to or from buffer named n else default to *Messages*"
          (interactive "bbuffer:")
          (let ((n (or n
                       "*Messages*")))
            (switch-to-buffer (if (string= (buffer-name) n)
                                  (other-buffer) n))))

3.  read and write elisp vars to file

        
        (defun rgr/elisp-write-var (f v)
          (with-temp-file f
            (prin1 v (current-buffer))))
        
        (defun rgr/elisp-read-var (f)
          (with-temp-buffer
            (insert-file-contents f)
            (cl-assert (eq (point) (point-min)))
            (read (current-buffer))))

4.  completing lines

        (use-package emacs
          :init
          (defvar rgr/complete-line-f 'rgr/newline-below "The fname called by `rgr/complete-line'")
          :config
        
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
        
          :bind
          ("<C-S-return>" . rgr/complete-line))

5.  Lazy Language Learning, lazy-lang-learn

    My own hack for popping up text to learn
    
        (use-package lazy-lang-learn
          :straight (lazy-lang-learn :local-repo "~/development/projects/emacs/lazy-lang-learn" :type git :host github :repo "rileyrg/lazy-lang-learn" )
          :bind
          ("C-c L" . lazy-lang-learn-mode)
          ("<f12>" . lazy-lang-learn-translate)
          ("S-<f12>" . lazy-lang-learn-translate-from-history))

6.  provide

        (provide 'rgr/utils)


## Emacs startup

Raw: [rgr/startup](etc/elisp/rgr-startup.el)

    (require 'rgr/startup "rgr-startup" 'NOERROR)


### library

1.  persistence  and history

        
        (recentf-mode)
        (savehist-mode)
        (save-place-mode)
        ;;(desktop-save-mode t)
        ;;(midnight-mode t)
        
        (defun rgr/save-current-file-to-register ()
          "Save current file to register."
          ;; https://www.reddit.com/r/emacs/comments/oui4c6/using_register_to_save_current_file
          (interactive)
          (let ((reg (register-read-with-preview "File name to register: ")))
            (set-register reg `(file . ,(buffer-file-name)))))
        
        (defun rgr/startup-hook ()
          (bookmark-maybe-load-default-file)
          (bookmark-jump "current"))
        
        (defun rgr/remember-last-buffer ()
          (when buffer-file-name
            (bookmark-set "current")))
        
        ;; (add-hook 'window-buffer-change-functions #'rgr/remember-last-buffer)
        (add-hook 'kill-emacs-hook  #'rgr/remember-last-buffer)
        
        (add-hook 'server-after-make-frame-hook #'rgr/startup-hook)

2.  quitting emacs

        (defun rgr/quit-or-close-emacs(&optional kill)
          (interactive)
          (if (or current-prefix-arg kill)
              (rgr/server-shutdown)
            (delete-frame)))
        
        (defun rgr/server-shutdown ()
          "Save buffers, Quit, and Shutdown (kill) server"
          (interactive)
          (save-buffers-kill-emacs))
        
        (global-set-key (kbd "C-c x") 'rgr/quit-or-close-emacs)
        
        (provide 'rgr/startup)


## General configuration

Raw: [rgr/general-config](etc/elisp/rgr-general-config.el).

    (require  'rgr/general-config "rgr-general-config" 'NOERROR)


### library

1.  general ui

        
        (use-package emacs
          :init
          (require 'iso-transl) ;; supposed to cure deadkeys when my external kbd is plugged into my thinkpad T44460.  It doesnt.
                                                ; t60
          (scroll-bar-mode -1)
          (tool-bar-mode -1)
          (menu-bar-mode -1)
          (show-paren-mode 1)
          (winner-mode 1)
        
          (global-auto-revert-mode 1)
          ;; Also auto refresh dired, but be quiet about it
          (setq global-auto-revert-non-file-buffers t)
          (setq auto-revert-verbose nil)
        
          (global-visual-line-mode 1)
        
          (setq column-number-mode t)
        
          (delete-selection-mode 1)
        
          (setq frame-title-format (if (member "-chat" command-line-args)  "Chat: %b" '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " — Emacs")))
        
          (defalias 'yes-or-no-p 'y-or-n-p)
        
          (setq disabled-command-function nil)
        
          (global-hl-line-mode t)
        
          ;; https://github.com/rolandwalker/browse-url-dwim
          ;; Context-sensitive external browse URL or Internet search from Emacs.
          (use-package
            browse-url-dwim
            :config
            (browse-url-dwim-mode))
        
          (use-package alert)
        
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
        
          (add-hook 'before-save-hook 'delete-trailing-whitespace)
          :bind
          ("C-x C-q" . view-mode)
          ("C-c e" . rgr/erc-start)
          ("C-x C-b" . ibuffer)
          ("C-x C-i" . imenu)
          ("C-x k" . rgr/kill-current-buffer)
          ("M-0" . delete-window)
          ("M-1" . delete-other-windows)
          ("S-<f1>" . describe-face)
          ( "M-m"  . manual-entry)
          ("S-<f10>" . menu-bar-open))

2.  posframe

    [Posframe](https://github.com/tumashu/posframe)
    can pop up a frame at point, this posframe is a child-frame connected to its root window's buffer.
    
        (use-package posframe)

3.  ACE utilities

    1.  [Ace-Window](https://github.com/abo-abo/ace-window) provides better window switching.
    
            (use-package ace-window
              ;; :init
              ;; (defalias 'other-window 'ace-window)
              :bind
              ("M-o" . other-window)
              ("C-x o" . ace-window)
              ("M-S o" . ace-delete-window))
    
    2.  hopping around links
    
        Quickly follow [links](https://github.com/abo-abo/ace-link) in Emacs.
        
            (use-package ace-link
              :demand
              :config
              (ace-link-setup-default)
              :bind*
              (:map emacs-lisp-mode-map
                    ("C-c o" . ace-link-addr))
              ("C-c o" . ace-link)
              )
    
    3.  hopping around in the buffer
    
        Allows word, char and line hopping. The [wiki](https://github.com/winterTTr/ace-jump-mode/wiki) is a food source of info.
        
            (use-package ace-jump-mode
              :bind
              ("M-s c" . ace-jump-mode)
              )

4.  Golden Ratio

    Zoom into current buffer
    
        (use-package
          golden-ratio
          :init
          (golden-ratio-mode 1))

5.  beacon

    visual feedback as to cursor position
    
        (use-package beacon
          :disabled t
          :custom
          (beacon-blink-delay 1)
          (beacon-size 10)
          (beacon-color "orange" nil nil "Customized with use-package beacon")
          (beacon-blink-when-point-moves-horizontally 32)
          (beacon-blink-when-point-moves-vertically 8)
          :config
          (beacon-mode 1))

6.  pulsar

    visual feedback as to cursor position
    <https://protesilaos.com/emacs/pulsar>
    
        (use-package pulsar
          :custom
          (pulsar-pulse t)
          (pulsar-delay 0.7)
          (pulsar-iterations 30)
          (pulsar-face 'pulsar-yellow)
          (pulsar-highlight-face 'pulsar-green)
          :init
          (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)
          (add-hook 'consult-after-jump-hook #'pulsar-recenter-middle)
          (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
          :config
          (
           pulsar-global-mode 1))

7.  blackout modeline

    Blackout is a package which allows you to hide or customize the display of major and minor modes in the mode line.
    
        (straight-use-package
         '(blackout :host github :repo "raxod502/blackout"))

8.  boxquote

        (use-package boxquote
          :straight (:branch "main")
          :bind
          ("C-S-r" . boxquote-region))

9.  volatile-highlights

    brings visual feedback to some operations by highlighting portions relating to the operations.
    
        (use-package
          volatile-highlights
          :disabled
          :init (volatile-highlights-mode 1))

10. web pasting

        (use-package
          dpaste
          :init
          :bind ("C-c y" . dpaste-region-or-buffer))

11. Accessibility

    1.  fonts
    
        JetBrains fonts are nice. See [nerd-fonts](https://github.com/ryanoasis/nerd-fonts)
        
            ;;(set-frame-font "-JB-JetBrainsMono Nerd Font-regular-normal-normal-*-14-*-*-*-*-0-fontset-auto1" nil t)
    
    2.  Darkroom
    
        Zoom in and center using [darkroom](https://github.com/joaotavora/darkroom).
        
            (use-package
              darkroom
              :bind
              ( "<C-f7>" . 'darkroom-mode))

12. Ansi colour

    [Ansi colour hooks](https://www.emacswiki.org/emacs/AnsiColor) to enable emacs buffers to handle ansi.
    
        (require 'ansi-color)
        (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
        (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

13. Tabs

        
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

14. bookmarks

    1.  bookmark+
    
            (use-package bookmark+
              :demand t
              :bind
              ("C-x x <right>" . bmkp-next-bookmark)
              ("C-x x <left>" . bmkp-previous-bookmark))

15. emjois

    <https://github.com/iqbalansari/emacs-emojify>
    
        (use-package emojify
          :init
          (global-emojify-mode))

16. Cursor/Region related

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
    
    1.  multiple-cursors
    
        <https://github.com/magnars/multiple-cursors.emacs-lisp>
        
            (use-package multiple-cursors
              :bind (("C-<mouse-1>" . mc/add-cursor-on-click)
                     ("C->" . mc/mark-next-like-this)
                     ("C-<" . mc/mark-previous-like-this)
                     ("C-c C->" . mc/mark-all-like-this)
                     ("C-c C-SPC" . mc/edit-lines)
                     ))

17. Folding/Hide Show

    [hs-minor-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html) allows hiding and showing different blocks of text/code (folding).
    
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
    
    \#+end\_src

18. jinx : the enchanted spell checker

        (use-package jinx
          :hook (emacs-startup . global-jinx-mode)
          :bind (("M-$" . jinx-correct)
                 ("C-M-$" . jinx-languages)))

19. flyspell

    supereded by [jinx : the enchanted spell checker](#org6ce83a0)
    
    :ID:       9f285553-52e6-41f2-aa76-386ef9abe279
    
        (use-package flyspell
          :disabled
          :config
          (defun flyspell-check-next-highlighted-word ()
            "Custom fnction to spell check next highlighted word"
            (interactive)
            (flyspell-goto-next-error)
            (ispell-word)
            )
        
          :bind (("C-<f8>" . flyspell-mode)
                 ("S-<f8>" . flyspell-check-previous-highlighted-word)
                 ("C-S-<f8>" . flyspell-buffer)
                 ("M-<f8>" . flyspell-word)
                 )
          ;; :hook
          ;; (prog-mode .  (flyspell-prog-mode))
          )

20. rg, ripgrep

    rg is pretty quick
    
        (use-package
          ripgrep)

21. provide

        (provide 'rgr/general-config)


## Minibuffer Enrichment (search, sudo edit&#x2026;)

Various plugins for minibuffer enrichment

Raw: [rgr/minibuffer](etc/elisp/rgr-minibuffer.el)

    (require 'rgr/minibuffer "rgr-minibuffer" 'NOERROR)


### library

1.  file opening

    1.  read only by default
    
        Increasingly editing by mistake. Can use [read-only-mode](help:read-only-mode) to edit it.
        
            (defun maybe-read-only-mode()
              (when (cond ((eq major-mode 'org-mode) t))
                (message "Setting readonly mode for %s buffer" major-mode)
                (read-only-mode +1)))
                                                    ;(add-hook 'find-file-hook 'maybe-read-only-mode)
    
    2.  [sudo-edit](https://github.com/nflath/sudo-edit) Priviliged file editing
    
            (use-package sudo-edit)
    
    3.  find file at point
    
            (use-package ffap
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

2.  [Prescient](https://github.com/raxod502/prescient.el) provides sorting and filtering.

    Disabled as I cant get my head around it. It doesnt seem to do much thats important to me
    that corfu and orderless and vertico do. Corfu and vertico already do recency ordering.
    
        (use-package prescient )

3.  Consult

    Consult]] Provides various commands based on the Emacs completion function completing-read
    
    :ID:       ec5375c7-4387-42a1-8938-5fad532be79b
    
          ;; Example configuration for Consult
        ;; Example configuration for Consult
        (use-package consult
          ;; Replace bindings. Lazily loaded by `use-package'.
          :bind (;; C-c bindings in `mode-specific-map'
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
    
    1.  consult-omni
    
        <https://github.com/armindarvish/consult-omni>
        consult-omni is a package for getting search results from one or several custom sources (web search engines, AI assistants, elfeed database, org notes, local files, desktop applications, mail servers, …) directly in Emacs minibuffer.
        
            (use-package consult-omni
              :straight (consult-omni :type git :host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el"))
              :after consult
              :config
            (require 'consult-omni-sources)
            (require 'consult-omni-wikipedia)
              )

4.  [Embark](https://github.com/oantolin/embark) Emacs Mini-Buffer Actions Rooted in Keymaps

        (use-package embark
          :ensure t
        
          :bind
          (("C-." . embark-act)         ;; pick some comfortable binding
           ("C-;" . embark-dwim)        ;; good alternative: M-.
           ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
        
          :init
        
          ;; Optionally replace the key help with a completing-read interface
          (setq prefix-help-command #'embark-prefix-help-command)
        
          ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
          ;; strategy, if you want to see the documentation from multiple providers.
          ;;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
          ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
        
          :config
        
          ;; Hide the mode line of the Embark live/completions buffers
          (add-to-list 'display-buffer-alist
                       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                         nil
                         (window-parameters (mode-line-format . none)))))
    
    1.  embark-consult
    
            ;; Consult users will also want the embark-consult package.
            (use-package embark-consult
              :ensure t ; only need to install it, embark loads it after consult if found
              :hook
              (embark-collect-mode . consult-preview-at-point-mode))

5.  [Marginalia](https://en.wikipedia.org/wiki/Marginalia) margin annotations for info on line

    are marks or annotations placed at the margin of the page of a book or in this case helpful colorful annotations placed at the margin of the minibuffer for your completion candidates
    
    :ID:       f3802197-c745-40b2-ac0d-72d7da291aaf
    
        ;; Enable rich annotations using the Marginalia package
        (use-package marginalia
          ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
          ;; available in the *Completions* buffer, add it to the
          ;; `completion-list-mode-map'.
          :bind (:map minibuffer-local-map
                      ("M-A" . marginalia-cycle))
        
          ;; The :init section is always executed.
          :init
        
          ;; Marginalia must be activated in the :init section of use-package such that
          ;; the mode gets enabled right away. Note that this forces loading the
          ;; package.
          (marginalia-mode))

6.  all-the-icons

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

7.  provide

        (provide 'rgr/minibuffer)


## Completion

Let emacs suggest completions

Raw:[rgr/completion](etc/elisp/rgr-completion.el)

    (require 'rgr/completion "rgr-completion" 'NOERROR)


### library

1.  corfu

        (use-package corfu
          :disabled
          ;; Optional customizations
          :custom
          ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
          (corfu-auto nil)                 ;; Enable auto completion
          (corfu-popupinfo-mode t)
          ;; (corfu-separator ?\s)          ;; Orderless field separator
          ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
          ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
          ;; (corfu-preview-current nil)    ;; Disable current candidate preview
          ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
          ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
          ;; (corfu-scroll-margin 5)        ;; Use scroll margin
        
          ;; Enable Corfu only for certain modes.
          ;; :hook ((prog-mode . corfu-mode)
          ;;        (shell-mode . corfu-mode)
          ;;        (eshell-mode . corfu-mode))
        
          ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
          ;; be used globally (M-/).  See also the customization variable
          ;; `global-corfu-modes' to exclude certain modes.
          :init
          (defun corfu-enable-in-minibuffer ()
            "Enable Corfu in the minibuffer."
            (when (local-variable-p 'completion-at-point-functions)
              ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
              (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                          corfu-popupinfo-delay nil)
              (corfu-mode 1)))
          (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
        
          (global-corfu-mode))
        
        
        ;; A few more useful configurations...
        (use-package emacs
          :init
          ;; TAB cycle if there are only few candidates
          (setq completion-cycle-threshold 3)
        
          ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
          ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
          ;; (setq read-extended-command-predicate
          ;;       #'command-completion-default-include-p)
        
          ;; Enable indentation+completion using the TAB key.
          ;; `completion-at-point' is often bound to M-TAB.
          (setq tab-always-indent 'complete))
    
    1.  Cape capf extensions
    
        <https://github.com/minad/cape>
        
            ;; Add extensions
            (use-package cape
              :demand t
              ;; Bind dedicated completion commands
              ;; Alternative prefix keys: C-c p, M-p, M-+, ...
              :bind (("C-c p p" . completion-at-point) ;; capf
                     ("C-c p t" . complete-tag)        ;; etags
                     ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
                     ("C-c p h" . cape-history)
                     ("C-c p f" . cape-file)
                     ("C-c p k" . cape-keyword)
                     ("C-c p s" . cape-elisp-symbol)
                     ("C-c p e" . cape-elisp-block)
                     ("C-c p a" . cape-abbrev)
                     ("C-c p l" . cape-line)
                     ("C-c p w" . cape-dict)
                     ("C-c p :" . cape-emoji)
                     ("C-c p \\" . cape-tex)
                     ("C-c p _" . cape-tex)
                     ("C-c p ^" . cape-tex)
                     ("C-c p &" . cape-sgml)
                     ("C-c p r" . cape-rfc1345))
              :init
              ;; Add to the global default value of `completion-at-point-functions' which is
              ;; used by `completion-at-point'.  The order of the functions matters, the
              ;; first function returning a result wins.  Note that the list of buffer-local
              ;; completion functions takes precedence over the global list.
              (add-to-list 'completion-at-point-functions #'cape-dabbrev)
              (add-to-list 'completion-at-point-functions #'cape-file)
              (add-to-list 'completion-at-point-functions #'cape-elisp-block)
              ;;(add-to-list 'completion-at-point-functions #'cape-history)
              ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
              ;;(add-to-list 'completion-at-point-functions #'cape-tex)
              ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
              ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
              ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
              (add-to-list 'completion-at-point-functions #'cape-dict)
              ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
              ;;(add-to-list 'completion-at-point-functions #'cape-line)
              )

2.  Which Key

    [which-key](https://github.com/justbur/emacs-which-key) shows you what further key options you have if you pause on a multi key command.
    
        (use-package
          which-key
          :demand t
          :config (which-key-mode))

3.  Yasnippet

    [YASnippet](https://github.com/joaotavora/yasnippet)  is a template system for Emacs.
    
        (use-package yasnippet
          :config
          (use-package yasnippet-snippets)
          :init
          (yas-global-mode))

4.  Abbrev Mode

    [Abbrev Mode](https://www.emacswiki.org/emacs/AbbrevMode#toc4) is very useful for expanding small text snippets
    
        (setq-default abbrev-mode 1)
        (defadvice expand-abbrev (after my-expand-abbrev activate)
          ;; if there was an expansion
          (if ad-return-value
              ;; start idle timer to ensure insertion of abbrev activator
              ;; character (e.g. space) is finished
              (run-with-idle-timer 0 nil
                                   (lambda ()
                                     ;; if there is the string "@@" in the
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

5.  company

        (use-package company
          ;;:disabled
          :config
          (use-package company-box
            :config
            (setf (alist-get 'internal-border-width company-box-doc-frame-parameters) 1)
            :hook (company-mode . company-box-mode))
          :hook
          (prog-mode . company-mode)
          :bind( :map company-mode-map
                 ("<tab>" .  company-indent-or-complete-common)))

6.  [Orderless](https://github.com/oantolin/orderless) provides an orderless completion style that divides the pattern into space-separated components

        (use-package orderless
          :init
          ;; Tune the global completion style settings to your liking!
          ;; This affects the minibuffer and non-lsp completion at point.
          (setq completion-styles '(
                                    orderless
                                    ;;partial-completion
                                    basic)
                completion-category-defaults nil
                completion-category-overrides nil);;'((file (styles partial-completion))))
          ;; A few more useful configurations...
          (use-package emacs
            :init
            ;; Add prompt indicator to `completing-read-multiple'.
            ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
            (defun crm-indicator (args)
              (cons (format "[CRM%s] %s"
                            (replace-regexp-in-string
                             "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                             crm-separator)
                            (car args))
                    (cdr args)))
            (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
        
            ;; Do not allow the cursor in the minibuffer prompt
            (setq minibuffer-prompt-properties
                  '(read-only t cursor-intangible t face minibuffer-prompt))
            (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
        
            ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
            ;; Vertico commands are hidden in normal buffers.
            ;; (setq read-extended-command-predicate
            ;;       #'command-completion-default-include-p)
        
            ;; Enable recursive minibuffers
            (setq enable-recursive-minibuffers t)))

7.  vertico , vertical interactive completion

    <https://github.com/minad/vertico>
    
        ;; Enable vertico
        (use-package vertico
          ;;:disabled
          :custom
          (vertico-cycle t)
          :config
          (use-package vertico-prescient
            :disabled
            :init (vertico-prescient-mode)
            :custom
            (vertico-prescient-enable-sorting nil))
          :init
          (vertico-mode))

8.  Abbrev Mode

    [Abbrev Mode](https://www.emacswiki.org/emacs/AbbrevMode#toc4) is very useful for expanding small text snippets
    
        (setq-default abbrev-mode 1)

9.  provide

        (provide 'rgr/completion)


## Org functionality

General org-mode config

Raw: [rgr/org](etc/elisp/rgr-org.el)

    (require 'rgr/org "rgr-org" 'NOERROR)


### library

1.  Org Mode, org-mode

        (use-package org
          :custom
          (org-agenda-files (no-littering-expand-etc-file-name "org/agenda-files.txt"))
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
    
    1.  org-contrib
    
            (use-package org-contrib)
    
    2.  org-id
    
        create unique link IDs when sharing a link to an org section
        
            (require 'org-id)
    
    3.  crypt
    
            (require 'org-crypt)
            (org-crypt-use-before-save-magic)
    
    4.  async babel blocks
    
            (use-package ob-async)
    
    5.  org-super-agenda
    
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
                                  (:name "Linux related"
                                         :and (:tag ("linux") :not (:tag "emacs")))
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
    
    6.  github compliant markup
    
            (use-package
              ox-gfm
              :demand)

2.  provide

        (provide 'rgr/org)

3.  org agenda files

    See `org-agenda-files` [org-agenda-files](#orgcaaa98b)
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


## LaTeX

Need to install dvipng, texlive, texlive-latex-extra

Raw: [rgr/typesetting](etc/elisp/rgr-typesetting.el)

    (require 'rgr/typesetting "rgr-typesetting" 'NOERROR)


### library

1.  auctex

        (use-package auctex
          :init
          (require 'ox-latex)
          (use-package lsp-latex)
          :custom
          (TeX-auto-save t)
          (TeX-parse-self t)
          (TeX-master nil)
          (TeX-PDF-mode t)
          (org-preview-latex-default-process 'dvipng)
          :config
          (defun rgr/latex-mode-hook()
            ;; buggy as this then gets savd to the global abbrevs
            (load-file (no-littering-expand-etc-file-name "abbrev/latex-songbook-chords.el"))
            (setq abbrevs-changed nil)
            (turn-on-reftex)
            (visual-line-mode)
            (LaTeX-math-mode)
            (flyspell-mode)
            (lsp-deferred))
        
          :hook
          (TeX-mode .
                    (lambda () (rgr/latex-mode-hook)(TeX-fold-mode 1)))); Automatically activate TeX-fold-mode.

2.  lilypond

    <https://lilypond.org/doc/v2.23/Documentation/usage/text-editor-support>
    
        (use-package
          lilypond
          :disabled)
    
    1.  example

3.  provide

        (provide 'rgr/typesetting)


## Lazy Language Learning, lazy-lang-learn

My own hack for popping up text to learn

    (use-package lazy-lang-learn
      :straight (lazy-lang-learn :local-repo "~/development/projects/emacs/lazy-lang-learn" :type git :host github :repo "rileyrg/lazy-lang-learn" )
      :bind
      ("C-c L" . lazy-lang-learn-mode)
      ("<f12>" . lazy-lang-learn-translate)
      ("S-<f12>" . lazy-lang-learn-translate-from-history))


## Reference/Lookup/Media

lookup and reference uilities and config

Raw: [rgr/reference](etc/elisp/rgr-reference.el)

    (require 'rgr/reference "rgr-reference" 'NOERROR)


### library

**\***

1.  chatgpt

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

2.  Ellama

    <https://github.com/s-kostyaev/ellama>
    <https://github.com/s-kostyaev/ellama>
    <https://ollama.ai/library/zephyr>
    
        (use-package ellama
          :custom
          (ellama-sessions-directory (no-littering-expand-var-file-name "ellama-sessions"))
          :init
          (setopt ellama-language "German")
          (require 'llm-ollama))

3.  web lookup/view

    1.  eww
    
            (use-package eww
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
                (alert "Launching external browser")
                (call-process-shell-command "swaymsg workspace number 2" nil 0)
                (eww-browse-with-external-browser)
                (quit-window))
              :hook (eww-after-render . rgr/eww-after-render)
              :bind
              ("C-c o" . 'eww)
              (:map eww-mode-map
                    ( "&" . rgr/eww-launch-external-browser-from-buffer)))
    
    2.  go-translate
    
        This is a translation framework for emacs, and is flexible and powerful.
        
            (use-package go-translate
              ;;:disabled
              :custom
              (gts-translate-list '(("en" "de")))
              (gts-default-translator
               (gts-translator
                :picker (gts-prompt-picker)
                :engines (list (gts-bing-engine) (gts-google-engine))
                :render (gts-buffer-render)))
              :bind
              ("C-c T" . gts-do-translate))
        
        1.  Google translate
        
                (use-package google-translate
                  :disabled ;; use go-translate
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
                    (let  ((phrase (if phrase phrase (rgr/thing-at-point-dwim))))
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
    
    3.  utility funcs
    
        A bit of a dogs dinner.

4.  use browser for docs

        (use-package emacs
          :init
          (defcustom rgr/browser-doc-url "https://www.google.com/search?q=%s" "format url variable used for function `rgr/browser-doc-search'")
          (defun rgr/browser-doc-search(&optional sym)
            "call function `browse-url' with a url variable `rgr/browser-doc-url' formatted with variable `sym'"
            (interactive
             (list
              (let((sym (replace-regexp-in-string  "^\\." "" (rgr/kill-dwim) )))
                (read-string (format "search(%s):" sym)
                             nil nil sym))))
             (browse-url (format rgr/browser-doc-url sym))))

5.  Dictionary,Thesaurus

    The more emacsy [Dictionary](https://melpa.org/#/dictionary) .
    
        (use-package
          dictionary
          :commands (rgr/dictionary-search)
          :custom
          (dictionary-server "dict.org")
          ;;(dictionary-server "localhost")
          :config
          (use-package mw-thesaurus)
          (defun rgr/dictionary-search(&optional w)
            (interactive)
            (dictionary-search (if w w (rgr/thing-at-point-dwim))))
          :bind
          ("<f6>" . rgr/dictionary-search)
          ("S-<f6>" . mw-thesaurus-lookup-dwim))

6.  GoldenDict - external lookup and reference

    When using goldendict-dwim why not add your program to the wonderful [GoldenDict](http://goldendict.org/)? A call to [trans-shell](https://github.com/soimort/translate-shell) in the dictionary programs tab gives us google translate:-
    
        trans -e google -s de -t en -show-original y -show-original-phonetics n -show-translation y -no-ansi -show-translation-phonetics n -show-prompt-message n -show-languages y -show-original-dictionary n -show-dictionary n -show-alternatives n "%GDWORD%"
    
        (use-package
          goldendict
          :commands (goldendict-dwim)
          :config
          (defun goldendict-dwim
              (&optional
               w)
            "lookup word at region, thing at point or prompt for something, in goldendict. Use a prefix to force prompting."
            (interactive)
            (let ((w (if w w (rgr/thing-at-point-dwim))))
              (call-process-shell-command (format  "goldendict \"%s\"" w ) nil 0)))
          :bind (("C-x G" . goldendict-dwim)))

7.  dash-docs

    I'm not sure if this works unless you pay now.
    
        (use-package dash-docs
          :disabled t
          :custom
          (dash-docs-common-docsets dash-docs-common-docsets)
          (dash-docs-docsets-path (no-littering-expand-var-file-name "dashdocs"))
          :config
          (defun rgr/dash-search(&optional s)
            (interactive)
            (let ((sym (if s s (thing-at-point 'symbol))))
              (message "dash docs search %s" sym)
              (dash-docs-search sym)))
          :bind
          ("C-S-a" . rgr/dash-search ))

8.  devdocs-browser

    <https://github.com/blahgeek/emacs-devdocs-browser> :
    
    Browse devdocs.io documents inside Emacs!
    
        (use-package devdocs-browser
          :custom
          (devdocs-data-dir (no-littering-expand-var-file-name  "devdocs-browser"))
          (devdocs-browser-cache-directory (no-littering-expand-var-file-name  "devdocs-browser/cache"))
          (devdocs-browser-data-directory (no-littering-expand-var-file-name  "devdocs-browser/data"))
          :config
          (defun rgr/devdocs()
            "If in an emacs-lisp buffer or bable block use `rgr/elisp-lookup-reference' else devdocs."
            (interactive)
            (if (or (derived-mode-p  'emacs-lisp-mode) (and (eq
                                                             major-mode 'org-mode) (string= "emacs-lisp" (car (org-babel-get-src-block-info)))))
                (rgr/emacs-lisp-help)
              (devdocs-browser-open)))
          :bind
          ("C-q" . rgr/devdocs))

9.  Elfeed

    [Elfeed](https://github.com/skeeto/elfeed) is an extensible web feed reader for Emacs, supporting both Atom and RSS.
    
        (use-package elfeed
          :config
          (use-package elfeed-org
            :custom
            (rmh-elfeed-org-files (list (no-littering-expand-etc-file-name "elfeed/elfeed.org")))
            :config
            (elfeed-org))
          (run-at-time nil (* 8 60 60) #'elfeed-update)
          :bind
          ( "C-c w" . elfeed)
          (:map elfeed-show-mode-map
                ("b" . (lambda()(call-process-shell-command "swaymsg workspace number 2" nil 0)(interactive)(elfeed-show-visit t))))
          (:map elfeed-search-mode-map
                ("b" . (lambda()(call-process-shell-command "swaymsg workspace number 2" nil 0)(interactive)(elfeed-search-browse-url t)))))
    
    1.  elfeed-org

10. pdf-tools

    [pdf-tools](https://github.com/politza/pdf-tools) is, among other things, a replacement of DocView for PDF files
    
        (use-package pdf-tools
          :after (org-plus-contrib)
          :config
          (pdf-tools-install)
          (add-hook 'pdf-isearch-minor-mode-hook (lambda () ;; (ctrlf-local-mode -1)
                                                   ))
          (use-package org-pdftools
            :hook (org-mode . org-pdftools-setup-link)))
    
    1.  requirements
    
            sudo apt install libpng-dev zlib1g-dev libpoppler-glib-dev libpoppler-private-dev imagemagick

11. impatient-showdow, markdown view live

    Preview markdown buffer live over HTTP using showdown.
    <https://github.com/jcs-elpa/impatient-showdown>
    
        (use-package impatient-showdown
          :disabled
          :hook (markdown-mode . impatient-showdown-mode))

12. provide

        (provide 'rgr/reference)


## Shells and Terminals

lookup and reference uilities and config

Raw: [rgr/shells](etc/elisp/rgr-shells.el)

    (require 'rgr/shells "rgr-shells" 'NOERROR)


### library

1.  EAT

    [Emulate A Terminal](https://codeberg.org/akib/emacs-eat), in a region, in a buffer and in Eshell
    
        (use-package  eat
          :custom
          (eat-kill-buffer-on-exit t)
          :config
          (defun rgr/eat()
              (interactive)
              (split-window)
              (eat))
          :bind
          ("M-g t" . rgr/eat)
          :straight (:type git
                           :host codeberg
                           :repo "akib/emacs-eat"
                           :files ("*.el" ("term" "term/*.el") "*.texi"
                                   "*.ti" ("terminfo/e" "terminfo/e/*")
                                   ("terminfo/65" "terminfo/65/*")
                                   ("integration" "integration/*")
                                   (:exclude ".dir-locals.el" "*-tests.el"))))

2.  VTERM

        (use-package  vterm)

3.  provide

        (provide 'rgr/shells)


## Email

Raw: [rgr/email](etc/elisp/rgr-email.el)

    (require 'rgr/email "rgr-email" 'NOERROR)


### library

1.  notmuch

        (use-package notmuch
          :init
          (defun rgr/notmuch-new()
            (message "refreshing notmuch")
            (call-process-shell-command "notmuch new" nil 0))
          :hook
          (notmuch-hello-mode . rgr/notmuch-new)
          )

2.  mu4e

        (use-package mu4e
          ;;:disabled
          :straight ( :host github
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
          ( mu4e-mu-binary (expand-file-name "build/mu/mu" (straight--repos-dir "mu")) )
          ( mu4e-update-interval nil )
          ( mu4e-use-fancy-chars t )
          ( mu4e-view-prefer-html nil )
          ( mu4e-view-show-addresses t )
          ( smtpmail-smtp-service 587 )
          ( user-full-name "Richard G.Riley" )
          :config
        
          (use-package mu4e-alert
            :init
            (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))
        
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
                         ("g" . rgr/mu4e-refresh))
                   (:map mu4e-headers-mode-map
                         ("v" . mu4e-view-action)
                         ("C-c u" . mu4e-headers-mark-all-unread-read))))
        ;;:map mu4e-view-mode-map
        ;;   ("V" . '(lambda()(message "%s" (mu4e-message-at-point))))))) ;; mu4e-action-view-in-browser))))

3.  provide

        (provide 'rgr/email)


## Chat

Chat SW

Raw: [rgr/chat](etc/elisp/rgr-chat.el)

    (require 'rgr/chat "rgr-chat" 'NOERROR)


### library

1.  erc

        
        (use-package erc
          :demand
          :config
          (defun rgr/erc-switch-to-channel(&optional channel)
            (when (string= (or channel "#emacs") (buffer-name (current-buffer)))
              (switch-to-buffer (current-buffer))))
        
          (defun rgr/erc-start()
            (interactive)
            (unless(get-buffer "libera.chat:6697")
              (progn
                (erc-tls :server "irc.libera.chat" :port 6697)
                (add-hook 'erc-join-hook 'rgr/erc-switch-to-channel))))
        
          (defun rgr/erc-quit()
            (interactive)
            (erc-quit-server ""))
        
          :bind
           (:map erc-mode-map
                  (("C-c C-q" . rgr/erc-quit))))

2.  provide

        (provide 'rgr/chat)


## Programming Language related

Raw: [rgr/programming](etc/elisp/rgr-programming.el)

    (require 'rgr/programming "rgr-programming" 'NOERROR)


### library

1.  compilation

        (global-set-key (kbd "C-c C-r") 'recompile)

2.  indent bars

        (use-package indent-bars
          :disabled
          :ensure t
          :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
          :hook
          (prog-mode . indent-bars-mode))

3.  JSON

        (use-package json-mode)

4.  Treemacs

        (use-package
          treemacs
          :init
          (add-to-list 'image-types 'svg)
          :custom
          (treemacs-follow-after-init t)
          :config
          (treemacs-follow-mode +1)
          (treemacs-fringe-indicator-mode)
          (treemacs-git-mode 'deferred)
          (use-package treemacs-magit)
          (use-package treemacs-projectile)
          :bind
          ("M-9"   . 'treemacs-select-window)
          (:map treemacs-mode-map
                ("<right>" . treemacs-peek)))

5.  duplicate thing

        (use-package duplicate-thing
          :bind
          ("C-S-d" . 'duplicate-thing))

6.  Breadcrumbs

    <https://github.com/joaotavora/breadcrumb>
    
        (use-package breadcrumb
          :straight (breadcrumb :local-repo "~/development/projects/emacs/breadcrumb"))
    
    1.  rmsbolt
    
        RMSbolt is a compiler output viewer in Emacs.
        <https://github.com/emacsmirror/rmsbolt>
        
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
    
    2.  parrot
    
            (defun my/parrot-animate-when-compile-success (buffer result)
              (if (string-match "^finished" result)
                  (parrot-start-animation)))
            
            (use-package parrot
              :ensure t
              :config
              (parrot-mode)
              (add-to-list 'compilation-finish-functions 'my/parrot-animate-when-compile-success))

7.  prog-mode hack

        (unless (fboundp 'prog-mode)
          (defalias 'prog-mode 'fundamental-mode))

8.  undo tree

        (use-package undo-tree
          :init
          (global-undo-tree-mode))

9.  Show Line numbers

        (global-set-key (kbd "S-<f2>") 'display-line-numbers-mode)
        (add-hook 'prog-mode-hook (lambda() (display-line-numbers-mode t)))

10. code format

        ;; auto-format different source code files extremely intelligently
        ;; https://github.com/radian-software/apheleia
        (use-package apheleia
          :disabled
          :ensure t
          :config
          (apheleia-global-mode +1))

11. Project Management

    1.  projectile
    
            (use-package projectile
              :demand
              :config
              (projectile-mode +1)
              (defun rgr/projectile-term()
                (interactive)
                (if (string-equal major-mode "eat-mode")
                    (previous-buffer)
                  (let ((default-directory (projectile-project-root)))
                    (split-window-below)
                    (other-window 1)
                    (eat))))
              :bind
              (:map projectile-mode-map
                    (("C-x p" . projectile-command-map)))
              (:map projectile-command-map
                    (( "b" . consult-project-buffer)
                     ("t" . #'rgr/projectile-term))))
        
        1.  projectile npm support
        
                (projectile-register-project-type 'npm '("package.json")
                                                  :project-file "package.json"
                                                  :compile "npm install"
                                                  :test "npm test"
                                                  :run "alacritty --command tmux new-session -A -s 'npm projectile' 'npm start'"
                                                  :test-suffix ".spec")
        
        2.  org-projectile
        
            <https://github.com/IvanMalison/org-projectile>
            
                (use-package org-project-capture
                  :demand
                  :custom
                  (org-project-capture-per-project-filepath "TODO.org")
                  :config
                  (use-package org-projectile :demand)
                  (setq org-project-capture-default-backend
                        (make-instance 'org-project-capture-projectile-backend))
                  (org-project-capture-per-project)
                  (push (org-projectile-project-todo-entry) org-capture-templates) ;; this doesnt work. I had to exec it then save in custom
                  :bind (("C-c n p" . org-projectile-project-todo-completing-read)))

12. BASH

    1.  Navigating Bash set -x output
    
            ;; try to work with next-error for bash's "set -x" output
            (use-package compile
              :config
              (add-to-list 'compilation-error-regexp-alist
                           'bash-set-x)
              (add-to-list 'compilation-error-regexp-alist-alist
                           '(pascal
                             "\\(.+?\\)\\(\\([0-9]+\\),\\([0-9]+\\)\\).*" 1 2 3)))

13. PHP

        (use-package php-mode
          :hook
          (php-mode . (lambda()(lsp-deferred))))

14. JSON, YAML Configuration files

    1.  YAML
    
            (use-package yaml-mode)
    
    2.  json
    
            (use-package json-reformat)
            (use-package hydra)

15. FlyCheck

        (use-package flycheck
          :disabled t
          :ensure t
          :config
          (use-package flycheck-pos-tip
            :config
            (flycheck-pos-tip-mode))
          (add-hook 'after-init-hook #'global-flycheck-mode))

16. Flymake

        (use-package flymake
          :demand t
          :init
          (defun rgr/flymake-hook()
            (setq-local next-error-function 'flymake-goto-next-error))
          (add-hook 'flymake-mode-hook  #'rgr/flymake-hook)
          :config
          (use-package flymake-diagnostic-at-point
            :after flymake
            :config
            (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
          :bind
          ("M-n" . next-error)
          ("M-p" . previous-error))
    
    1.  shellcheck
    
            
            (use-package flycheck-bashate
              :disabled t
              :after (flycheck)
              :config
              (flycheck-bashate-setup))
            
            (use-package flymake-shellcheck
              :disabled t
              :commands flymake-shellcheck-load
              :init
              (defun rgr/sh-mode-hook()
                (flymake-shellcheck-load)
                (flymake-mode +1))
              :hook (sh-mode . rgr/sh-mode-hook))

17. Version Control

    1.  It's [Magit](Https://github.com/magit/magit)! A Git porcelain inside Emacs
    
            (use-package
              magit
              :init
              (use-package magit-filenotify)
              (use-package transient
                :straight (transient :type git :host github :repo "magit/transient"))
              :bind
              ("C-x g" . magit-status)
              :hook
              (magit-status-mode . magit-filenotify-mode))
    
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
    
    3.  Git Gutter
    
        1.  diff-hl mode
        
            I prefer this to [git-gutter.el](https://github.com/emacsorphanage/git-gutter).
            
                (use-package diff-hl
                  :hook
                  (server-after-make-frame .  global-diff-hl-mode)
                  (magit-pre-refresh . diff-hl-magit-pre-refresh)
                  (magit-post-refresh . diff-hl-magit-post-refresh)
                  :bind
                  ("C-x v ="  . diff-hl-show-hunk))

18. Dart/Flutter

    Running emulator from command line:
    
        emulator -avd Pixel_6_Pro_API_33
    
        (use-package dart-mode
          :disabled
          :custom
          (lsp-dart-flutter-widget-guides t)
          :init
          (use-package flutter
            :after dart-mode
            :custom
            (flutter-sdk-path "~/bin/thirdparty/flutter")
            :config
            (use-package flutter-l10n-flycheck)
            (setenv "JAVA_HOME" (concat (getenv "ANDROID_STUDIO_HOME") "/jbr"))
            :bind (:map dart-mode-map
                        ("C-M-x" . (lambda()
                                     (interactive)
                                     (save-buffer)
                                     (flutter-run-or-hot-reload))))
            :hook   (dart-mode . (lambda()
                                   (flutter-test-mode))))
          :config
          (use-package lsp-dart :after lsp)
          (defun rgr/init-dart-buffer()
            (lsp-deferred) )
          :hook   (dart-mode . rgr/init-dart-buffer ))
    
    1.  Java
    
            ;; (use-package emacs
            ;;   :hook (java-mode . eglot-ensure)
            ;;   )

19. Tree Sitter

    1.  treesit-auto
    
        Automatically install and use tree-sitter major modes in Emacs 29+. If the tree-sitter version can’t be used, fall back to the original major mode.
        
            (use-package treesit-auto
              :custom
              (treesit-auto-install 'prompt)
              (treesit-extra-load-path `(,(no-littering-expand-var-file-name "tree-sitter")))
              :config
              (treesit-auto-add-to-auto-mode-alist 'all)
              (global-treesit-auto-mode))
        
        \*\*\*\*JavaScript Family
    
    2.  Javascript
    
            (use-package js
              :demand t
              :init
              (add-to-list 'auto-mode-alist '("\\.mjs" . javascript-mode)) ;; js module file
              (defun rgr/javascript-typescript-common-mode-hook ()
                (electric-pair-mode 1)
                (setq-local rgr/complete-line-f 'rgr/complete-c-line)
                (lsp-deferred)
                )
              :config
              (defun rgr/js-ts-mode-hook ()
                )
              :hook
              (js-ts-mode . rgr/javascript-typescript-common-mode-hook)
              (js-ts-mode . rgr/js-ts-mode-hook))
    
    3.  Typescript
    
            (use-package typescript-ts-mode
              :demand t
              :init
              (defun rgr/typescript-ts-mode-hook ()
                )
              :hook
              (typescript-ts-mode .  rgr/javascript-typescript-common-mode-hook)
              (typescript-ts-mode .  rgr/typescript-ts-mode-hook))

20. Language Server Protocol (LSP), lsp-mode

    [Emacs-lsp](https://github.com/emacs-lsp) : Language Server Protocol client for Emacs
    
    Raw: [rgr/lsp](etc/elisp/rgr-lsp.el)
    
        (require 'rgr/lsp "rgr-lsp" 'NOERROR)
    
    1.  library
    
        1.  lsp
        
                (use-package lsp-mode
                  ;;:disabled
                  :init
                  (setq gc-cons-threshold (* 100 1024 1024)
                        read-process-output-max (* 1024 1024))
                  (use-package lsp-ui
                    ;;:disabled t
                    :init
                    (use-package dap-mode
                      :disabled t
                      :bind (:map dap-mode-map
                                  (("<f8>" . dap-next)
                                   ("S-<f8>" . dap-continue)
                                   ("<f7>" . dap-step-in)
                                   ("S-<f7>" . dap-step-out)
                                   ("M-<f8>" . dap-debug)
                                   ("C-<f8>" . dap-disconnect)
                                   ))
                      :init
                      (require 'dap-cpptools)
                      (dap-cpptools-setup))
                    (defun rgr/lsp-ui-mode-hook()
                      (message "rgr/lsp-ui-mode-hook")
                      ;;(dap-mode t)
                      (lsp-ui-doc-mode -1)
                      (when buffer-file-name
                        (setq-local buffer-save-without-query t))
                      ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
                      )
                    :custom
                    (lsp-ui-doc-mode 1)
                    :bind (:map lsp-ui-mode-map
                                ("C-h ." . lsp-ui-doc-focus-frame)
                                ("C-h d" . lsp-ui-doc-mode)
                                ("C-h g" . lsp-ui-doc-glance)
                                ("M-." . lsp-find-definition)
                                ("C-x C-i" . lsp-ui-imenu))
                    :hook
                    (lsp-ui-mode . rgr/lsp-ui-mode-hook))
                  (use-package lsp-treemacs
                    :config
                    (lsp-treemacs-sync-mode t)
                    :commands lsp-treemacs-errors-list)
                  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
                         (lsp-mode . lsp-enable-which-key-integration))
                  :commands (lsp lsp-deferred))
        
        2.  eglot
        
            Emacs lsp client
            <https://github.com/joaotavora/eglot>
            
                (use-package eglot
                  :disabled
                  ;;:straight `(eglot ,@(when (>= emacs-major-version 29) '(:type built-in)))
                  ;; :config
                  ;; (use-package eldoc-box)
                  ;; :hook
                  ;; (prog-mode . eldoc-box-hover-at-point-mode)
                  :bind
                  (:map eglot-mode-map
                        ("<C-return>" . eglot-code-actions)))
        
        3.  provide
        
                (provide 'rgr/lsp)

21. Serial Port

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

22. PlatformIO

    [platformio-mode](https://github.com/emacsmirror/platformio-mode) is an Emacs minor mode which allows quick building and uploading of PlatformIO projects with a few short key sequences.
    The build and install process id documented [here](https://docs.platformio.org/en/latest/ide/emacs.html).
    
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

23. Python

    1.  ipython
    
            (setq python-shell-interpreter "ipython")
            (setq python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
    
    2.  virtualenv
    
            (use-package auto-virtualenv
              :config
              (add-hook 'python-mode-hook  #'auto-virtualenv-set-virtualenv))

24. Haskell

    1.  haskell-mode
    
            ;; I'm typically confused when it comes to haskell. Note that the interactive stuff I cribbed doesnt work.
            (use-package haskell-mode
              :disabled t
              :config
              ;;(add-hook 'haskell-mode-hook #'eglot-ensure)
              ;;(add-hook 'haskell-literate-mode-hook #'eglot-ensure)
              (eval-after-load "haskell-mode"
                '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
              (eval-after-load "haskell-cabal"
                '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
              (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

25. lldb debugging in emacs

    1.  voltron
    
            (use-package lldb-voltron
              :straight (lldb-voltron :local-repo "~/development/projects/emacs/emacs-lldb-voltron" :type git :host github :repo "rileyrg/emacs-lldb-voltron" )
              ;;:config
              ;; (breadcrumb-mode t)
              )

26. rust

        
        (use-package rust-mode
          :ensure t
          :init
          (defcustom rgr/rust-browser-doc-url (concat (format "file://%s/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/index.html?search=" (getenv "HOME")) "%s") "used to format variable `rgr/browser-doc-url'")
          (setq rust-mode-treesitter-derive t))
        
        (use-package rustic
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
            (prettify-symbols-mode)
            (if (featurep 'yasnippet)
                (yas-minor-mode)))
          :hook
          (rustic-mode . rgr/rust-mode-hook)
          :bind
          (:map rustic-mode-map
                ("C-q" . rgr/browser-doc-search)))

27. C

    1.  c-mode-common-hook
    
            (use-package c-ts-mode
              :config
              (defun rgr/c-ts-mode-common-hook ()
                (setq-local rgr/complete-line-f 'rgr/c-complete-line)
                (message "rgr/c-ts-mode-common-hook")
                (lsp-deferred)
                ;; (if(featurep 'platformio-mode)
                ;;     (platformio-conditionally-enable))
                (if (featurep 'yasnippet)
                    (yas-minor-mode)))
              :hook
              (c-ts-mode . rgr/c-ts-mode-common-hook))

28. cc,cpp, C++, cc-mode

        (add-hook 'c++-ts-mode-hook 'rgr/c-ts-common-mode-hook)

29. Linux tools

    1.  [logview](https://github.com/doublep/logview) - view system logfiles
    
            (use-package logview
              :demand t
              :init
              (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
              (add-to-list 'auto-mode-alist '("log\\'" . logview-mode)))

30. Assembler

    1.  [x86Lookup](https://nullprogram.com/blog/2015/11/21/)
    
            (use-package strace-mode)

31. Godot GDScript

    This [package](https://github.com/GDQuest/emacs-gdscript-mode) adds support for the GDScript programming language from the Godot game engine in Emacs. It gives syntax highlighting and indentations
    
        (use-package gdscript-mode
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

32. Web,Symfony and Twig

    1.  Symfony
    
        1.  custom
        
                (defgroup rgr/symfony nil
                  "Symfony Development"
                  :group 'rgr)
                
                (defcustom symfony-server-command "~/.symfony/bin/symfony server:start"
                  "Start the symfony web server"
                  :type 'string
                  :group 'rgr/symfony)
        
        2.  Start a symfony web server when applicable
        
                (use-package php-mode
                  :disabled t
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
            
            We can trigger it using a .dir-locals.el
            
                ((php-mode
                  (eval php-mode-webserver-hook)))
        
        3.  webmode
        
                (use-package
                  web-mode
                  :disabled
                  :demand t
                  :config
                  (defun rgr/web-mode-hook()
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

33. elf-mode - view the symbol list in a binary

    [https://oremacs.com/2016/08/28/elf-mode/](https://oremacs.com/2016/08/28/elf-mode/)
    
        (use-package elf-mode
          :demand t
          :config
          (add-to-list 'magic-mode-alist '("\dELF" . elf-mode))
          (add-to-list 'auto-mode-alist '("\\.\\(?:a\\|so\\)\\'" . elf-mode)))

34. provide

        (provide 'rgr/programming)


## Emacs Lisp, ELisp Utils

Load this relatively early in order to have utils available if there's a faied load
Raw: [rgr/elisp-utils](etc/elisp/rgr-elisp-utils.el)

    (require 'rgr/elisp (expand-file-name "rgr-elisp" elisp-dir))


### library

1.  rgr/kill-dwim

        (use-package rgr-kill-dwim
          :straight (rgr-kill-dwim :local-repo "~/development/projects/emacs/rgr-kill-dwim" :type git :host github :repo "rileyrg/rgr-kill-dwim" )
          :bind
          ("M-w" . rgr/kill-dwim))

2.  rgr/emacs-lisp-help

    Use helpful if installed else built in
    
        (defun rgr/emacs-lisp-help (&optional s)
          "Elisp help at point. default to `helpful-at-point' if available, else `describe-function' or `describe-variable'."
          (interactive)
          (let* ((sym (or s (thing-at-point 'symbol)))
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
                      (alert msg))
                    (setq sym nil)))))))
    
    1.  external info files
    
            (add-to-list 'Info-directory-list (no-littering-expand-etc-file-name  "info"))

3.  smartparens

        (use-package smartparens
          :hook
          ((emacs-lisp-mode . smartparens-mode)))

4.  electric-pair-mode

    [auto insert closing brackets](info:emacs#Matching)
    
        (use-package emacs
          :hook
          ((emacs-lisp-mode . electric-pair-mode)))

5.  elisp checks

        (defun rgr/elisp-edit-mode()
          "return non nil if this buffer edits elisp"
          (member major-mode '(emacs-lisp-mode lisp-interaction-mode)))

6.  linting

    [package-lint](https://github.com/purcell/package-lint) provides a linter for the metadata in Emacs Lisp files which are intended to be packages. You can integrate it into your build process.
    
        (use-package package-lint)

7.  helpful, enriched elisp help

        (use-package helpful)

8.  elisp popup context help

    Display a poup containing docstring at point
    
        (use-package el-docstring-sap
          :straight (el-docstring-sap :local-repo "~/development/projects/emacs/el-docstring-sap" :type git :host github :repo "rileyrg/el-docstring-sap" )
          :init
          (use-package quick-peek)
          :hook
          (emacs-lisp-mode . el-docstring-sap-mode)
          :bind
          ("M-<f2>" . el-docstring-sap-display)
          ("M-<f1>" . el-docstring-sap-mode))

9.  Elisp debugging

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

10. Formatting

        (use-package
          elisp-format
          :bind
          (:map emacs-lisp-mode-map
                ("C-c f" . elisp-format-region)))

11. popup query symbol

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

12. provide

        (provide 'rgr/elisp)


## Themes

Raw: [rgr/themes](etc/elisp/rgr-themes.el)

    (require 'rgr/themes "rgr-themes" 'NOERROR)


### library

1.  modus themes

    <https://github.com/protesilaos/modus-themes>
    
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

2.  ef-themes

    <https://github.com/protesilaos/ef-themes>
    
        (use-package ef-themes
          :disabled
          :demand t
          :config
          (ef-themes-select 'ef-duo-light))

3.  provide

        (provide 'rgr/themes)


## Late load

    (load-el-gpg (no-littering-expand-etc-file-name "late-load"))


# Associated emacs things


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
    
    !info
    !info/*
    
    !tree-sitter
    !tree-sitter/*
    
    !etc
    !var
    !var/secrets
    !var/secrets/bmkp
    !var/secrets/bmkp/**
    
    
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


## Setting up emacs as a default editor using a dot desktop file and associated protocol handler


### [php.ini](editor-config/php.ini) changes e.g /etc/php/7.3/php.ini

`xdebug.file_link_format` is used by compliant apps to format a protocol uri. This is handled on my Linux system as a result of [emacsclient.desktop](#orgc0afd0c) documented below.

    xdebug.file_link_format = "emacsclient://%f@%l"
    
    xdebug.remote_enable = 1
    xdebug.remote_host = localhost
    xdebug.remote_port = 9000

    xdebug.file_link_format = "emacsclient://%f@%l"
    
    xdebug.remote_enable = 1
    xdebug.remote_host = localhost
    xdebug.remote_port = 9000


### [emacsclient-linenumber](bin/emacsclient-linenumber) script to correctly parse the protocol in order to start emacs frame at correct line

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


<a id="orgc0afd0c"></a>

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

