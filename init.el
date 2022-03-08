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

(setq custom-file  (expand-file-name  "custom.el" user-emacs-directory)) ;;
(load custom-file 'noerror)

(debug-init)

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

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; look for a debug init file and load, trigger the debugger
(debug-init "debug-init-straight.el")

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

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

(require 'rgr/minibuffer "rgr-minibuffer" 'NOERROR)

(require 'rgr/completion "rgr-completion" 'NOERROR)

(use-package bookmark+
  :custom
  (bmkp-last-as-first-bookmark-file (no-littering-expand-var-file-name "bmkp/current-bookmark.el.gpg"))
  :demand)

(use-package lazy-lang-learn
  :straight (lazy-lang-learn :local-repo "~/development/projects/emacs/lazy-lang-learn" :type git :host github :repo "rileyrg/lazy-lang-learn" )
  :bind
  ("C-c L" . lazy-lang-learn-mode)
  ("<f12>" . lazy-lang-learn-translate)
  ("S-<f12>" . lazy-lang-learn-translate-from-history))

(require  'rgr/general-config "rgr-general-config" 'NOERROR)

(require 'rgr/org "rgr-org" 'NOERROR)

(use-package emojify
  :init
  (global-emojify-mode))

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

(use-package
  ripgrep)

(require 'rgr/reference "rgr-reference" 'NOERROR)

(require 'rgr/emms "rgr-emms" 'NOERROR)

(defun eshell/emacs-clean (&rest args)
  "run a clean emacs"
  (interactive)
  (message "args are %s" args)
  (save-window-excursion
    (shell-command "emacs -Q -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el &")))

(defun eshell/_ftrace_fn (&rest args)
  "useage: _ftrace_fn &optional function-name(def:printf)  depth(def:1)
creates a report in function-name.ftrace and opens it in a buffer"
  (interactive)
  (let ((fn (or (nth 2 args) "printf"))
        (depth (or (nth 3 args) 1)))
    (shell-command (format "sudo trace-cmd record -p function_graph --max-graph-depth %s -e syscalls -F %s && trace-cmd report | tee %s.ftrace" depth fn fn))
    (switch-to-buffer (find-file-noselect (format "%s.ftrace" fn) ))))

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

(use-package vterm
  :custom
  (vterm-shell "/usr/bin/zsh")
  (vterm-max-scrollback 100000)
  :bind
  ("M-g v" . vterm))

(use-package docker)

(defun rgr/toggle-buffer(n)
  "jump to or from buffer named n else default to *Messages*"
  (interactive "bbuffer:")
  (let ((n (or n
               "*Messages*")))
    (switch-to-buffer (if (string= (buffer-name) n)
                          (other-buffer) n))))

(use-package perspective
  :custom
  (persp-state-default-file (no-littering-expand-var-file-name "perspective/perspectile.el"))
  :config
  (persp-mode)
  (add-hook 'kill-emacs-hook  #'persp-state-save)
  :bind
  ("C-x C-b" . persp-list-buffers))

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

(use-package all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook  #'all-the-icons-dired-mode))

(use-package dired-git
  :config
  :hook (dired-mode . dired-git-mode))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package dired-filter
  :init
  (define-key dired-mode-map (kbd "/") dired-filter-map))

(use-package posframe)

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

(use-package
  hyperbole
  :disabled t)

(use-package undohist
  :disabled t
  :config
  (undohist-initialize))

(use-package undo-tree
  :disabled t
  :config
  (global-undo-tree-mode))

(use-package undo-fu
  :disabled t
  :init
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package back-button
  :disabled t
  :config
  (back-button-mode 1)
  :bind
  ("M-<left>" . previous-buffer)
  ("M-<right>" . next-buffer))

(use-package ace-window
  :init
  (defalias 'other-window 'ace-window)
  :bind*
  ("M-o" . ace-window)
  ("M-S o" . ace-delete-window))

(use-package ace-link
  :demand t
  :config
  (ace-link-setup-default)
  :bind*
  (:map emacs-lisp-mode-map
        ("C-c o" . ace-link-addr))
  ("C-c o" . ace-link)
  )

(use-package ace-jump-mode
  :bind
  ("M-s c" . ace-jump-mode)
  )

(defun htop-regexp()
  (interactive)
  (let ((s (completing-read (format "HTtop filter (%s): " (symbol-at-point)) minibuffer-history nil nil (symbol-at-point))))
    (condition-case nil
        (shell-command (format "htop-regexp %s" s))
      (error nil))))
(global-set-key (kbd "C-S-p") 'htop-regexp)

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

(use-package keycast
  )

(require 'rgr/programming "rgr-programming" 'NOERROR)

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

(load-el-gpg (no-littering-expand-etc-file-name "late-load"))
