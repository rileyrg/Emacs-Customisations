(defvar bootstrap-version)
(setq straight-base-dir (expand-file-name "etc" user-emacs-directory))
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-vc-git-default-protocol 'ssh))

(use-package el-patch)

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(defvar elisp-dir (expand-file-name "elisp" no-littering-etc-directory) "my elisp directory. directories are recursively added to path.")
(add-to-list 'load-path elisp-dir)
(let ((default-directory elisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file 'noerror)

(defun load-el-gpg (load-dir)
  (message "attempting mass load from %s." load-dir)
  (when (file-exists-p load-dir)
    (dolist (f (directory-files-recursively load-dir "\.[el|gpg]$"))
      (condition-case nil
          (load f 'no-error)
        (error nil)))))

(load-el-gpg (no-littering-expand-etc-file-name "early-load"))

(load-el-gpg (expand-file-name (system-name)  (no-littering-expand-etc-file-name "hosts")))

(use-package auth-source
  :demand
  :custom
  (auth-sources '("~/.gnupg/auth/authinfo.gpg" "~/.gnupg/auth/authirc.gpp"))
  :no-require t
  )

(require 'rgr/utils "rgr-utils" 'NOERROR)

(require 'rgr/elisp-utils (expand-file-name "rgr-elisp-utils" elisp-dir))
(global-set-key (kbd "C-M-S-e") 'rgr/elisp-helpers-popup-help-enabled-toggle)

(require 'rgr/daemon "rgr-daemon" 'NOERROR)

;; (setq system-time-locale "C") ;; doesnt work with daemon
;; (format-time-string "%A")
;;(setq system-time-locale "de.UTF8")
;; (setq system-time-locale nil)
;;(format-time-string "%A")
;; (set-locale-environment "en_GB.UTF8")
;;(add-hook 'server-switch-hook (lambda()(set-locale-environment "en_GB.UTF8")))

(add-hook 'emacs-startup-hook (lambda()(setq system-time-locale "C")))

(require 'rgr/minibuffer "rgr-minibuffer" 'NOERROR)

(require 'rgr/completion "rgr-completion" 'NOERROR)

(require  'rgr/alert-learn "rgr-alert-learn" 'NOERROR)
(alert "Emacs is starting!")

(require  'rgr/general-config "rgr-general-config" 'NOERROR)

(require 'rgr/org "rgr-org" 'NOERROR)

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

(use-package
  expand-region
  :disabled
  :config (defun er/select-call-f(arg)
            (setq current-prefix-arg arg)
            (call-interactively 'er/expand-region)
            (exchange-point-and-mark))
  (defun selectFunctionCall()
    (interactive)
    (er/select-call-f 3))
  :bind ("<C-return>" . selectFunctionCall)
  ("C-c e" . er/expand-region)
  ("C-c c" . er/contract-region))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(add-hook 'prog-mode-hook (lambda()(hs-minor-mode t)))
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
  (defun force-complete-ispell()
    (interactive)
    (let ((company-backends '(company-ispell)))
      (company-complete)))

  :bind (("C-<f8>" . flyspell-mode)
         ("C-S-<f8>" . flyspell-buffer)
         ("M-<f8>" . force-complete-ispell)
         ("<f8>" . flyspell-check-next-highlighted-word)
         ("S-<f8>" . flyspell-check-previous-highlighted-word)
         ))

(use-package
  ripgrep)

(use-package
  ag)

(use-package deft
  :config
  (setq deft-directory (expand-file-name "orgfiles" user-emacs-directory))
  (setq deft-recursive t)
  :bind(("M-<f3>" . #'deft)))

(require 'rgr/reference "rgr-reference" 'NOERROR)

(require 'rgr/emms "rgr-emms" 'NOERROR)

(use-package terminal-here
  :custom
  (terminal-here-terminal-command (list "oneterminal"))
  :bind
  ("C-<f5>" . #'terminal-here-project-launch)
  ("C-S-<f5>" . #'terminal-here-launch))

(use-package
  shell-switcher
  :config (setq shell-switcher-mode t)
  ;;(add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)
  :bind
  ("M-<f12>" . shell-switcher-switch-buffer)
  ("C-<f12>" . shell-switcher-new-shell))

(defun eshell/emacs-clean (&rest args)
  "run a clean emacs"
  (interactive)
  (message "args are %s" args)
  (save-window-excursion
    (shell-command "emacs -Q -l ~/.config/emacs/straight/repos/straight.el/bootstrap.el &")))

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
    (setq eshell-path-env (concat "/home/rgr/bin:" eshell-path-env))
    (setenv "PATH" (concat "/home/rgr/bin:" (getenv "PATH")))
    (setq pcomplete-cycle-completions nil))
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
  (use-package pcomplete-extension
    :config
    (defconst pcmpl-git-commands
      '("add" "bisect" "branch" "checkout" "clone"
        "commit" "diff" "fetch" "grep"
        "./init" "log" "merge" "mv" "pull" "push" "rebase"
        "reset" "rm" "show" "status" "tag" )
      "List of `git' commands")

    (defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
      "The `git' command to run to get a list of refs")

    (defun pcmpl-git-get-refs (type)
      "Return a list of `git' refs filtered by TYPE"
      (with-temp-buffer
        (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
        (goto-char (point-min))
        (let ((ref-list))
          (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
            (add-to-list 'ref-list (match-string 1)))
          ref-list)))

    (defun pcomplete/git ()
      "Completion for `git'"
      ;; Completion for the command argument.
      (pcomplete-here* pcmpl-git-commands)
      ;; complete files/dirs forever if the command is `add' or `rm'
      (cond
       ((pcomplete-match (regexp-opt '("add" "rm")) 1)
        (while (pcomplete-here (pcomplete-entries))))
       ;; provide branch completion for the command `checkout'.
       ((pcomplete-match "checkout" 1)
        (pcomplete-here* (pcmpl-git-get-refs "heads")))))    )
  (use-package
    eshell-git-prompt
    :config
    (eshell-git-prompt-use-theme 'powerline)
    (define-advice
        eshell-git-prompt-powerline-dir
        (:override ()
                   short)
      "Show only last directory."
      (file-name-nondirectory (directory-file-name default-directory))))
  )

(use-package docker
  :after projectile
  :bind (:map projectile-mode-map ("C-c k" . docker)))

(defun rgr/toggle-buffer(n)
  "jump to or from buffer named n else default to *Messages*"
  (interactive "bbuffer:")
  (let ((n (or n
               "*Messages*")))
    (switch-to-buffer (if (string= (buffer-name) n)
                          (other-buffer) n))))

(global-set-key (kbd "C-<f2>") 'rgr/toggle-buffer)
(global-set-key (kbd "C-h d") (lambda()(interactive)(apropos-documentation (symbol-or-region-at-point-as-string-or-prompt))))
(defun kill-next-window ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (if (not (one-window-p))(progn
                            (other-window 1)
                            (kill-this-buffer))
    (message "no next window to kill!")))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x K") 'kill-next-window)
(defun rgr/switch-to-buffer-list (buffer alist)
  (message "in rgr/switch-to-buffer-list")
  (select-window  (display-buffer-use-some-window buffer alist)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package dired-git
  :config
  :hook ((dired-mode-hook . dired-git-mode)))

(use-package dired-subtree
  :config
  (use-package dash)
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package dired-filter
  :config
  (use-package dash)
  )

(use-package dired-quick-sort
  :disabled t
  :config
  (dired-quick-sort-setup))

(use-package posframe)

(use-package popper
  :ensure t
  :init
  (defvar pfb "*posframe buffer*")
  ;;(setq popper-display-function 'rgr/popper-display-posframe)
  ;; (setq popper-group-function #'popper-group-by-projectile)
  (setq popper-reference-buffers
        '(
          "\\*Messages\\*"
          help-mode
          helpful-mode
          dictionary-mode
          compilation-mode))
  (popper-mode +1)
  (defun rgr/popper-display-posframe(buf &optional o)
    (save-excursion
      (let* ((db (generate-new-buffer pfb)))
        (with-current-buffer db
          (insert-buffer buf)
          )
        (posframe-show db
                       :internal-border-width 2
                       :internal-border-color "Orange"
                       :border-width 3
                       :border-color "IndianRed"
                       :width (/ (* (frame-width) 2) 3)
                       :height (/ (* (frame-height) 2) 3)
                       :poshandler 'posframe-poshandler-frame-center
                       :position t
                       ))))
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
  ("C-c j" . ace-jump-mode)
  )

(use-package
  elscreen
  :disabled t
  :config (elscreen-start))

(defun htop-regexp()
  (interactive)
  (let ((s (completing-read (format "HTtop filter (%s): " (symbol-at-point)) minibuffer-history nil nil (symbol-at-point))))
    (condition-case nil
        (shell-command (format "htop-regexp %s" s))
      (error nil))))
(global-set-key (kbd "C-S-p") 'htop-regexp)

(use-package
  treemacs
                                        ;,:disabled t
  :config
  (treemacs-git-mode 'deferred)
  (use-package
    treemacs-projectile)

  (use-package
    treemacs-icons-dired
    :config (treemacs-icons-dired-mode))

  (use-package
    treemacs-magit
    :after treemacs
    magit)
  (defun rgr/treemacs-select-window (close)
    (interactive "P")
    (if close (treemacs)
      (treemacs-select-window)))
  :bind ("M-0"   . rgr/treemacs-select-window)
  (:map treemacs-mode-map
        ("<right>" . treemacs-peek)))

(defun www-open-current-page-external ()
  "Open the current URL in desktop browser."
  (interactive)
  (let((url (or
             (if(fboundp 'eww-current-url)
                 (eww-current-url)
               (if(fboundp 'w3m-current-url)
                   (w3m-current-url)
                 nil)))))
    (if url
        (browse-url-xdg-open url)
      (message "No buffer url"))))

(defun www-open-link-external ()
  "Open the current link or image in Firefox."
  (interactive)
  (let((anchor (url-get-url-at-point)))
    (if anchor
        (browse-url-xdg-open anchor)
      (message "No valid anchor found at cursor!"))))

(use-package
  w3m
  :disabled t
  :custom (browse-url-browser-function 'w3m-browse-url)
  :bind
  ("C-c o" . 'browse-url)
  (:map w3m-mode-map
        ("O" . www-open-current-page-external)
        ("o" . www-open-link-external)))

(use-package
  eww
  :demand t
  :commands (eww-readable-url)
  :config
  (defun make-eww-readable()
    ;; make current eww buffer eww-readable and then remove the hook that called this so normal buffers are not eww-readable.
    (message "in make-eww-reabable")
    (unwind-protect
        ;;(eww-readable)
        (remove-hook 'eww-after-render-hook #'make-eww-readable)))

  (defun eww-readable-url (url)
    ;; when the eww buffer has rendered call a hook function that implements eww-readable and then removes that hook.
    ;; primarily for 'dash-docs-browser-func
    (interactive "sURL:")
    (add-hook 'eww-after-render-hook #'make-eww-readable)
    (message "eww readable browsing: %s, hook is: %s " url eww-after-render-hook)
    (eww url))
  :bind
  ("C-c M-o" . 'browse-url)
  ("C-c M-O" . 'eww-readable-url)
  (:map eww-mode-map
        ("O" . www-open-current-page-external)
        ("o" . www-open-link-external)))

(defun rgr/load-chats(switch)
  (require  'rgr-chat))
(add-to-list 'command-switch-alist '("chat" . rgr/load-chats))

(defun eshell/chat-client
    (&rest
     args)
  "chat with emacs.."
  (interactive)
  (save-window-excursion (call-process "oneemacs-chat" nil 0)))

(global-set-key (kbd "C-c i") 'eshell/chat-client)

(use-package
  gnus

  :disabled t
  :config

  (setq smtpmail-smtp-server "smtp.gmail.com" smtpmail-smtp-service 587 gnus-ignored-newsgroups
        "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  (require 'bbdb)
  (require 'bbdb-vcard)
  (bbdb-initialize 'gnus 'message)
  (add-hook 'message-setup-hook 'bbdb-mail-aliases)
  (defun rgr/gnus-themes()
    (load-theme-buffer-local 'alect-light (current-buffer)))
  (require 'gnus-desktop-notify)
  (gnus-desktop-notify-mode)
  (gnus-demon-add-scanmail)

  (define-key gnus-summary-mode-map (kbd "M-o") 'ace-link-gnus)
  (define-key gnus-article-mode-map (kbd "M-o") 'ace-link-gnus)
  (setq bbdb-use-pop-up nil)
  :bind	  ("C-c m".  'gnus))

(use-package mu4e

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
  :bind	  (("C-c m".  'mu4e)
           (:map mu4e-main-mode-map
                 ("m" . mu4e-compose-new))
           (:map mu4e-main-mode-map
                 ("g" . rgr/mu4e-refresh))
           (:map mu4e-headers-mode-map
                 ("C-c u" . mu4e-headers-mark-all-unread-read))))
;;(
;;:map mu4e-view-mode-map
;;   ("V" . '(lambda()(message "%s" (mu4e-message-at-point))))))) ;; mu4e-action-view-in-browser))))

(use-package camcorder
  :custom
  (camcorder-output-directory  "~/tmp"))

(use-package keycast
  )

(use-package
  pomidor
  :bind (("S-<f7>" . pomidor))
  :custom (pomidor-sound-tick nil)
  (pomidor-sound-tack nil)
  (pomidor-seconds (* 25 60))
  (pomidor-break-seconds (* 5 60))
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer)))))

(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))

(global-set-key (kbd "S-<f2>") 'linum-mode)
(add-hook 'prog-mode-hook (lambda() (linum-mode t)))

(use-package
  smartparens
  :disabled t
  :commands (smartparens-mode)
  :config (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  (sp-local-tag '(mhtml-mode html-mode) "b" "<span class=\"bold\">" "</span>")
  (smartparens-global-mode t))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(use-package projectile
  :custom
  (projectile-completion-system 'default)
  :config
  (projectile-mode +1)
  :bind ("<f2>" . 'projectile-dired)
  ("<f12>" . projectile-run-eshell)
  ("M-<RET>" . projectile-run-eshell)
  (:map projectile-mode-map ( "C-c p" . projectile-command-map)))

;; try to work with next-error for bash's "set -x" output
(use-package compile
  :config
  (add-to-list 'compilation-error-regexp-alist
               'bash-set-x)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pascal
                 "\\(.+?\\)\\(\\([0-9]+\\),\\([0-9]+\\)\\).*" 1 2 3)))

(use-package json-mode)

(use-package
  yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\.yaml\\'" . yaml-mode))
  )

(use-package
  flycheck
  :config (use-package
            flycheck-pos-tip)
  (flycheck-pos-tip-mode)
  (global-flycheck-mode))

;; (use-package
;;   diff-hl
;;   :init (global-diff-hl-mode 1))

(use-package
  magit
  :config
  (add-hook 'magit-post-commit-hook 'magit-mode-bury-buffer)
  :bind* ("C-x g" . magit-status))

(use-package ediff+
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :config
  (when (fboundp 'winnder-undo)
    (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
  :bind (:map prog-mode-map ("C-c C-d" . 'ediff-files)))

(use-package forge
  :after magit)

(use-package orgit
  :after magit
  :demand t)

(use-package git-gutter
  :demand t
  :config
  (global-git-gutter-mode +1)
  :bind
  ("C-x v ="  . git-gutter:popup-hunk))

(use-package rjsx-mode
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

(use-package prettier-js
  :custom
  (prettier-js-args '(
                      "--trailing-comma" "all"
                      "--bracket-spacing" "false"
                      "--print-width" "80"
                      )))

(defun rgr/js-mode-hook ()
  (setq-local js-indent-level 2)
  (prettier-js-mode t)
  (setq-local dash-docs-docsets '("React" "JavaScript" "jQuery")))

(add-hook 'js-mode-hook 'rgr/js-mode-hook)



;;(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))

(use-package rjsx-mode
  :disabled t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
  )

(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-mode))
  (defun rgr/ts-mode-hook ()
    (setq-local dash-docs-docsets '("React" "JavaScript")))
  (add-hook 'typescript-mode-hook 'rgr/ts-mode-hook))

(use-package tide
  :disabled t
  :config
  (defun setup-tide-mode ()
    "Setup function for tide."
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  (setq company-tooltip-align-annotations t)
  :init
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  )

(require 'rgr/lsp "rgr-lsp" 'NOERROR)

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

(straight-use-package 'platformio-mode)

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package  auto-virtualenv
  :init
  (add-hook 'python-mode-hook #'auto-virtualenv-set-virtualenv))

(defun rgr/asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; (local-unset-key "<return>") ; doesn't work. "RET" in a terminal.  http://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode
  (electric-indent-local-mode)  ; toggle off
                                        ;  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  ;; (setq tab-always-indent (default-value 'tab-always-indent))

  (defun asm-calculate-indentation ()
    (or
     ;; Flush labels to the left margin.
                                        ;   (and (looking-at "\\(\\.\\|\\sw\\|\\s_\\)+:") 0)
     (and (looking-at "[.@_[:word:]]+:") 0)
     ;; Same thing for `;;;' comments.
     (and (looking-at "\\s<\\s<\\s<") 0)
     ;; %if nasm macro stuff goes to the left margin
     (and (looking-at "%") 0)
     (and (looking-at "c?global\\|section\\|default\\|align\\|INIT_..X") 0)
     ;; Simple `;' comments go to the comment-column
                                        ;(and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
     ;; The rest goes at column 4
     (or 4)))
  )

(add-hook 'asm-mode-hook #'rgr/asm-mode-hook)

(straight-use-package 'clang-format)
(setq clang-format-style-option "llvm")
(fset 'c-indent-region 'clang-format-region)
(fset 'c-indent-buffer 'clang-format-buffer)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(use-package
  ccls
  :hook ((c++-mode c-mode objc-mode) . (lambda ()
                                         (require 'ccls)
                                         (ccls-use-default-rainbow-sem-highlight)
                                         ;;                 (ccls-code-lens-mode)
                                         )))
(defun rgr/c-setup ()
  "Set up my C mode."
  (platformio-mode))
;;        (define-key c-mode-map (kbd "C-c s") #'selectSerialPort
(add-hook 'c-mode-hook #'rgr/c-setup)
(add-hook 'objc-mode-hook #'rgr/c-setup)

(use-package x86-lookup
  :custom
  ( x86-lookup-pdf  (expand-file-name "pdf/intel-x86.pdf" user-emacs-directory))
  )

(use-package logview
  :demand t
  :init
  (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
  (add-to-list 'auto-mode-alist '("log\\'" . logview-mode)))

(use-package strace-mode)

(defun rgr/c++-mode-hook ()
  (setq-local dash-docs-docsets '("C++")))
(add-hook 'c++-mode-hook 'rgr/c++-mode-hook)

(use-package csharp-mode)
(use-package omnisharp)

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

(defgroup rgr/symfony nil
  "Symfony Development"
  :group 'rgr)

(defcustom symfony-server-command "~/.symfony/bin/symfony server:start"
  "Start the symfony web server"
  :type 'string
  :group 'rgr/symfony)

(use-package php-mode
  :config
  (add-to-list 'display-buffer-alist
               (cons "\\*Symfony Web Server\\*.*" (cons #'display-buffer-no-window nil)))
  (defun start-symfony-web-server()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (if (and default-directory (file-exists-p "bin/console") (eq (length (shell-command-to-string "pgrep symfony")) 0) (yes-or-no-p "Start web server?"))
          (async-shell-command symfony-server-command "*Symfony Web Server*"))))
  (defun php-mode-webserver-hook ()
    (interactive)
    (start-symfony-web-server)
    ))
;;:hook (php-mode . php-mode-webserver-hook))

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

(use-package elf-mode
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(?:a\\|so\\)\\'" . elf-mode)))

(load-el-gpg (no-littering-expand-etc-file-name "late-load"))
