(straight-use-package 'diminish)

;; don't complain if custom-paths.el doesn't exist
(require 'custom-paths nil t)

;; start emacs-server if not running
(unless(daemonp)
  (add-hook 'after-init-hook (lambda ()
                               (require 'server)
                               (unless (server-running-p)
                                 (message "Starting EmacsServer from init as not already running.")
                                 (server-start)))))

(defun startHook()
  ;;(desktop-save-mode 1)
  ;;(desktop-read)

  (global-set-key (kbd "S-<f1>") 'describe-face)) ;

(add-hook 'emacs-startup-hook 'startHook)

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

(use-package bongo :ensure t)

(defun rgr/toggle-buffer
    (&optional
     n)
  "jump to or from buffer named n else default to *Messages*"
  (interactive)
  (let ((n (or n
               "*Messages*")))
    (switch-to-buffer (if (string= (buffer-name) n)
                          (other-buffer) n))))

(defun generate-setq-form-function (variable value)
  `(setq ,variable ',(sort (delete-dups (copy-tree value)) #'(lambda (x y)
                                                               (string< (symbol-name x)
                                                                        (symbol-name y))))))

;; (setq x 3)
;; (generate-setq-form-function x '(b e c f a))

(use-package help-fns+
  :disabled t
  )

(use-package helpful

  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

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
  (global-set-key (kbd "C-h C") #'helpful-command)
  )

;; (set-language-environment 'utf-8)
;; (setq default-process-coding-system '(utf-8 . utf-8)) ;; needed this for calling call-process-shell-command
(require 'iso-transl) ;; supposed to cure deadkeys when my external kbd is plugged into my thinkpad T460.  It doesnt.

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(tooltip-mode 1)
(display-time-mode 1)

(global-visual-line-mode 1)

(delete-selection-mode 1)

(save-place-mode 1)
(savehist-mode 1)

(global-set-key (kbd "S-<f10>") #'menu-bar-open)
(global-set-key (kbd "<f10>") #'imenu)


(setq frame-title-format (if (member "-chat" command-line-args)  "Chat: %b" "Emacs: %b")) ;; used to select the window again (frame-list) (selected-frame)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(use-package boxquote
  :bind*
  ("C-S-r" . boxquote-region))

(use-package
  browse-url-dwim)

(use-package
  all-the-icons)

(use-package
  webpaste
  :bind ("C-c y" . webpaste-paste-region)
  ("C-c Y" . webpaste-paste-buffer))

;; brings visual feedback to some operations by highlighting portions relating to the operations.
(use-package
  volatile-highlights
  :init (volatile-highlights-mode 1))
;; display dir name when core name clashes
(require 'uniquify)

(global-set-key (kbd "C-c r") 'query-replace-regexp)

(use-package xclip
  :demand t
  :config
  (xclip-mode))

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(require 'pointhistory)

(use-package
  shell-switcher
  :config (setq shell-switcher-mode t)
  ;;(add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)
  :bind ("<f12>" . projectile-run-eshell)
  ("M-<f12>" . shell-switcher-switch-buffer)
  ("C-<f12>" . shell-switcher-new-shell))

(defun eshell/emacs-clean (&rest args)
  "run a clean emacs"
  (interactive)
  (save-window-excursion
    (shell-command "emacs -Q -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el &")))

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
  (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
  (use-package pcomplete-extension
    :config
    (defconst pcmpl-git-commands
      '("add" "bisect" "branch" "checkout" "clone"
        "commit" "diff" "fetch" "grep"
        "init" "log" "merge" "mv" "pull" "push" "rebase"
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
  :bind (:map eshell-mode-map
              ;; ([remap eshell-previous-matching-input-from-input] . previous-line)
              ;; ([remap eshell-next-matching-input-from-input] . next-line)
              ;;([remap eshell-list-history] . helm-eshell-history)
              ("C-r" . helm-eshell-history)))

(use-package docker
  :ensure t
  :after projectile
  :bind (:map projectile-mode-map ("C-c k" . docker)))

(use-package
  helm
  :custom
  (helm-buffer-max-length 64)
  (helm-candidate-numer 50 t)
  (helm-ff-auto-update-initial-value t)
  ;; (helm-grep-default-command "grep --color=always -a -i -d skip %e -n%cH -e %p %f")
  ;; (helm-grep-default-recurse-command "grep --color=always -i -a -d recurse %e -n%cH -e %p %f")
  ;; (helm-grep-git-grep-command "git --no-pager grep -n%cH --full-name -e %p -- %f")
  (helm-split-window-inside-p t)
  (helm-swoop-move-to-line-cycle t)
  (helm-ff-search-library-in-sexp t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-echo-input-in-header-line t)
  (helm-truncate-lines t)
  (helm-turn-on-recentf t) ;; doesnt work
  ;;      (helm-show-completion-display-function #'helm-show-completion-default-display-function) ; stop using popup frame
  (helm-show-completion-display-function nil) ; stop using popup frame
  :config
  (helm-mode 1)
  ;;(helm-autoresize-mode 1)
  (helm-popup-tip-mode 1)
  (use-package
    helm-ag)
  (use-package
    helm-rg)
  (use-package
    helm-git-grep)
  (use-package
    google-translate)
  (use-package
    helm-swoop
    :config
    (setq helm-swoop-pre-input-function (lambda () "")))
  (use-package
    helm-chrome)
  (use-package
    helm-dictionary)
  (require 'helm-config)
  (global-unset-key (kbd "C-x c"))
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  :bind ("C-c h" . helm-command-prefix)
  ("M-x" . helm-M-x)
  ("C-x b" . helm-mini)
  ("C-s" . helm-swoop)
  ("C-x C-f". helm-find-files)
  ("M-p". helm-show-kill-ring)
  ("C-h SPC" . helm-all-mark-rings)
  ("C-h C-SPC" . helm-global-mark-ring)
  (:map helm-map
        ("C-z" . helm-execute-persistent-action))
  (:map helm-command-map
        (("@" . straight-use-package)
         ("d" . helm-dictionary)
         ("r" . helm-resume)
         ("e" . helm-info-elisp)
         ("g" . helm-google-suggest)
         ("B". helm-chrome-bookmarks)
         ("p". helm-top)
         ("t". google-translate-at-point)
         ("T". google-translate-query-translate)
         ("b". helm-bookmarks)
         ("u". browse-url-dwim)
         ("o" . helm-multi-swoop))))

(use-package
  helm-projectile
  :demand
  :custom
  (helm-ag-insert-at-point 'symbol)
  :config
  (use-package helm-ag)
  (defun my-projectile-grep(&optional deepsearch)
    (interactive "P")
    ;; (let((helm-rg--extra-args (if deepsearch "--no-ignore-vcs" nil)))
    ;;   (helm-projectile-rg)))
  (let((helm-ag--extra-options (if deepsearch "-U" nil)))
    (helm-projectile-ag helm-ag--extra-options)))
  (defun my-projectile-find-file(search)
    (interactive "P")
    (if search
        (projectile-find-file)
      (projectile-find-file-dwim)))
  (helm-projectile-on)
  (projectile-mode 1)
  :bind ("<f2>" . 'projectile-dired)
  ("S-<f3>" . 'helm-do-grep-ag)
  ("<f3>" . 'my-projectile-grep)
  ("<f4>" . 'my-projectile-find-file)
  ("<f5>" . 'helm-projectile-switch-to-buffer)
  (:map projectile-mode-map ("C-c p" . projectile-command-map))
  (:map projectile-command-map ("o"  . helm-multi-swoop-projectile))
  (:map projectile-command-map ("g"  . helm-git-grep)))

(winner-mode 1)
(recentf-mode 1)
(global-set-key (kbd "C-<f2>") 'rgr/toggle-buffer)
(global-set-key (kbd "C-h d") (lambda()(interactive)(apropos-documentation (symbol-or-region-at-point-as-string-or-prompt))))
(defun kill-next-window ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (if (not (one-window-p))(progn
                            (other-window 1)
                            (kill-this-buffer))
    (message "no next window to kill!")))
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-next-window)
(defun rgr/switch-to-buffer-list (buffer alist)
  (message "in rgr/switch-to-buffer-list")
  (select-window  (display-buffer-use-some-window buffer alist)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package auto-sudoedit
  :demand t
  :config
  (auto-sudoedit-mode 1))

(global-set-key (kbd "C-x C-b") 'ibuffer)

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
  :ensure t
  :config
  (dired-quick-sort-setup))

(use-package posframe)

(use-package popwin
  :init (popwin-mode 1))

(use-package transpose-frame
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
  :config
  (back-button-mode 1)
  :bind
  ("M-<left>" . previous-buffer)
  ("M-<right>" . next-buffer))

(use-package ace-window
  :bind*
  ("C-x o" . ace-window)
  ("C-x O" . ace-delete-window))

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

(use-package centaur-tabs
  :disabled
  :demand
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :custom
  (centaur-tabs-height 32)
  (centaur-tabs-style "alternate")
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-cycle-scope 'default)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (defun centaur-tabs-buffer-groups ()
      "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
      (list
        (cond
         ((or (string-equal "*" (substring (buffer-name) 0 1))
              (memq major-mode '(magit-process-mode
                                 magit-status-mode
                                 magit-diff-mode
                                 magit-log-mode
                                 magit-file-mode
                                 magit-blob-mode
                                 magit-blame-mode
                                 )))
          "Emacs")
         ((derived-mode-p 'prog-mode)
          "Editing")
         ((derived-mode-p 'dired-mode)
          "Dired")
         ((memq major-mode '(helpful-mode
                             help-mode))
          "Help")
         ((memq major-mode '(org-mode
                             org-agenda-clockreport-mode
                             org-src-mode
                             org-agenda-mode
                             org-beamer-mode
                             org-indent-mode
                             org-bullets-mode
                             org-cdlatex-mode
                             org-agenda-log-mode
                             diary-mode))
          "OrgMode")
         (t
          (centaur-tabs-get-group-name (current-buffer))))))

  :hook
  (dired-mode . centaur-tabs-local-mode)
  :bind
  ("C-x B" . centaur-tabs-switch-group)
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package
  darkroom
  :config (define-globalized-minor-mode my-global-darkroom-mode darkroom-tentative-mode
            (lambda ()
              (darkroom-tentative-mode 1)))
  :bind ("<f7>" . 'darkroom-tentative-mode))

(use-package outshine :disabled t)

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

(use-package flyspell
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word)
    )
  :bind (("C-<f8>" . flyspell-mode)
        ("C-S-<f8>" . flyspell-buffer)
        ("<f8>" . flyspell-check-next-highlighted-word)
        ("S-<f8>" . flyspell-check-previous-highlighted-word)
        ))

(use-package
  yasnippet
  :init (yas-global-mode 1)
  :config

  (use-package
    php-auto-yasnippets)
  (use-package
    yasnippet-snippets)
  (use-package
    yasnippet-classic-snippets))

(use-package
  company
  :demand t
  :config
  ;; (setq company-backends
  ;;       '((company-capf :with company-dabbrev-code :separate)
  ;;         (company-files :with company-dabbrev-code)
  ;;         (company-nxml company-dabbrev-code company-keywords :with company-yasnippet)
  ;;         (company-oddmuse :with company-yasnippet)
  ;;         (company-dabbrev :with company-yasnippet)))
  (use-package
    company-box
    :disabled
    :hook (company-mode . company-box-mode))
  (require 'company-ispell)
  (global-company-mode)
  :bind ("C-<tab>" . company-complete))

(use-package
  which-key
  :demand t
  :config (which-key-mode))

(defun check-expansion ()
  (save-excursion (if (looking-at "\\_>") t (backward-char 1)
                      (if (looking-at "\\.") t (backward-char 1)
                          (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (yas/create-php-snippet)))))
;;              (indent-for-tab-command)))))

(use-package ag)

(setq-default abbrev-mode t)

(use-package deft
  :config
  (setq deft-directory (expand-file-name "orgfiles" user-emacs-directory))
  (setq deft-recursive t))

(use-package
  google-this
  :config
  (google-this-mode 1)
  :bind ("C-c G" . google-this-search))

(defgroup rgr/lookup-reference nil
        "Define functions to be used for lookup"
        :group 'rgr)

      (defcustom mode-lookup-reference-functions-alist '(
                                                         (nil (goldendict-dwim goldendict-dwim))
                                                         (c++-mode  (my-lsp-ui-doc-glance my-dash))
                                                         (gdscript-mode  (my-lsp-ui-doc-glance my-dash))
;;                                                         (gdscript-mode  (my-gdscript-docs-browse-symbol-at-point my-dash))
                                                         (php-mode  (my-lsp-ui-doc-glance my-dash))
                                                         (web-mode  (my-lsp-ui-doc-glance my-devdocs))
                                                         (org-mode (elisp-lookup-reference-dwim))
                                                         (js2-mode (my-dash my-devdocs))
                                                         (js-mode (my-dash my-devdocs))
                                                         (rjsx-mode (my-dash my-devdocs))
                                                         (typescript-mode (my-dash my-devdocs))
                                                         (lisp-interaction-mode (elisp-lookup-reference-dwim my-dash))
                                                         (emacs-lisp-mode (elisp-lookup-reference-dwim my-dash)))
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

      (defcustom  lookup-reference-functions '(my-describe-symbol goldendict-dwim my-linguee-lookup my-dictionary-search my-jquery-lookup google-this-search)
        "list of functions to be called via C-n prefix call to lookup-reference-dwim"
        :type 'hook
        :group 'rgr/lookup-reference)

      (defun sys-browser-lookup(w template)
        (interactive)
        (browse-url-xdg-open (replace-regexp-in-string "%S%" (if w w (symbol-or-region-at-point-as-string-or-prompt)) template)))

      (defun symbol-or-region-at-point-as-string-or-prompt()
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

      (defun my-describe-symbol(w)
        (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
        (let ((s (if (symbolp w) w (intern-soft w))))
          (if s (describe-symbol s)
            (message "No such symbol: %s" w))))

      (defun my-linguee-lookup(w)
        (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
        (sys-browser-lookup w linguee-url-template))

      (defun my-php-api-lookup(w)
        (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
        (let ((dash-docs-docsets '("PHP")))
          (helm-dash w)))
      ;; (sys-browser-lookup w php-api-url-template))

      (defun my-jquery-lookup(&optional w)
        (interactive(cons (symbol-or-region-at-point-as-string-or-prompt) nil))
        (let (;;(zeal-at-point-docset "jQuery")
              (dash-docs-docsets '("jQuery")))
          (helm-dash w)))
      ;; (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
      ;; (sys-browser-lookup w jquery-url-template))

      (defun my-gdscript-docs-browse-symbol-at-point(&optional w)
        (gdscript-docs-browse-symbol-at-point))

      (defun lookup-reference-dwim(&optional secondary)
        "if we have a numeric prefix then index into lookup-reference functions"
        (interactive)
        (let((w (symbol-or-region-at-point-as-string-or-prompt))
             ;; PREFIX integer including 4... eg C-2 lookup-reference-dwim
             (idx (if (and  current-prefix-arg (not (listp current-prefix-arg)))
                      (- current-prefix-arg 1)
                    nil)))
          (if idx (let((f (nth idx lookup-reference-functions)))
                    (funcall (if f f (car lookup-reference-functions)) w))
            (let* ((funcs (get-mode-lookup-reference-functions))(p (car funcs))(s (cadr funcs)))
              (if (not secondary)
                  (unless (funcall p w)
                    (if s (funcall s w)))
                (if s (funcall s w)))))))

      (defun lookup-reference-dwim-secondary()
        (interactive)
        (lookup-reference-dwim t))

      (bind-key* "C-q" 'lookup-reference-dwim) ;; overrides major mode bindings
      (bind-key* "C-S-q" 'lookup-reference-dwim-secondary)

(use-package
  dictionary
  :commands (my-dictionary-search)
  :config
  (defun my-dictionary-search(&optional w)
    (interactive)
    (dictionary-search (if w w (symbol-or-region-at-point-as-string-or-prompt))))
  :bind ("<f6>" . my-dictionary-search) )

(defun display-elisp-lookup-reference-popup(sym)
  (interactive)
  (popup+-normal (if (fboundp sym)
                     (popup+-emacs-function sym)
                   (popup+-emacs-variable sym))))

(defun elisp-lookup-reference-dwim
    (&optional
     w)
  "Checks to see if the 'thing' is known to elisp and, if so, use internal docs else call out to goldendict"
  (interactive)
  (let ((sym (if (symbolp w) w (intern-soft w))))
    (if (and sym
             (or (fboundp sym)
                 (boundp sym)))
        (display-elisp-lookup-reference-popup sym) nil)))

(define-minor-mode my-contextual-help-mode
  "Show help for the elisp symbol at point in the current *Help* buffer.

Advises `eldoc-print-current-symbol-info'."
  :lighter " C-h"
  :global t
  (require 'help-mode) ;; for `help-xref-interned'
  (when (eq this-command 'my-contextual-help-mode)
    (message "Contextual help is %s" (if my-contextual-help-mode "on" "off")))
  (and my-contextual-help-mode
       (eldoc-mode 1)
       (if (fboundp 'eldoc-current-symbol)
           (eldoc-current-symbol)
         (elisp--current-symbol))
       (my-contextual-help :force)))

(defadvice eldoc-print-current-symbol-info (before my-contextual-help activate)
  "Triggers contextual elisp *Help*. Enabled by `my-contextual-help-mode'."
  (and my-contextual-help-mode
       (derived-mode-p 'emacs-lisp-mode)
       (my-contextual-help)))

(defvar-local my-contextual-help-last-symbol nil
  ;; Using a buffer-local variable for this means that we can't
  ;; trigger changes to the help buffer simply by switching windows,
  ;; which seems generally preferable to the alternative.
  "The last symbol processed by `my-contextual-help' in this buffer.")

(defun my-contextual-help (&optional force)
  "Describe function, variable, or face at point, if *Help* buffer is visible."
  (let ((help-visible-p (get-buffer-window (help-buffer))))
    (when (or help-visible-p force)
      (let ((sym (if (fboundp 'eldoc-current-symbol)
                     (eldoc-current-symbol)
                   (elisp--current-symbol))))
        ;; We ignore keyword symbols, as their help is redundant.
        ;; If something else changes the help buffer contents, ensure we
        ;; don't immediately revert back to the current symbol's help.
        (and (not (keywordp sym))
             (or (not (eq sym my-contextual-help-last-symbol))
                 (and force (not help-visible-p)))
             (setq my-contextual-help-last-symbol sym)
             sym
             (save-selected-window
               (help-xref-interned sym)))))))

(defun my-contextual-help-toggle ()
  "Intelligently enable or disable `my-contextual-help-mode'."
  (interactive)
  (if (get-buffer-window (help-buffer))
      (my-contextual-help-mode 'toggle)
    (my-contextual-help-mode 1)))

(my-contextual-help-mode 1)

(use-package
  goldendict
  :commands (goldendict-dwim)
  :config
  (defun goldendict-dwim
      (&optional
       w)
    "lookup word at region, thing at point or prompt for something, in goldendict. Use a prefix to force prompting."
    (interactive)
    (let ((w (if w w (symbol-or-region-at-point-as-string-or-prompt))))
      (call-process-shell-command (format  "goldendict \"%s\"" w ) nil 0)))
  :bind (("C-c g" . goldendict-dwim)))

(use-package devdocs
  :commands (my-devdocs)
  :config
  (defun my-devdocs (&optional w)
    (interactive)
    (devdocs-search)t)
  :bind* ("C-c v" . 'my-devdocs))

(use-package zeal-at-point
  :disabled t ;;way too buggy
  :commands (my-zeal)
  :config
  (defun my-zeal (&optional w)
    (interactive)
    (zeal-at-point)t)
  :bind* ("C-c z" . 'my-zeal))

(use-package
        helm-dash
        :demand t
        :custom
        (dash-docs-common-docsets '("Emacs Lisp" "Docker"))
        (dash-docs-docsets '())
        :config
           (setq helm-dash-browser-func 'eww-readable-url)
;;         (setq helm-dash-browser-func 'eww)
        (defun my-dash (w)
          (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
          (message "my-dash: %s" w)
          (message "docsets are: " dash-docs-docsets)
          (helm-dash w)t)
        :bind* ("C-c d" . 'my-dash))

(use-package
  treemacs
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
  (defun my-treemacs-select-window (close)
    (interactive "P")
    (if close (treemacs)
      (treemacs-select-window)))
  :bind ("M-0"   . my-treemacs-select-window)
  (:map treemacs-mode-map
        ("<right>" . treemacs-peek)))

(use-package
  alert
  :commands (alert)
  :custom (alert-default-style 'libnotify))

(defun www-open-current-page-external ()
  "Open the current URL in desktop browser."
  (interactive)
  (let((url (or
             (if(fboundp 'eww-currentl-url)
                 (eww-current-url)
               (if(fboundp 'w3m-currentl-url)
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
  :config
  (use-package helm-w3m)
  :bind
  ("C-c o" . 'browse-url)
  (:map w3m-mode-map
        ("O" . www-open-current-page-external)
        ("o" . www-open-link-external)))

(use-package
  eww
  ;:disabled t
  :demand t
  :commands (eww-readable-url)
  :config
  (use-package helm-eww)
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
  ("C-c o" . 'browse-url)
  ("C-c O" . 'eww-readable-url)
  (:map eww-mode-map
        ("O" . www-open-current-page-external)
        ("o" . www-open-link-external)))

(defun rgr/load-chats(switch)
  (require  'rgr-chat))
(add-to-list 'command-switch-alist '("chat" . rgr/load-chats))

(defun eshell/chat-client
    (&rest
     args)
  "run a clean emacs"
  (interactive)
  (save-window-excursion (call-process "oneemacs-chat" nil 0)))

(global-set-key (kbd "C-c i") 'eshell/chat-client)

(use-package org
  :straight org-plus-contrib
  :custom
  (org-directory "~/.emacs.d/orgfiles")
  (org-agenda-files (list org-directory (concat org-directory "/projects" )))
  (org-refile-targets `((,(directory-files-recursively org-directory "^[[:alnum:]].*\\.\\(org\\|gpg\\)\\'") :maxlevel . 9)))
  (org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (org-refile-use-outline-path t)                  ; Show full paths for refiling
  :config
  (require 'org-protocol)
  (require 'org-inlinetask)
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
  ("C-c t" . org-todo))

(use-package
  org-bullets
  :config (add-hook 'org-mode-hook (lambda ()
                                     (org-bullets-mode 1)
                                     ;; (org-num-mode 1)
                                     )))

(org-clock-persistence-insinuate)
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(require 'org-crypt)
(org-crypt-use-before-save-magic)

;; The following lines are always needed.  Choose your own keys.

(add-hook 'org-mode-hook 'rgr/elisp-helpers)


(defface org-canceled
  ;; originally copied from font-lock-type-face
  (org-compatible-face nil '((((class color)
                               (min-colors 16)
                               (background light))
                              (:foreground "darkgrey"
                                           :bold t))
                             (((class color)
                               (min-colors 16)
                               (background dark))
                              (:foreground "grey"
                                           :bold t))
                             (((class color)
                               (min-colors 8))
                              (:foreground "grey"))
                             (t
                              (:bold t))))
  "Face used for todo keywords that indicate DONE items."
  :group 'org-faces)

(defface org-wait
  ;; originally copied from font-lock-type-face
  (org-compatible-face nil '((((class color)
                               (min-colors 16)
                               (background light))
                              (:foreground "darkgrey"
                                           :bold t))
                             (((class color)
                               (min-colors 16)
                               (background dark))
                              (:foreground "grey"
                                           :bold t))
                             (((class color)
                               (min-colors 8))
                              (:foreground "grey"))
                             (t
                              (:bold t))))
  "Face used for todo keywords that indicate DONE items."
  :group 'org-faces)

(use-package org-journal
  :bind ("C-c J" . org-journal-new-entry)
  :bind ("C-c S" . org-journal-search))

(use-package org-roam
  :disabled t
  :after org
  :hook (org-mode . org-roam-mode)
  :straight (:host github :repo "jethrokuan/org-roam")
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph))

(defun insert-property(&optional p)
  "insert PROPERTY value of pdftools link"
  (unless p (setq p "TEST"))
  (message "property passed is: %s" p)
  (let ((pvalue
         (save-window-excursion
           (message "%s" (org-capture-get :original-buffer))
           (switch-to-buffer (org-capture-get :original-buffer))
           (org-entry-get (point) p)
           )))
    pva))

(use-package ox-gfm
  :defer 3
  :after org
  :config
  (defun config-export-to-markdown()
    (interactive)
    (if (and (eq major-mode 'org-mode) (file-exists-p (concat (file-name-sans-extension (buffer-file-name)) ".md")))
        (org-gfm-export-to-markdown)))

  (add-hook 'after-save-hook 'config-export-to-markdown))

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
  (defun my-gnus-themes()
    (load-theme-buffer-local 'alect-light (current-buffer)))
  (require 'gnus-desktop-notify)
  (gnus-desktop-notify-mode)
  (gnus-demon-add-scanmail)

  (define-key gnus-summary-mode-map (kbd "M-o") 'ace-link-gnus)
  (define-key gnus-article-mode-map (kbd "M-o") 'ace-link-gnus)
  (setq bbdb-use-pop-up nil)
  (use-package
    helm-bbdb)
  :bind	  ("C-c m".  'gnus))

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

(global-set-key (kbd "S-<f2>") 'linum-mode)
(add-hook 'prog-mode-hook (lambda() (linum-mode t)))

(use-package
  smartparens
  :commands (smartparens-mode)
  :config (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  (sp-local-tag '(mhtml-mode html-mode) "b" "<span class=\"bold\">" "</span>")
  (smartparens-global-mode t))

(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))

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
  elisp-format
  :bind
  ("C-c f" . elisp-format-region)
  (:map emacs-lisp-mode-map
        ("C-c f" . elisp-format-region)))

(use-package popup+
  :config
  (defun show-symbol-details ()
    (interactive)
    (popup-tip (format "intern-soft thing-at-point: %s, symbolp: %s, symbol-name:%s"
                       (setq-local sym (intern-soft (thing-at-point 'symbol)))
                       (symbolp sym)
                       (symbol-name sym))))
  :bind  ("C-M-S-s" . #'show-symbol-details))

(use-package
  edebug-x
  :demand t
  :init
  (global-set-key (kbd "C-S-<f9>") 'toggle-debug-on-error)
  ;;:custom
  ;;(edebug-trace nil)
  :config
  (require 'edebug)
  (defun instrumentForDebugging()
    (interactive)
    (eval-defun 0))
  (defun instrumentForDebugging()
    "use the universal prefix arg (C-u) to remove instrumentation"
    (interactive)
    (if current-prefix-arg (eval-defun nil) (eval-defun 0)))
  (defun rgr/elisp-helpers()
    ;; Meh, use C-u C-M-x
    ;; (define-key (current-local-map)
    ;;   (kbd "C-<f9>") #'instrumentForDebugging)
    )
  (add-hook 'emacs-lisp-mode-hook #'rgr/elisp-helpers)
  (add-hook 'lisp-interaction-mode-hook #'rgr/elisp-helpers)
  (add-hook 'help-mode-hook #'rgr/elisp-helpers)
  )

(use-package
  auto-compile
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

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

(use-package
  diff-hl
  :init (global-diff-hl-mode 1))
(use-package
  magit
  :demand t
  :config (add-hook 'magit-post-commit-hook 'magit-mode-bury-buffer)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind* ("C-x g" . magit-status)
  )

(use-package forge
  :after magit)

(use-package rjsx-mode
  :config
  (use-package npm-mode)
  (defun my-js2-mode-hook ()
    ;;         (setq-local zeal-at-point-docset '("JavaScript" "jQuery"))
    (npm-mode t)
    (setq-local dash-docs-docsets '("React" "JavaScript" "jQuery")))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook)
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

(defun my-js-mode-hook ()
  (setq-local js-indent-level 2)
  (prettier-js-mode t)
  (setq-local dash-docs-docsets '("React" "JavaScript" "jQuery")))

(add-hook 'js-mode-hook 'my-js-mode-hook)



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
  (defun my-ts-mode-hook ()
    (setq-local dash-docs-docsets '("React" "JavaScript")))
  (add-hook 'typescript-mode-hook 'my-ts-mode-hook))

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

;; if you want to change prefix for lsp-mode keybindings.
(use-package lsp-mode
  :custom
  (lsp-enable-file-watchers . nil)
  :config
  (use-package
    lsp-ui
    :custom
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-delay 3)
    (lsp-ui-doc-delay 1.7)
    :config
    (use-package lsp-treemacs
      :config
      (lsp-treemacs-sync-mode 1))
    (define-key lsp-ui-mode-map [(meta ?.)]  #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [(meta ??)] #'lsp-ui-peek-find-references)
    (defun toggle-lsp-ui-sideline ()
      (interactive)
      (if lsp-ui-sideline-mode (progn (message "Disable LSP UI Sideline Mode")
                                      (lsp-ui-sideline-mode -1))
        (progn (message "Enable LSP UI Sideline Mode")
               (lsp-ui-sideline-mode 1))))
    (defun toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode (progn (message "Disable LSP UI Doc Mode")
                                 (lsp-ui-doc-mode -1)
                                 (lsp-ui-doc--hide-frame))
        (progn (lsp-ui-doc-mode 1)
               (message "Enable LSP UI Doc mode"))))

    (defun my-lsp-ui-doc-glance (&optional w)
      "Trigger display hover information popup and hide it on next typing."
      (interactive)
      (lsp-describe-thing-at-point)
      ;; (message "lsp-ui-doc--displayed:%s" lsp-ui-doc--displayed)
      )
    (defun my-lsp-ui-mode-hook()
      (lsp-ui-sideline-mode -1)
      (lsp-ui-doc-mode -1)
      )

    (defun my-lsp-ui-imenu-view()
      (interactive)
      (lsp-ui-imenu--view)
      )


    :bind ((:map lsp-ui-mode-map
                ;; rather use helm's imenu< C-c h i>             ("C-c m"   . lsp-ui-imenu)
                ;;("C-q"   . lsp-ui-doc-show)
                ("M-S-<f9>" . myDapDebugOn)
                ("<f10>" . lsp-ui-imenu)
                ("C-c S"   . toggle-lsp-ui-sideline)
                ("C-c D"   . toggle-lsp-ui-doc))
           (:map lsp-ui-imenu-mode-map
                ("<RET>" . my-lsp-ui-imenu-view)
                ))

    :hook ((lsp-ui-mode . my-lsp-ui-mode-hook)))

  (use-package dap-mode
    :straight(dap-mode :type git :host github :repo "emacs-lsp/dap-mode"
                       :fork (:host github
                                    :repo "rileyrg/dap-mode"))
    :demand t
    :commands (myDapDebugOn)
    :config
    (setq dap-auto-configure-features '(sessions locals  tooltip))
    (require 'dap-gdb-lldb)
    ;;          (dap-gdb-lldb-setup)
    (add-hook 'dap-stopped-hook (lambda (arg)
                                  (call-interactively #'dap-hydra)))

    (require 'dap-chrome)

;; (dap-register-debug-template "Chrome Browse my-project"
;;   (list :type "chrome"
;;         :cwd nil
;;         :mode "url"
;;         :request "launch"
;;         :webRoot "/home/rgr/Dropbox/homefiles/development/projects/react/my-app/"
;;         :url "http://localhost:3000"
;;         :sourceMap "true"
;;         :name "Chrome Browse my-project"))


    (defun myDapDebugOn(p)
      "turn on dap modes"
      (interactive "P")
      ;;         (centreCursorLineOn)
      (if p
          (dap-debug nil)
        (dap-debug-last))
      ;;    (dap-hydra)
      )
    (defun my-lsp-mode-hook()
      (lsp-enable-which-key-integration)
      )

    :bind (:map dap-mode-map
                ("<f9>" . dap-hydra)
                ))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ((c-mode c++-mode js-mode php-mode gdscript-mode). lsp)
         ;; if you want which-key integration
         (lsp-mode . my-lsp-mode-hook)))

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

(straight-use-package 'clang-format)
(setq clang-format-style-option "llvm")
(fset 'c-indent-region 'clang-format-region)
(fset 'c-indent-buffer 'clang-format-buffer)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(use-package
  ccls
  :hook ((c++-mode c-mode objc-mode) . (lambda ()
                                         (ccls-use-default-rainbow-sem-highlight)
                                         ;;                 (ccls-code-lens-mode)
                                         )))
(defun my-c-setup ()
  "Set up my C mode."
  (platformio-mode))
;;        (define-key c-mode-map (kbd "C-c s") #'selectSerialPort
(add-hook 'c-mode-hook #'my-c-setup)
(add-hook 'objc-mode-hook #'my-c-setup)

(defun my-c++-mode-hook ()
  (setq-local dash-docs-docsets '("C++")))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

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

(use-package composer :disabled t)

(defun rgr/debug-php()
  (interactive)
  (dap-debug (cdr (car (cdr dap-debug-template-configurations)))))

(use-package
  php-mode
  :disabled t
  :commands (php-mode)
  :config
  (defun my-php-mode-hook()
    (setq-local dash-docs-docsets '("Symfony" "PHP")));test
  (add-hook 'php-mode-hook 'my-php-mode-hook)
  (use-package
    transient)
  (use-package
    phpstan)
  (use-package
    flycheck-phpstan
    :after phpstan
    )
  (define-transient-command
    php-transient-menu
    ()
    "Php"
    [["Class" ("cc" "Copy" phpactor-copy-class)
      ("cn" "New" phpactor-create-new-class)
      ("cr" "Move" phpactor-move-class)
      ("ci" "Inflect" phpactor-inflect-class)
      ("n"  "Namespace" phpactor-fix-namespace)]
     ["Properties" ("a"  "Accessor" phpactor-generate-accessors)
      ("pc" "Constructor" phpactor-complete-constructor)
      ("pm" "Add missing props" phpactor-complete-properties)
      ("r" "Rename var locally" phpactor-rename-variable-local)
      ("R" "Rename var in file" phpactor-rename-variable-file)]
     ["Extract" ("ec" "constant" phpactor-extract-constant)
      ("ee" "expression" phpactor-extract-expression)
      ("em"  "method" phpactor-extract-method)]
     ["Methods" ("i" "Implement Contracts" phpactor-implement-contracts)
      ("m"  "Generate method" phpactor-generate-method)]
     ["Navigate" ("x" "List refs" phpactor-list-references)
      ("X" "Replace refs" phpactor-replace-references)
      ("."  "Goto def" phpactor-goto-definition)]
     ["Phpactor" ("s" "Status" phpactor-status)
      ("u" "Install" phpactor-install-or-update)
      ("q" "Quit" transient-quit-all)]])
  (use-package
    phpactor
    :config (use-package
              company-phpactor))
  :bind (:map php-mode-map
              ( "C-c C-y" .  yas/create-php-snippet)
              ( "C-S-<return>" . c-complete-line)
              ( "C-<return>" . c-newline-below)
              ( "C-S-y" . c-insert-previous-line)
              ( "M-<return>" . php-transient-menu)))

(use-package
  php-scratch
  :bind (:map php-mode-map
              ("C-x s" . php-scratch)))

(use-package
  web-mode
  ;;:disabled t
  :demand t
  :config
  (defun my-web-mode-hook()
    (setq-local dash-docs-docsets '("Twig" "CSS" "HTML"))
    )
  (add-hook 'web-mode-hook 'my-web-mode-hook)
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

(use-package ediff+
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :config
  (when (fboundp 'winnder-undo)
    (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
  :bind (:map prog-mode-map ("C-c C-d" . 'ediff-files)))

(defun c-complete-line()
  (interactive)
  (end-of-line)
  (unless (eql ?\; (char-after (- (point-at-eol) 1)))
    (progn (insert ";")))
  (newline-and-indent))
(defun c-insert-previous-line()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  (insert (string-trim (current-kill 0))))
(defun c-newline-below()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(load (expand-file-name "flowers.el.gpg" user-emacs-directory ))
(eval-after-load "gnus" '(progn
                           (setq circe-default-nick (nth 0 (auth-source-user-and-password "circe")))
                           (setq circe-default-user (nth 1 (auth-source-user-and-password "circe")))
                           (setq twit-user (nth 0 (auth-source-user-and-password "twitter")))
                           (setq twit-pass (nth 1 (auth-source-user-and-password "twitter")))
                           (setq twittering-username (nth 0 (auth-source-user-and-password
                                                             "twitter")))
                           (setq twittering-password (nth 1 (auth-source-user-and-password
                                                             "twitter")))
                           (setq twitter-username (nth 0 (auth-source-user-and-password "twitter")))
                           (setq twitter-password (nth 1(auth-source-user-and-password "twitter")))
                           (setq emms-player-mpd-server-password (nth 1
                                                                      (auth-source-user-and-password
                                                                       "emmsmpd"))          )
                           (setq org-mobile-encryption-password (nth 1(auth-source-user-and-password
                                                                       "org-mobile")))))

(defun load-host-customisation()
  "allow the requiring of a custom-$HOSTNAME library"
  (message (concat "Attempting to require host specific settings: custom-" (system-name)))
  (let ((init-host-feature (intern (concat "custom-" (system-name)))))
    (require init-host-feature nil 'noerror)))

(use-package
  helm-themes
  :config (use-package
            panda-theme)
  (use-package
    dracula-theme)

  (use-package modus-operandi-theme
    :ensure t)

  (use-package modus-vivendi-theme
    :ensure t)

  (load-theme (if (daemonp) 'modus-vivendi 'panda)  t)
  )
