(setq custom-file  (expand-file-name  "custom.el" user-emacs-directory)) ;;
(load custom-file 'noerror)

(debug-init)

;; look for a debug init file and load, trigger the debugger
(debug-init "debug-init-straight.el")

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
  ;;:disabled
  :custom
  (bmkp-last-as-first-bookmark-file (no-littering-expand-var-file-name "bmkp/current-bookmark.el.gpg"))
  :demand)

(require 'rgr/org "rgr-org" 'NOERROR)

(use-package lazy-lang-learn
  :straight (lazy-lang-learn :local-repo "~/development/projects/emacs/lazy-lang-learn" :type git :host github :repo "rileyrg/lazy-lang-learn" )
  :bind
  ("C-c L" . lazy-lang-learn-mode)
  ("<f12>" . lazy-lang-learn-translate)
  ("S-<f12>" . lazy-lang-learn-translate-from-history))

(require  'rgr/general-config "rgr-general-config" 'NOERROR)

(use-package emacs
  :init
  (setq rgr/complete-line-function 'rgr/newline-below)
  :config
  (defun rgr/complete-c-line()
    (interactive)
    (end-of-line)
    (delete-trailing-whitespace)
    (unless (eql ?\; (char-before (point-at-eol)))
      (progn (insert ";")))
    (if current-prefix-arg
        (newline-and-indent)
      (progn
        (next-line)
        (beginning-of-visual-line))))

  (defun rgr/insert-previous-line()
    (interactive)
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    (insert (string-trim (current-kill 0))))

  (defun rgr/newline-below()
    (interactive)
    (end-of-line)
    (newline-and-indent))
  :bind
  ("<C-return>" . (lambda()(interactive)(funcall rgr/complete-line-function))))

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

(use-package multiple-cursors
 :bind (("C-<mouse-1>" . mc/add-cursor-on-click)
       ("C->" . mc/mark-next-like-this)
       ("C-<" . mc/mark-previous-like-this)
       ("C-c C->" . mc/mark-all-like-this)
       ("C-c C-SPC" . mc/edit-lines)
       ))

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

(use-package
  ripgrep)

(require 'rgr/reference "rgr-reference" 'NOERROR)

(use-package
  eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (defun rgr/projectile-term()
    (interactive)
    (if (string-equal major-mode "eat-mode")
        (previous-buffer)
      (let ((default-directory (projectile-project-root)))
        (eat))))

  :bind
  ("M-g v" . #'rgr/projectile-term))

(defun rgr/toggle-buffer(n)
  "jump to or from buffer named n else default to *Messages*"
  (interactive "bbuffer:")
  (let ((n (or n
               "*Messages*")))
    (switch-to-buffer (if (string= (buffer-name) n)
                          (other-buffer) n))))

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
  :disabled
  :ensure t
  :init
  (use-package posframe)
  ;;(setq popper-display-function 'rgr/popper-display-posframe)
  (setq popper-reference-buffers
        '(
          "\\*Messages\\*"
          ;;magit-mode
          ;;      help-mode
          helpful-mode
          inferior-python-mode
          dictionary-mode
          compilation-mode))
  (popper-mode +1)
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)))

(use-package ace-window
  :init
  (defalias 'other-window 'ace-window)
  :bind*
  ("M-o" . 'other-window)
  ("C-x o" . ace-window)
  ("M-S o" . ace-delete-window))

(use-package ace-link
  :demand
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

(use-package
  golden-ratio
  :init
  (golden-ratio-mode 1))

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

(require 'rgr/email "rgr-email" 'NOERROR)

(require 'rgr/programming "rgr-programming" 'NOERROR)

(require 'rgr/themes "rgr-themes" 'NOERROR)

(load-el-gpg (no-littering-expand-etc-file-name "late-load"))
(switch-to-buffer "*scratch*")
