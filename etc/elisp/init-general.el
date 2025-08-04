(emacs-alert " ... is starting up..." )

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
  :config
  (load-theme 'modus-operandi :no-confirm))

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

(use-package project
  :ensure t
  :after (org)
  :custom
  (project-vc-extra-root-markers '(".project"))
  :config
  (define-key project-prefix-map "v" '("vterm" .  multi-vterm-project))
  (define-key project-prefix-map "u" '("project url" .  rgr/project-url)))

(setq load-path (cons (expand-file-name "project-org-todo-capture" rgr/emacs-project-dir ) load-path))
(use-package project-org-todo-capture
  :after org
  :ensure nil
  :init
  (require 'org-capture)
  :bind (:map project-prefix-map
              (("t" . project-org-todo-capture)
              ("T" . project-org-todo-capture-file-open))))

(defun rgr/toggle-buffer(n)
  "jump to or from buffer named n else default to *Messages*"
  (interactive "bbuffer:")
  (let ((n (or n
               "*Messages*")))
    (switch-to-buffer (if (string= (buffer-name) n)
                          (other-buffer) n))))

(defun rgr/elisp-write-var (f v)
  (with-temp-file f
    (prin1 v (current-buffer))))

(defun rgr/elisp-read-var (f)
  (with-temp-buffer
    (insert-file-contents f)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

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

(setq load-path (cons (expand-file-name "lazy-lang-learn" rgr/emacs-project-dir ) load-path))
(use-package lazy-lang-learn
  :ensure  nil
  :bind
  ("C-c L" . lazy-lang-learn-mode)
  ("<f12>" . lazy-lang-learn-translate)
  ("S-<f12>" . lazy-lang-learn-translate-from-history))

(use-package sudo-edit)

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

(use-package consult-omni
  :disabled t ;; https://github.com/armindarvish/consult-omni/issues/47
      :ensure  (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el"))
      :after consult)

(use-package stillness-mode
  :ensure '( :host github :repo "neeasade/stillness-mode.el" :branch "main")
  :config
  (stillness-mode 1))

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

(use-package  which-key
  :ensure nil
  :demand t
  :config (which-key-mode))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  :init
  (yas-global-mode)
  )

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

;; Enable vertico
(use-package vertico
  ;;:disabled
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode))

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

(use-package
  lilypond
  :disabled)

(use-package gptel
  :bind
  ("C-c q" . #'gptel-send))

(use-package ellama
  :custom
  (ellama-sessions-directory (expand-file-name  "var/ellama-sessions/" user-emacs-directory))
  :init
  (setopt ellama-language "German")
  (require 'llm-ollama))

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

(use-package go-translate
  :disabled
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

(defun rgr/open-buffer-file-in-external-browser()
  (interactive)
  (eww-browse-with-external-browser buffer-file-name)
  (quit-window))

(defcustom rgr/browser-doc-url "https://www.google.com/search?q=%s" "format url variable used for function `rgr/browser-doc-search'")
(defun rgr/browser-doc-search(&optional sym)
  "call function `browse-url' with a url variable `rgr/browser-doc-url' formatted with variable `sym'"
  (interactive
   (list
    (let((sym (replace-regexp-in-string  "^\\." "" (kill-dwim) )))
      (read-string (format "search(%s):" sym)
                   nil nil sym))))
  (browse-url (format rgr/browser-doc-url sym)))

(use-package define-word 
  :bind
  ("C-<f6>" . define-word-at-point))

(use-package
  dictionary
  :ensure nil
  :custom
  ;;(dictionary-server "dict.org")
  (dictionary-server "localhost")
  :bind
  ("<f6>" . dictionary-lookup-definition))

(use-package mw-thesaurus
  :bind
  ("S-<f7>" . mw-thesaurus-lookup-dwim))

(use-package powerthesaurus
  :bind
  ("S-<f6>" . powerthesaurus-lookup-word-dwim))

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

(use-package devdocs-browser
  ;;:disabled t
  :custom
  (devdocs-data-dir (expand-file-name  "var/devdocs-browser"  user-emacs-directory))
  (devdocs-browser-cache-directory (expand-file-name  "var/devdocs-browser/cache"  user-emacs-directory))
  (devdocs-browser-data-directory (expand-file-name  "var/devdocs-browser/data" user-emacs-directory))
  :hook
  (c-ts-mode . (lambda()(setq-local devdocs-browser-active-docs '("c"))))
  (c++-ts-mode . (lambda()(setq-local devdocs-browser-active-docs '("cpp" "qt")))))

(use-package pdf-tools
  :demand t
  :config
  (pdf-loader-install)
  :bind
  (:map pdf-view-mode-map
        ("&" . rgr/open-buffer-file-in-external-browser)))

(use-package nov
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; (use-package nov-xwidget
;;   :after nov
;;   :config
;;   (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

(use-package impatient-showdown
  :disabled
  :hook (markdown-mode . impatient-showdown-mode))

(use-package vterm
  :ensure t)
(use-package  multi-vterm
  :ensure t
  :commands (multi-vterm-project)
  :bind
  ("M-g t" . multi-vterm-project))

(use-package evil-matchit
  :bind
  ("M-(" . evilmi-jump-items-native))

(use-package consult-xref-stack :ensure (:host github :repo "brett-lempereur/consult-xref-stack")
:bind
(("M-," . consult-xref-stack-backward)))

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

(use-package parrot
  :config
  (defun my/parrot-animate-when-compile-success (buffer result)
    (if (string-match "^finished" result)
        (parrot-start-animation)))    (parrot-mode)
  (add-to-list 'compilation-finish-functions 'my/parrot-animate-when-compile-success))

(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "<f9>")
  '(lambda () (interactive)
      (condition-case nil (next-error)
         (error (next-error 1 t)))))

(use-package indent-bars
  :disabled
  :ensure t
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook
  (prog-mode . indent-bars-mode))

(use-package json-mode)
(use-package jsonrpc)

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

(use-package hideshow
  :ensure nil
  :hook
  (emacs-lisp-mode . hs-minor-mode)
  :bind
  (:map hs-minor-mode-map
        (("C-h t" . hs-toggle-hiding))))

(use-package duplicate-thing
  :demand t
  :bind   ("C-S-d" . 'duplicate-thing))

(use-package breadcrumb)

(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))

(global-set-key (kbd "S-<f2>") 'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda() (display-line-numbers-mode t)))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :disabled
  :ensure t
  :config
  (apheleia-global-mode +1))

;; try to work with next-error for bash's "set -x" output
(use-package compile
  :ensure nil
  :config
  (add-to-list 'compilation-error-regexp-alist
               'bash-set-x)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pascal
                 "\\(.+?\\)\\(\\([0-9]+\\),\\([0-9]+\\)\\).*" 1 2 3)))

(use-package php-mode)

(use-package yaml-mode)

(use-package json-reformat)
(use-package hydra)

(use-package transient)
(use-package
  magit
  :after transient
  :bind
  ("C-x g" . magit-status))

(use-package diff-hl
  :demand t
  :hook
  (elpaca-after-init .  global-diff-hl-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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

(use-package typescript-ts-mode
  :ensure nil
  :demand t
  :init
  (defun rgr/typescript-ts-mode-hook ()
    )
  :hook
  (typescript-ts-mode .  rgr/javascript-typescript-common-mode-hook)
  (typescript-ts-mode .  rgr/typescript-ts-mode-hook))

(use-package kotlin-ts-mode
  :demand t
  :init
  (defun rgr/kotlin-ts-mode-hook ()
    (eglot-ensure)
    )
  (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-ts-mode))
  :hook
  (kotlin-ts-mode .  rgr/kotlin-ts-mode-hook))

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

(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; I'm typically confused when it comes to haskell. Note that the interactive stuff I cribbed doesnt work.
(use-package haskell-mode
  :disabled t
  :config
  (eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
  (eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package lldb-voltron :disabled t
  :straight (lldb-voltron :local-repo "~/development/emacs/emacs-lldb-voltron" :type git :host github :repo "rileyrg/emacs-lldb-voltron" )
  ;;:config
  ;; (breadcrumb-mode t)
  )

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

(setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt\\'" . cmake-mode))
         '(("\\.cmake\\'" . cmake-mode))
         auto-mode-alist))

(use-package logview
  :demand t
  :init
  (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
  (add-to-list 'auto-mode-alist '("log\\'" . logview-mode)))

(use-package strace-mode)

(use-package elf-mode
  :demand t
  :config
  (elf-setup-default))

(use-package helpful
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)
  ("C-h x" . #'helpful-command))

(use-package fretboard
  :ensure (:host github :repo "skyefreeman/fretboard.el"))

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

(setq load-path (cons (expand-file-name "kill-dwim" rgr/emacs-project-dir ) load-path))
(use-package kill-dwim
  :ensure nil
  :bind
  ("M-w" . kill-dwim))

(use-package flymake
  :custom
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-no-changes-timeout 1.5)
  :bind
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error))

(use-package flymake-shellcheck
  :disabled t
  :commands flymake-shellcheck-load
  :init
  (defun rgr/sh-mode-hook()
    (flymake-shellcheck-load)
    (flymake-mode +1))
  :hook (sh-mode . rgr/sh-mode-hook))

(defun rgr/elisp-edit-mode()
  "return non nil if this buffer edits elisp"
  (member major-mode '(emacs-lisp-mode lisp-interaction-mode)))

(use-package package-lint)

(setq load-path (cons (expand-file-name "el-docstring-sap" rgr/emacs-project-dir ) load-path))
(use-package el-docstring-sap
  :after (eldoc posframe)
  :ensure nil
  :hook
  (emacs-lisp-mode . el-docstring-sap-mode)
  :bind
  ("M-<f2>" . el-docstring-sap-display)
  ("M-<f1>" . el-docstring-sap-mode))

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

(use-package
  elisp-format
  :bind
  (:map emacs-lisp-mode-map
        ("C-c f" . elisp-format-region)))

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

(use-package posframe
  :demand t)

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  ("M-d" . ace-delete-window))

(use-package ace-link
  :demand
  :config
  (ace-link-setup-default))

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

(use-package boxquote
  ;;:straight (:branch "main")
  :bind
  ("C-S-r" . boxquote-region))

(use-package
  volatile-highlights
  :init (volatile-highlights-mode 1))

(use-package
  dpaste
  :init
  :bind ("C-c y" . dpaste-region-or-buffer))

(use-package
  darkroom
  :bind
  ( "<C-f7>" . 'darkroom-mode))

(use-package olivetti
  :disabled t
  :bind 
  ("C-S-<f7>" . olivetti-mode))

(use-package writeroom-mode
  :bind 
  ("C-S-<f7>" . writeroom-mode))

(defun consult-buffer-other-tab ()
  "Variant of `consult-buffer' which opens in other tab."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-tab))
    (consult-buffer)))

(use-package modern-tab-bar
:ensure (:host github :repo "aaronjensen/emacs-modern-tab-bar")
:custom
(tab-bar-show t)
(tab-bar-new-button nil)
(tab-bar-close-button-show nil)
(tab-bar-select-tab-modifiers '(control))
:init
(modern-tab-bar-mode))

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

(defvar rgr/last-dired nil "last dired buffer opened")
(add-hook 'dired-after-readin-hook (lambda()(setq rgr/last-dired (buffer-name))))
(defun switch-to-dired(&optional d)
  (if (and rgr/last-dired (get-buffer rgr/last-dired))
      (switch-to-buffer rgr/last-dired)
    (let ((db (nth 0 (last (get-buffers-with-major-mode "dired-mode")))))
      (if db (switch-to-buffer db)
        (dired (if d d "~"))))))

(use-package multiple-cursors
  :bind
  ("C-<mouse-1>" . add-cursor-on-click)
  ("C-S-n" . mc/mark-next-like-this)
  ("C-S-p" . mc/mark-previous-like-this)
  ("C-S-w" . mc/mark-all-dwim)
  ("C-S-a" . mc/mark-all-like-this)
  ("C-S-SPC" . mc/edit-lines)
  )

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind
  ("M-$" . jinx-correct)
  ("<f8>" . jinx-correct)
  ("S-<f8>" . jinx-correct-word)
  ("C-<f8>" . jinx-languages))

(add-hook 'elpaca-after-init-hook (lambda()(load-el-gpg (expand-file-name "etc/late-load" user-emacs-directory))))
