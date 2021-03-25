Using org-babel to write out the config


# [straight.el](https://github.com/raxod502/straight.el#bootstrapping-straightel) package management

```emacs-lisp
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
```


# config


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


## Customization


### Standard Emacs customisation

```emacs-lisp
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file 'noerror)
```


### Load user customisations and gpg

Load all files in certain directories.

```emacs-lisp
(defun load-el-gpg (load-dir)
  (message "attempting mass load from %s." load-dir)
  (when (file-exists-p load-dir)
    (dolist (f (directory-files-recursively load-dir "\.[el|gpg]$"))
      (condition-case nil
          (load f 'no-error)
        (error nil)))))
```

1.  early load

    ```emacs-lisp
    (load-el-gpg (no-littering-expand-etc-file-name "early-load"))
    ```

2.  host specific

    Stick a custom in here. eg my thinkpad [custom file](./etc/hosts/thinkpadx270/custom.el).

    ```emacs-lisp
    (load-el-gpg (expand-file-name (system-name)  (no-littering-expand-etc-file-name "hosts")))
    ```


## [Auth-Sources](https://www.gnu.org/software/emacs/manual/auth.html)

Let emacs take care of security things automagically

```emacs-lisp
(use-package auth-source
  :demand
  :custom
  (auth-sources '("~/.gnupg/auth/authinfo.gpg" "~/.gnupg/auth/authirc.gpp"))
  :no-require t
  )
```


## Macros & Utilities


### General utility functions

Raw: [rgr-utils](etc/elisp/rgr-utils.el).

```emacs-lisp
(require 'rgr/utils "rgr-utils" 'NOERROR)
```

1.  rgr-utils library

    1.  line

        ```emacs-lisp
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
        ```

    2.  scratch

        ```emacs-lisp
        (use-package scratch)
        ```

    3.  provide

        ```emacs-lisp
        (provide 'rgr/utils)
        ```


### Emacs Lisp, ELisp Utils

Load this relatively early in order to have utils available if there's a faied load Raw: [rgr/elisp-utils](etc/elisp/rgr-elisp-utils.el)

```emacs-lisp
(require 'rgr/elisp-utils (expand-file-name "rgr-elisp-utils" elisp-dir))
(global-set-key (kbd "C-M-S-e") 'rgr/elisp-helpers-popup-help-enabled-toggle)
```

1.  rgr/elisp-utils library

    1.  emacs source

        ```emacs-lisp
        (defcustom rgr/emacs-source (no-littering-expand-var-file-name "emacs-source/current") "where the source is for the current emacs")
        (setq source-directory rgr/emacs-source)
        ```

    2.  elisp checks

        ```emacs-lisp
        (defun rgr/elisp-edit-mode()
          "return non nil if this buffer edits elisp"
          (member major-mode '(emacs-lisp-mode lisp-interaction-mode)))
        ```

    3.  helpful, enriched elisp help

        ```emacs-lisp
        (use-package helpful
          :config
          ;; Note that the built-in `describe-function' includes both functions
          ;; and macros. `helpful-function' is functions only, so we provide
          ;; `helpful-callable' as a drop-in replacement.
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

    4.  read and write elisp vars to file

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

    5.  delayed idle help popup

        ```emacs-lisp
        (defcustom rgr/elisp-helpers-popup-help-delay 1.5 "How long to delay for auto popup of symbol at point" :type 'float)
        (defcustom rgr/elisp-helpers-popup-help-enabled t "If popup elisp help is timer enabled" :type 'boolean)

        (defun rgr/elisp-helpers-popup-help-enabled-toggle()
          (interactive)
          (setq-local rgr/elisp-helpers-popup-help-enabled (not rgr/elisp-helpers-popup-help-enabled)))

        (defun rgr/elisp-helpers-popup-help-enable()
          "buffer local rgr/elisp-helpers-popup-help-enabled on"
          (setq-local rgr/elisp-helpers-popup-help-enabled rgr/elisp-helpers-popup-help-enabled))

        (add-hook 'emacs-lisp-mode-hook #'rgr/elisp-helpers-popup-help-enable)
        (add-hook 'lisp-interaction-mode-hook #'rgr/elisp-helpers-popup-help-enable)
        (add-hook 'org-mode-hook #'rgr/elisp-helpers-popup-help-enable)
        (add-hook 'help-mode-hook #'rgr/elisp-helpers-popup-help-enable)

        (defun chunyang-elisp-function-or-variable-quickhelp (&optional symbol)
          "Display summary of function or variable at point.

          Adapted from `describe-function-or-variable'."
          (interactive)
          (when rgr/elisp-helpers-popup-help-enabled
            (let* ((v-or-f (variable-at-point))
                   (found (symbolp v-or-f))
                   (v-or-f (if found v-or-f (function-called-at-point))))
              (if (and v-or-f (symbolp v-or-f))
                  (let* ((fdoc (when (fboundp v-or-f )
                                 (or (documentation v-or-f  t) "Not documented.")))
                         (fdoc-short (and (stringp fdoc)
                                          (substring fdoc 0 (string-match "\n" fdoc))))
                         (vdoc (when  (boundp v-or-f )
                                 (or (documentation-property v-or-f  'variable-documentation t)
                                     "Not documented as a variable.")))
                         (vdoc-short (and (stringp vdoc)
                                          (substring vdoc 0 (string-match "\n" vdoc)))))
                    (and (require 'popup nil 'no-error)
                         (popup-tip
                          (or
                           (and fdoc-short vdoc-short
                                (concat fdoc-short "\n\n"
                                        (make-string 30 ?-) "\n" (symbol-name
                                                                  v-or-f )
                                        " is also a " "variable." "\n\n"
                                        vdoc-short))
                           fdoc-short
                           vdoc-short)
                          :margin t)))))))

        (run-with-idle-timer rgr/elisp-helpers-popup-help-delay t '(lambda()(when (rgr/elisp-edit-mode) (chunyang-elisp-function-or-variable-quickhelp nil))))

        ```

    6.  Elisp debugging

        ```emacs-lisp
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
            "use the universal prefix arg (C-u) to remove instrumentation"
            (interactive)
            (if current-prefix-arg (eval-defun nil) (eval-defun 0)))
          )
        ```

    7.  Auto-compile

        ```emacs-lisp
        (use-package
          auto-compile
          :demand
          :config
          (auto-compile-on-load-mode 1)
          (auto-compile-on-save-mode 1))

        ;; (when (memq window-system '(mac ns x))
        ;;   (exec-path-from-shell-initialize))
        ```

    8.  Formatting

        ```emacs-lisp
        (use-package
          elisp-format
          :bind
          (:map emacs-lisp-mode-map
                ("C-c f" . elisp-format-region)))
        ```

    9.  popup query symbol

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

    10. provide

        ```emacs-lisp
        (provide 'rgr/elisp-utils)
        ```


## Emacs daemon & startup

Load up the daemon if not loaded, amongst other things.

Raw: [rgr/daemon](etc/elisp/rgr-daemon.el)

```emacs-lisp
(require 'rgr/daemon "rgr-daemon" 'NOERROR)
```


### rgr-daemon libarary

```emacs-lisp

;; start emacs-server if not running
(unless(daemonp)
  (add-hook 'after-init-hook (lambda ()
                               (require 'server)
                               (unless (server-running-p)
                                 (message "Starting EmacsServer from init as not already running.")
                                 (server-start))
                               ))
  )

(defun startHook()
  (message "In emacs-startup-hook"))

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

(provide 'rgr/daemon)
```


### System

1.  date format

    org-journal uses format-timestring which picks up sys locale. various elisp func calls didnt work ended up using [own format](#config-Emacs_daemon_&_startup-System-date_format-simply_redefine_org_journal_date_function)

    1.  simply redefine org journal date function

        > '(org-journal-date-format "%d/%m/%Y")


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

2.  fzf

    ```emacs-lisp
    (use-package fzf)
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

    2.  Priviliged file editing

        ```emacs-lisp
        (use-package sudo-edit)
        ```

    3.  find file at point - documented in the [selectrum wiki](https://github.com/raxod502/selectrum/wiki/Additional-Configuration#complete-file-names-at-point)

        ```emacs-lisp
        (use-package ffap
          :custom
          (ffap-require-prefix nil)
          :config
          (add-hook 'completion-at-point-functions
                    (defun complete-path-at-point+ ()
                      (let ((fn (ffap-file-at-point))
                            (fap (thing-at-point 'filename)))
                        (when (and (or fn
                                       (equal "/" fap))
                                   (save-excursion
                                     (search-backward fap (line-beginning-position) t)))
                          (list (match-beginning 0)
                                (match-end 0)
                                #'completion-file-name-table)))) 'append)
          (ffap-bindings))
        ```

4.  [ctrlf](https://github.com/raxod502/ctrlf) - back to basics search

    ```emacs-lisp
    (use-package ctrlf
      :custom-face
      (ctrlf-highlight-active ((t (:inherit nil :background "gold" :foreground "dim gray"))))
      (ctrlf-highlight-passive ((t (:inherit nil :background "red4" :foreground "white"))))
      :custom
      (ctrlf-auto-recenter t nil nil "Customized with use-package ctrlf")
      (ctrlf-highlight-current-line t)
      (ctrlf-auto-recenter t)
      ;; (ctrlf-mode-bindings
      ;;  '(("C-s" . ctrlf-forward-fuzzy-regexp)
      ;;    ("C-r" . ctrlf-backward-fuzzy-regexp)
      ;;    ("C-M-s" . ctrlf-forward-literal)
      ;;    ("C-M-r" . ctrlf-backward-literal)
      ;;    ("M-s _" . ctrlf-forward-regexp)
      ;;    ))
      (ctrlf-mode-bindings
       '(("C-s" . ctrlf-forward-fuzzy-regexp)
         ("C-r" . ctrlf-backward-fuzzy-regexp)
         ))
      :config
      (ctrlf-mode +1))
    ```

5.  [Selectrum](https://github.com/raxod502/selectrum) provides UI for selection from candidate list

    ```emacs-lisp
    (use-package selectrum
      :config
      (selectrum-mode +1)
      :bind ("C-x C-z" . #'selectrum-repeat))
    ```

6.  [Prescient](https://github.com/raxod502/prescient.el) provides sorting and filtering.

    ```emacs-lisp
    (use-package prescient
      :config
      (prescient-persist-mode +1)
      (if (featurep 'selectrum)
          (use-package selectrum-prescient
            :config
            (selectrum-prescient-mode +1))))
    ```

7.  [Consult](https://github.com/minad/consult) provides various commands based on the Emacs completion function completing-read

    ```emacs-lisp
    (use-package consult
      ;; Replace bindings. Lazily loaded due by `use-package'.
      :bind (("C-x M-:" . consult-complex-command)
             ("C-c h" . consult-history)
             ;;               ("C-c m" . consult-mode-command)
             ("C-x b" . consult-buffer)
             ("C-x 4 b" . consult-buffer-other-window)
             ("C-x 5 b" . consult-buffer-other-frame)
             ("C-x r x" . consult-register)
             ("C-x r b" . consult-bookmark)
             ("M-g a" . consult-apropos)
             ("M-g g" . consult-goto-line)
             ("M-g M-g" . consult-goto-line)
             ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
             ("M-g l" . consult-line)          ;; "M-s l" is a good alternative.
             ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
             ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
             ("M-g r" . consult-ripgrep)      ;; or consult-grep, consult-ripgrep
             ("M-g f" . consult-find)          ;; or consult-fdfind, consult-locate
             ("M-g i" . consult-project-imenu) ;; or consult-imenu
             ("M-g e" . consult-error)
             ("M-g s" . consult-grep)
             ("M-s m" . consult-multi-occur)
             ("M-y" . consult-yank-pop)
             ("<f3>" . consult-ripgrep)

             ("<help> a" . consult-apropos)
             ;;("C-s" . consult-line)
             )

      ;; The :init configuration is always executed (Not lazy!)

      :init

      ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
      (fset 'multi-occur #'consult-multi-occur)
      ;;(fset 'projectile-ripgrep 'consult-ripgrep)

      ;; Configure other variables and modes in the :config section, after lazily loading the package
      :config

      ;; Optionally configure a function which returns the project root directory
      (autoload 'projectile-project-root "projectile")
      (setq consult-project-root-function #'projectile-project-root)

      ;; Optionally configure narrowing key.
      ;; Both < and C-+ work reasonably well.
      (setq consult-narrow-key "<") ;; (kbd "C-+")
      ;; Optionally make narrowing help available in the minibuffer.
      ;; Probably not needed if you are using which-key.
      ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

      ;; Optional configure a view library to be used by `consult-buffer'.
      ;; The view library must provide two functions, one to open the view by name,
      ;; and one function which must return a list of views as strings.
      ;; Example: https://github.com/minad/bookmark-view/
      ;; (setq consult-view-open-function #'bookmark-jump
      ;;       consult-view-list-function #'bookmark-view-names)

      ;; Optionally enable previews. Note that individual previews can be disabled
      ;; via customization variables.
      ;; (consult-preview-mode)

      (defun mode-buffer-exists-p (mode)
        (seq-some (lambda (buf)
                    (provided-mode-derived-p
                     (buffer-local-value 'major-mode buf)
                     mode))
                  (buffer-list)))

      (defvar eshell-source
        `(:category 'consult-new
                    :name "EShell"
                    :narrow ?e
                    :face     'font-lock-constant-face
                    :open
                    ,(lambda (&rest _) (eshell))
                    :items
                    ,(lambda ()
                       (unless (mode-buffer-exists-p 'eshell-mode)
                         '("*eshell* (new)")))))

      (defvar term-source
        `(:category 'consult-new
                    :name "Term"
                    :narrow ?t
                    :face     'font-lock-constant-face
                    :open
                    ,(lambda (&rest _)
                       (ansi-term (or (getenv "SHELL") "/bin/sh")))
                    :items
                    ,(lambda ()
                       (unless (mode-buffer-exists-p 'term-mode)
                         '("*ansi-term* (new)")))))

      (add-to-list 'consult-buffer-sources 'eshell-source 'append)
      (add-to-list 'consult-buffer-sources 'term-source 'append)
      )


    ;; Enable Consult-Selectrum integration.
    ;; This package should be installed if Selectrum is used.
    (use-package consult-selectrum
      :disabled t
      :after selectrum
      :demand t)

    ;; Optionally add the `consult-flycheck' command.
    (use-package consult-flycheck
      :bind (:map flycheck-command-map
                  ("!" . consult-flycheck)))
    ```

8.  [Embark](https://github.com/oantolin/embark) Emacs Mini-Buffer Actions Rooted in Keymaps

    ```emacs-lisp
    (use-package embark
      :demand t
      :config
      (add-hook 'embark-target-finders
                (defun current-candidate+category ()
                  (when selectrum-active-p
                    (cons (selectrum--get-meta 'category)
                          (selectrum-get-current-candidate)))))

      (add-hook 'embark-candidate-collectors
                (defun current-candidates+category ()
                  (when selectrum-active-p
                    (cons (selectrum--get-meta 'category)
                          (selectrum-get-current-candidates
                           ;; Pass relative file names for dired.
                           minibuffer-completing-file-name)))))

      ;; No unnecessary computation delay after injection.
      (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)

      ;; The following is not selectrum specific but included here for convenience.
      ;; If you don't want to use which-key as a key prompter skip the following code.

      (setq embark-action-indicator
            (lambda (map) (which-key--show-keymap "Embark" map nil nil 'no-paging)
              #'which-key--hide-popup-ignore-command)
            embark-become-indicator embark-action-indicator)
      :bind
      ("C-S-a" . embark-act)) ; pick some comfortable binding
    ```

9.  [Marginalia](https://en.wikipedia.org/wiki/Marginalia) are marks or annotations placed at the margin of the page of a book or in this case helpful colorful annotations placed at the margin of the minibuffer for your completion candidates

    The [marginalia](https://github.com/minad/marginalia) pckage in emacs is very helpful.

    ```emacs-lisp
    (use-package marginalia
      :custom
      (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
      :config
      (marginalia-mode)
      (advice-add #'marginalia-cycle :after
                  (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))
    ```

10. provide

    ```emacs-lisp
    (provide 'rgr/minibuffer)
    ```


## Completion

Let emacs suggest completions

Raw:[rgr/completion](etc/elisp/rgr-completion.el)

```emacs-lisp
(require 'rgr/completion "rgr-completion" 'NOERROR)
```


### library

1.  Abbrev Mode

    [Abbrev Mode](https://www.emacswiki.org/emacs/AbbrevMode#toc4) is very useful for expanding small text snippets

    ```emacs-lisp
    (setq-default abbrev-mode 1)
    ```

2.  Snippets with yasnippet

    ```emacs-lisp
    (use-package
      yasnippet
      :init (yas-global-mode 1)
      :config
      (use-package
        php-auto-yasnippets)
      (use-package
        el-autoyas)
      (use-package
        yasnippet-snippets)
      (use-package
        yasnippet-classic-snippets))
    ```

3.  Company Mode

    [company-box](https://github.com/sebastiencs/company-box) provides nice linked help on the highlighted completions when available.

    ![img](./images/company-box.png "company-box in action for php/lsp mode.")

    ```emacs-lisp
    (use-package
      company
      :init
      (add-hook 'after-init-hook 'global-company-mode)
      :custom
      (company-backends
       '((company-capf :with company-dabbrev company-files company-ispell)))
      :config
      (use-package
        company-box
        :hook (company-mode . company-box-mode))
      (require 'company-ispell)
      (use-package company-prescient
        :after company
        :config
        (company-prescient-mode +1))
      ) ;; :bind ("C-<tab>" . company-complete))
    ```

4.  Which Key

    [which-key](https://github.com/justbur/emacs-which-key) shows you what further key options you have if you pause on a multi key command.

    ```emacs-lisp
    (use-package
      which-key
      :demand t
      :config (which-key-mode))
    ```

5.  Tying it all together

    ```emacs-lisp
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
    ```

6.  provide

    ```emacs-lisp
    (provide 'rgr/completion)
    ```


## Alert and Alert Learn

Added in a utility which pops up fortunes when idle and a hot key (see google-translate section to call up google-translate for the last fortune shown.. Handy for learning a language.

Raw: [rgr/alert-learn](etc/elisp/rgr-alert-learn.el).

```emacs-lisp
(require  'rgr/alert-learn "rgr-alert-learn" 'NOERROR)
(alert "Emacs is starting!")
```


### library

Raw: [rgr-alert-learn.el](etc/elisp/rgr-alert-learn.el).

```emacs-lisp

(use-package
  alert
  :demand
  :init
  (defvar rgr/alert-learn-history  nil "list of learns in rgr/alert-learn")
  (defgroup rgr/alert-learn nil "Options to control the Alert based popup learning" :group 'rgr)
  (defcustom rgr/alert-learn-on t "Whether pop up learning is on" :type 'boolean :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-command "fortune de" "Shell command to run to generate a learn" :type 'boolean :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-period 120 "How many seconds before another learn is displayed when idle" :type 'integer :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-fade-time 30 "How many seconds before the learn fades out" :type 'integer :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-history-auto-save  t "Auto save learn history?" :type 'boolean :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-history-file  (if (featurep 'no-littering) (no-littering-expand-var-file-name "alert-learn/alert-learn.txt" ) (expand-file-name "alert-learn.alist" user-emacs-directory)) "filename in which to save learns" :type '() :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-history-length 200 "How many learns to store in history" :type 'integer :group 'rgr/alert-learn)
  :config

  (defvar rgr/alert-learn-timer nil "timer object for alert-learn prompt")

  (let ((dir (file-name-directory rgr/alert-learn-history-file)))
    (unless (file-exists-p dir)
      (make-directory dir)))

  (defun rgr/alert-learn-history-save()
    (when rgr/alert-learn-history-auto-save
      (rgr/elisp-write-var rgr/alert-learn-history-file (butlast rgr/alert-learn-history (- (length rgr/alert-learn-history) rgr/alert-learn-history-length)))))

  (defun rgr/alert-learn-history-load()
    (when rgr/alert-learn-history-auto-save
      (setq rgr/alert-learn-history (rgr/elisp-read-var rgr/alert-learn-history-file))))

  (defun rgr/alert-learn-select-from-history()
    (interactive)
    (if rgr/alert-learn-history ;; select from old ones by bubbling one to top
        (let ((learn (completing-read "Select learn:" rgr/alert-learn-history)))
          (when learn
            (setq rgr/alert-learn-history (remove learn rgr/alert-learn-history))
            (add-to-list 'rgr/alert-learn-history learn)
            (rgr/alert-learn-history-save)))
      (message "No previous learns."))
    (car rgr/alert-learn-history))

  (defun rgr/alert-fortune(&optional title id)
    ;; alert a fortune and return it. (rgr/alert-fortune)
    (let ((f (shell-command-to-string rgr/alert-learn-command)))
      (alert  f :title (or title "Fortune favours the brave...") :id (or id 'emacs))
      f))

  (defun rgr/alert-learn()
    ;; alert a learn and store it. (rgr/alert-learn)
    (interactive)
    (let ((alert-fade-time rgr/alert-learn-fade-time)
          (learn (rgr/alert-fortune "Learn Now..." "learn")))
      (add-to-list 'rgr/alert-learn-history learn)
      (rgr/alert-learn-history-save)
      ;;
      )
    nil)

  (defun rgr/alert-learn-timer()
    "start or remove learn timer as appropriate."
    (if rgr/alert-learn-on
        (unless rgr/alert-learn-timer
          (setq rgr/alert-learn-timer
                (run-with-idle-timer rgr/alert-learn-period t 'rgr/alert-learn)))
      (when rgr/alert-learn-timer
        (cancel-timer rgr/alert-learn-timer)
        (setq rgr/alert-learn-timer nil))))

  (defun rgr/google-translate-learn(&optional prefix)
    "prefix 1 to toggle on/off, 2 to create a new learn, anything else to swap languages"
    (interactive "P")
    (when prefix
      (if (eq prefix 0) ;; C-u 0 to toggle on/off
          (progn
            (setq rgr/alert-learn-on (if rgr/alert-learn-on nil t))
            (rgr/alert-learn-timer))
        (if (eq prefix 1) ;; C-u 1 to create new learn
            (rgr/alert-learn)
          (if (eq prefix 2) ;; C-u 2 to select an old learn and relearn it
              (rgr/alert-learn-select-from-history)
            (google-translate-swap-default-languages)))))
    (when rgr/alert-learn-on
      (let ((learn (car rgr/alert-learn-history)))
        (when learn
          (google-translate-translate google-translate-default-source-language google-translate-default-target-language (car rgr/alert-learn-history))
          (if google-translate-pop-up-buffer-set-focus
              (select-window (display-buffer "*Google Translate*")))))))

  (rgr/alert-learn-history-load)

  (rgr/alert-learn-timer)

  :bind
  ("C-c L" . rgr/google-translate-learn))

(provide 'rgr/alert-learn)


```


## General configuration

Raw: [rgr/general-config](etc/elisp/rgr-general-config.el).

```emacs-lisp
(require  'rgr/general-config "rgr-general-config" 'NOERROR)
```


### library

1.  General

    ```emacs-lisp
    (require 'iso-transl) ;; supposed to cure deadkeys when my external kbd is plugged into my thinkpad T44460.  It doesnt.
                                            ; t60
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (show-paren-mode 1)
    (winner-mode 1)
    (tooltip-mode 1)
    (global-auto-revert-mode)

    (global-visual-line-mode 1)

    (setq column-number-mode t)

    (delete-selection-mode 1)

    (global-set-key (kbd "S-<f1>") 'describe-face)

    (global-set-key (kbd "S-<f10>") #'menu-bar-open)
                                            ;          (global-set-key (kbd "<f10>") #'imenu)


    (setq frame-title-format (if (member "-chat" command-line-args)  "Chat: %b" "Emacs: %b")) ;; used to select the window again (frame-list)

    (defalias 'yes-or-no-p 'y-or-n-p)

    ;; Auto refresh buffers
    (global-auto-revert-mode 1)

    ;; Also auto refresh dired, but be quiet about it
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)

    ;; ;; restore desktop
    (setq desktop-dirname (expand-file-name "desktop" user-emacs-directory))
    ;; (desktop-save-mode 1)

    (setq disabled-command-function nil)

    (global-hl-line-mode t)

    (use-package boxquote
      :straight (:branch "main")
      :bind
      ("C-S-r" . boxquote-region))

    (use-package
      browse-url-dwim)

    (use-package
      all-the-icons)

    (use-package beacon
      :custom
      (beacon-blink-delay 1)
      (beacon-size 10)
      (beacon-color "orange" nil nil "Customized with use-package beacon")
      (beacon-blink-when-point-moves-horizontally 32)
      (beacon-blink-when-point-moves-vertically 8)
      :config
      (beacon-mode 1))

    (use-package
      webpaste
      :bind ("C-c y" . (lambda()(interactive)(call-interactively 'webpaste-paste-region)(deactivate-mark)))
      ("C-c Y" . webpaste-paste-buffer))

    ;; brings visual feedback to some operations by highlighting portions relating to the operations.
    (use-package
      volatile-highlights
      :init (volatile-highlights-mode 1))
    ;; display dir name when core name clashes
    (require 'uniquify)

    (add-to-list 'Info-directory-list (expand-file-name "info" user-emacs-directory)) ;; https://www.emacswiki.org/emacs/ExternalDocumentation


    (global-set-key (kbd "C-c r") 'query-replace-regexp)

    ```

2.  Accessibility

    1.  fonts

        JetBrains fonts are nice. See [nerd-fonts](https://github.com/ryanoasis/nerd-fonts)

        :CUSTOM\_ID: General\_configuration-general\_config\_library-Accessibility-fonts-93efb5b8

        ```emacs-lisp
        ;; (set-face-attribute 'default nil :height 60 :family "JetBrainsMono Nerd Font" :foundry "JB")
        ```

    2.  Darkroom

        Zoom in and center using [darkroom](https://github.com/joaotavora/darkroom).

        ```emacs-lisp
        (use-package
          darkroom
          :bind
          ( "<f7>" . 'darkroom-mode))
        ```

3.  Transparency

    ```emacs-lisp
    (set-frame-parameter (selected-frame) 'alpha '(95 . 50))
    (add-to-list 'default-frame-alist '(alpha . (95 . 50)))
    ```

4.  Clipboard

    Allow terminal emacs to interact with the x clipboard.

    ```emacs-lisp
    (use-package xclip
      :demand t
      :config
      (xclip-mode))
    ```

5.  Ansi colour

    [Ansi colour hooks](https://www.emacswiki.org/emacs/AnsiColor) to enable emacs buffers to handle ansi.

    ```emacs-lisp
    (require 'ansi-color)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
    ```

6.  Tab Bar Mode

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

7.  Memory

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

8.  provide

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
      (org-M-RET-may-split-line nil)
      (org-agenda-files (no-littering-expand-etc-file-name "org/agenda-files.txt" ))
      (org-agenda-include-diary t)

      (org-agenda-remove-tags nil)
      (org-agenda-restore-windows-after-quit t)
      (org-agenda-show-inherited-tags t)
      (org-agenda-skip-deadline-if-done t)
      (org-agenda-skip-scheduled-if-done t)
      (org-agenda-skip-scheduled-if-deadline-is-shown t)
      (org-agenda-skip-scheduled-delay-if-deadline t)
      (org-agenda-start-on-weekday 0)
      (org-agenda-window-setup 'current-window)
      (org-babel-load-languages '((emacs-lisp . t)(python . t) (shell . t)))
      ;;         (org-babel-python-command "python3")
      (org-catch-invisible-edits 'smart)
      (org-clock-into-drawer t)
      (org-clock-persist 'history)
      (org-confirm-babel-evaluate nil)
      (org-contacts-files (no-littering-expand-etc-file-name  "org/orgfiles/contacts.org"))
      (org-crypt-disable-auto-save t)
      (org-ctrl-k-protect-subtree t)
      (org-default-notes-file "refile.org")
      (org-directory (no-littering-expand-var-file-name "org/orgfiles" ))
      (org-enforce-todo-dependencies t)
      (org-export-backends '(ascii html icalendar latex md odt))
      (org-google-weather-location "Hamburg,Germany" t)
      (org-goto-interface 'outline-path-completion)
      (org-link-search-must-match-exact-headline nil)
      (org-log-done 'time)
      (org-log-into-drawer t)
      (org-log-note-clock-out t)
      (org-outline-path-complete-in-steps nil)
      (org-refile-allow-creating-parent-nodes 'confirm)
      (org-refile-use-outline-path 'file)
      (org-refile-targets         '((org-agenda-files :tag . "refile")           (org-agenda-files :maxlevel . 16)))
      (org-remember-clock-out-on-exit nil t)
      (org-return-follows-link t)
      (org-reverse-note-order t)
      (org-tag-persistent-alist '(("noexport" . 110) ("trash" . 116)))
      (org-tags-exclude-from-inheritance '("PROJECT" "DEFAULTCLOCKTASK" "crypt"))
      (org-use-property-inheritance t)
      :init
      (defun rgr/org-refile-targets()
        (directory-files-recursively org-directory "^[[:alnum:]].*\\.\\(org\\|gpg\\)\\'"))
      :config
      (setq org-babel-default-header-args:python
            '((:results  . "output")))

      (progn
        ;;  stuff for setting fixed ref links - https://blog.phundrak.com/better-custom-ids-orgmode/
        (require 'org-id)
        (defun eos/org-id-new (&optional prefix)
          "Create a new globally unique ID.

           An ID consists of two parts separated by a colon:
           - a prefix
           - a   unique   part   that   will   be   created   according   to
             `org-id-method'.

           PREFIX  can specify  the  prefix,  the default  is  given by  the
           variable  `org-id-prefix'.  However,  if  PREFIX  is  the  symbol
           `none', don't  use any  prefix even if  `org-id-prefix' specifies
           one.

           So a typical ID could look like \"Org-4nd91V40HI\"."
          (let* ((prefix (if (eq prefix 'none)
                             ""
                           (concat (or prefix org-id-prefix)
                                   "-"))) unique)
            (if (equal prefix "-")
                (setq prefix ""))
            (cond
             ((memq org-id-method
                    '(uuidgen uuid))
              (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
              (unless (org-uuidgen-p unique)
                (setq unique (org-id-uuid))))
             ((eq org-id-method 'org)
              (let* ((etime (org-reverse-string (org-id-time-to-b36)))
                     (postfix (if org-id-include-domain
                                  (progn
                                    (require 'message)
                                    (concat "@"
                                            (message-make-fqdn))))))
                (setq unique (concat etime postfix))))
             (t (error "Invalid `org-id-method'")))
            (concat prefix (car (split-string unique "-")))))
        (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
        (defun eos/org-custom-id-get (&optional pom create prefix)
          "Get the CUSTOM_ID property of the entry at point-or-marker POM.

           If POM is nil, refer to the entry at point. If the entry does not
           have an CUSTOM_ID, the function returns nil. However, when CREATE
           is non nil, create a CUSTOM_ID if none is present already. PREFIX
           will  be passed  through to  `eos/org-id-new'. In  any case,  the
           CUSTOM_ID of the entry is returned."
          (interactive)
          (org-with-point-at pom
            (let* ((orgpath (mapconcat #'identity (org-get-outline-path) "-"))
                   (heading (replace-regexp-in-string
                             "/\\|~\\|\\[\\|\\]" ""
                             (replace-regexp-in-string
                              "[[:space:]]+" "_" (if (string= orgpath "")
                                                     (org-get-heading t t t t)
                                                   (concat orgpath "-" (org-get-heading t t t t))))))
                   (id (org-entry-get nil "CUSTOM_ID")))
              (cond
               ((and id
                     (stringp id)
                     (string-match "\\S-" id)) id)
               (create (setq id (eos/org-id-new (concat prefix heading)))
                       (org-entry-put pom "CUSTOM_ID" id)
                       (org-id-add-location id
                                            (buffer-file-name (buffer-base-buffer)))
                       id)))))

        (defun eos/org-add-ids-to-headlines-in-file ()
          "Add CUSTOM_ID properties to all headlines in the current file
           which do not already have one.

           Only adds ids if the `auto-id' option is set to `t' in the file
           somewhere. ie, #+OPTIONS: auto-id:t"
          (interactive)
          (save-excursion
            (widen)
            (goto-char (point-min))
            (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t"
                                     (point-max)
                                     t)
              (org-map-entries (lambda ()
                                 (eos/org-custom-id-get (point)
                                                        'create))))))

        (add-hook 'org-mode-hook
                  (lambda ()
                    (add-hook 'before-save-hook
                              (lambda ()
                                (when (and (eq major-mode 'org-mode)
                                           (eq buffer-read-only nil))
                                  (eos/org-add-ids-to-headlines-in-file)))))))

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
      ("C-c C-t" . org-todo))

    (use-package ob-async)

    (use-package
      org-bullets
      :demand
      :config (add-hook 'org-mode-hook (lambda ()
                                         (org-bullets-mode 1)
                                         ;; (org-num-mode 1)
                                         )))

    (org-clock-persistence-insinuate)
    (add-hook 'auto-save-hook 'org-save-all-org-buffers)

    ;; The following lines are always needed.  Choose your own keys.

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
    ```

    1.  org-id

        ```emacs-lisp
        (require 'org-id)
        (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
        ```

    2.  org-appear

        provides a way to toggle visibility of emphasis markers, links, subscripts, and superscripts by customising variables such as org-hide-emphasis-markers

        ```emacs-lisp
        (use-package org-appear
          :disabled ;; PITA
          :straight(:type git :host github :repo "awth13/org-appear")
          :custom
          (org-appear-autoemphasis  t)
          (org-appear-autolinks t)
          (org-appear-autosubmarkers t)
          :config
          (add-hook 'org-mode-hook 'org-appear-mode))
        ```

    3.  org-tempo

        ```emacs-lisp
        (require 'org-tempo)
        (add-to-list 'org-structure-template-alist '("ba" . "src bash"))
        (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
        (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
        (add-to-list 'org-structure-template-alist '("py" . "src python"))
        ```

    4.  org crypt

        1.  config

            ```emacs-lisp
            (require 'org-crypt)
            (setq  org-crypt-key "rileyrg")
            (org-crypt-use-before-save-magic)
            ```

    5.  org agenda

        maintain a file pointing to agenda sources

        ```conf
        ~/.emacs.d/var/org/orgfiles/
        ~/.emacs.d/var/org/orgfiles/journals/
        ~/.emacs.d/var/lessons/
        ~/.emacs.d/var/org/orgfiles/projects/
        ~/development/projects/Python/python-lernen.de
        ```

    6.  Journal, org-journal

        More advanced journalling courtesy of [org-journal](https://github.com/bastibe/org-journal). A great habit it to get into is use org journal to todo your day and refile at a later date. Obviously add your journal files to your agenda files!

        ```emacs-lisp
        (use-package org-journal
          :demand t
          :custom
          (org-journal-dir (expand-file-name "journals" org-directory))
          (org-journal-file-format "%Y%m%d.org")
          (org-journal-date-format "%d/%m/%Y")
          :config
          ;;(add-to-list 'org-agenda-files org-journal-dir)

          :hook (org-journal-mode  . (lambda ()
                                       (local-unset-key (kbd "C-c C-s"))))
          :bind (
                 ("C-c S" . org-journal-search)
                 ("C-c J" . org-journal-new-entry)
                 ))
        ```

2.  Self documenting config file, tangling

    Create a filename.export to auto export markdown

    ```emacs-lisp

    (use-package f);; f-touch

    (use-package ox-gfm);; github compliant markdown

    (defun rgr/org-tangle-and-export(&optional filename)
      (when (buffer-file-name)
        (let* ((filename (if filename filename (buffer-file-name)))
               (f-org (concat (file-name-sans-extension filename) ".org"))
               (f-export (concat (file-name-sans-extension f-org) ".export"))
               (f-tangle (concat (file-name-sans-extension f-org) ".tangle"))
               (alert-fade-time 3))
          (when (and (file-exists-p f-export) (file-newer-than-file-p f-org f-export))
            (when (featurep 'alert)
              (alert (format "%s is older than %s,exporting." f-export f-org)))
            (org-gfm-export-to-markdown)
            (f-touch f-export))
          (when (and (file-exists-p f-tangle) (file-newer-than-file-p f-org f-tangle))
            (when(featurep 'alert)
              (alert (format "%s is older than %s,tangling." f-tangle f-org)))
            (org-babel-tangle)
            (f-touch f-tangle)))))

    (advice-add #'magit-status :before (lambda()"look to see if we need to export and tangle"(interactive)(rgr/org-tangle-and-export)))

    ;;             (add-hook 'after-save-hook (lambda(tangle)(interactive "P")(when tangle (call-interactive #'rgr/org-tangle-and-export))))
    (add-hook 'after-save-hook (lambda()(when current-prefix-arg (rgr/org-tangle-and-export))))

    ```

3.  provide

    ```emacs-lisp
    (provide 'rgr/org)
    ```


## Text tools


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

2.  expand-region

    [expand-region](https://github.com/magnars/expand-region.el) is an Emacs extension to increase selected region by semantic units.

    ```emacs-lisp
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
    ```

3.  easy-kill

    [easy-kill](https://github.com/leoliu/easy-kill) enables you to kill & Mark Things Easily in Emacs

    ```emacs-lisp
    (use-package easy-kill
      :config
      (global-set-key [remap kill-ring-save] 'easy-kill))
    ```


### Folding/Hide Show

[hs-minor-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html) allows hiding and showing different blocks of text/code (folding).

```emacs-lisp
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
```


### rg, ripgrep

rg is pretty quick

```emacs-lisp
(use-package
  ripgrep)
```

1.  ~/bin/helm-rg-wrapper

    NB : left despite removing helm from config

    [The issue is that helm-projectile automatically adds options like &#x2013;ignore \*.o to ag. When it tries to send the same options to rg (which doesn't have an &#x2013;ignore option), you get errors. This behavior currently is not configurable.](https://gist.github.com/pesterhazy/fabd629fbb89a6cd3d3b92246ff29779#gistcomment-2352523)

    A workaround is to create a wrapper script that removes these options, named rg-wrapper

    ```bash
    #!/usr/bin/bash
    #Maintained in linux-scripts-and-configs.org
    # correcting applied ignores in helm/projectile emacs
    set -euo pipefail
    newargs="$(echo "$@" | sed 's/\-\-ignore .* //')"
    rg $newargs
    ```

    1.  .ignore for the emacs root sample

        ```conf
        # Maintained in emacs-config.org
        !*
        .git
        .cache
        auto-save
        history
        undohist
        var/*.el
        ```


### ag, silver searcher

```emacs-lisp
(use-package
  ag)
```


### Deft - text searching

[Deft](https://jblevins.org/projects/deft/) is an Emacs mode for quickly browsing, filtering, and editing directories of plain text notes, inspired by Notational Velocity. It was designed for increased productivity when writing and taking notes by making it fast and simple to find the right file at the right time and by automating many of the usual tasks such as creating new files and saving files.

```emacs-lisp
(use-package deft
  :config
  (setq deft-directory (expand-file-name "orgfiles" user-emacs-directory))
  (setq deft-recursive t)
  :bind(("M-<f3>" . #'deft)))

```


## Reference/Lookup/Media

lookup and reference uilities and config

Raw: [rgr/reference](etc/elisp/rgr-reference.el)

```emacs-lisp
(require 'rgr/reference "rgr-reference" 'NOERROR)
```


### reference library

1.  Google related

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
                (let  ((phrase (if phrase phrase (symbol-or-region-at-point-as-string-or-prompt))))
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
              ("C-c t" . rgr/google-translate-at-point)
              ("C-c T" . rgr/google-translate-query-translate)
              ("C-c b" . rgr/google-translate-in-history-buffer))


            ```

        3.  provide

            ```emacs-lisp
            (provide 'rgr/google)
            ```

2.  Reference and dictionary

    The aim here is to link to different reference sources and have a sensible default for different modes. eg elisp mode would use internal doc sources, whereas javascript uses Dash/Zeal or even a straight URL search to lookup help. On top of that provide a list of other sources you can call by prefixing the core lookup-reference-dwim call. But if you lookup internal docs and it doesnt exist then why not farm it out to something like Goldendict which you can configure to look wherever you want? Examples here show Goldendict plugged into google translate amonst other things. The world's your oyster.

    1.  utility funcs

        ```emacs-lisp

        (defgroup rgr/lookup-reference nil
          "Define functions to be used for lookup"
          :group 'rgr)

        (defcustom mode-lookup-reference-functions-alist '(
                                                           (nil (goldendict-dwim goldendict-dwim))
                                                           (c++-mode  (rgr/lsp-ui-doc-glance rgr/dash))
                                                           (gdscript-mode  (rgr/lsp-ui-doc-glance rgr/dash))
                                                           ;;                                                         (gdscript-mode  (rgr/gdscript-docs-browse-symbol-at-point rgr/dash))
                                                           (php-mode  (rgr/lsp-ui-doc-glance rgr/dash))
                                                           (web-mode  (rgr/lsp-ui-doc-glance rgr/devdocs))
                                                           (org-mode (rgr/elisp-lookup-reference-dwim))
                                                           (Info-mode (rgr/elisp-lookup-reference-dwim))
                                                           (js2-mode (rgr/dash rgr/devdocs))
                                                           (js-mode (rgr/dash rgr/devdocs))
                                                           (rjsx-mode (rgr/dash rgr/devdocs))
                                                           (typescript-mode (rgr/dash rgr/devdocs))
                                                           (lisp-interaction-mode (rgr/elisp-lookup-reference-dwim rgr/dash))
                                                           (emacs-lisp-mode (rgr/elisp-lookup-reference-dwim rgr/dash)))
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

        (defcustom  lookup-reference-functions '(rgr/describe-symbol goldendict-dwim rgr/linguee-lookup rgr/dictionary-search rgr/jquery-lookup google-this-search)
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

        (defun rgr/describe-symbol(w)
          (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
          (let ((s (if (symbolp w) w (intern-soft w))))
            (if s (describe-symbol s)
              (message "No such symbol: %s" w))))

        (defun rgr/linguee-lookup(w)
          (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
          (sys-browser-lookup w linguee-url-template))

        (defun rgr/php-api-lookup(w)
          (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
          (let ((dash-docs-docsets '("PHP")))
            (dash-docs-search w)))
        ;; (sys-browser-lookup w php-api-url-template))

        (defun rgr/jquery-lookup(&optional w)
          (interactive(cons (symbol-or-region-at-point-as-string-or-prompt) nil))
          (let (;;(zeal-at-point-docset "jQuery")
                (dash-docs-docsets '("jQuery")))
            (dash-docs-search w)))
        ;; (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
        ;; (sys-browser-lookup w jquery-url-template))

        (defun rgr/gdscript-docs-browse-symbol-at-point(&optional w)
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
            (dictionary-search (if w w (symbol-or-region-at-point-as-string-or-prompt))))
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
              (let* ((sym (if sym sym (symbol-or-region-at-point-as-string-or-prompt)))
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
            (let ((w (if w w (symbol-or-region-at-point-as-string-or-prompt))))
              (call-process-shell-command (format  "goldendict \"%s\"" w ) nil 0)))
          :bind (("C-c g" . goldendict-dwim)))
        ```

    5.  DevDocs

        ```emacs-lisp
        (use-package devdocs
          :commands (rgr/devdocs)
          :config
          (defun rgr/devdocs (&optional w)
            (interactive)
            (devdocs-search)t)
          :bind* ("C-c v" . 'rgr/devdocs))
        ```

    6.  Zeal - Linux Dash

        ```emacs-lisp
        (use-package zeal-at-point
          :disabled t ;;way too buggy
          :commands (rgr/zeal)
          :config
          (defun rgr/zeal (&optional w)
            (interactive)
            (zeal-at-point)t)
          :bind* ("C-c z" . 'rgr/zeal))
        ```

    7.  DASH - API documentation for most languages

        Dash packages docs for many languages.

        ```emacs-lisp
        (use-package
          dash-docs
          ;;:custom
          ;;(dash-docs-browser-func 'eww-readable-url)
          :config
          (setq dash-docs-common-docsets '("C++" "Emacs Lisp" "Docker"))
          (setq dash-docs-docsets '("C++" "Emacs Lisp" "Docker"))
          (defun rgr/dash (w)
            (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
            (message "docsets are: %s" dash-docs-docsets)
            (message "%s" (dash-docs-search w)))
          :bind ("C-c d" . 'rgr/dash))
        ```

3.  Man Pages

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

4.  Elfeed

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
      :bind ( "C-x w" . elfeed))

    ```

    1.  elfeed-org

        ```emacs-lisp

        ```

5.  provide

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


### open terminal in current/project  dir

```emacs-lisp
(use-package terminal-here
  :custom
  (terminal-here-terminal-command (list "oneterminal"))
  :bind
  ("C-<f5>" . #'terminal-here-project-launch)
  ("C-S-<f5>" . #'terminal-here-launch))
```


### Shell Switcher

[shell-switcher](https://github.com/DamienCassou/shell-switcher) allows easier shell switching.

```emacs-lisp
(use-package
  shell-switcher
  :config (setq shell-switcher-mode t)
  ;;(add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)
  :bind
  ("M-<f12>" . shell-switcher-switch-buffer)
  ("C-<f12>" . shell-switcher-new-shell))
```


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
            (shell-command "emacs -Q -l ~/.config/emacs/straight/repos/straight.el/bootstrap.el &")))
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
    alias resemacshub resgithub.sh "Emacs customisation centered around lsp,helm,dap and projectile."
    alias to httplug.client.app.http_methods
    alias grep grep --color=always --exclude="*.lock" --exclude-dir=log --exclude-dir=cache -iR $*
    alias gg *grep -C 2 -iR $*
    alias awg aw|*grep -C 2 -i $*
    alias dconp co debug:container  --show-private $*
    alias dcon co debug:container  $*
    alias dc co debug:config $*
    alias cc co cache:clear
    alias hg helm-projectile-grep
    alias ll ls -l $*
    alias aw co debug:autowiring
    alias cdu co config:dump $1
    alias pg projectile-grep
    alias co bin/console --no-ansi $*
    alias pff helm-projectile-find-file $*
    alias ff helm-projectile-find-file
    alias em cd ~/.emacs.d
    alias dcg dc $1 |*grep -C 5 -i $2
    alias coenv co about
    alias R/W multiple sector transfer: Max = 1 Current = 1
    alias sp projectile-switch-open-project
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

    1.  This setup uses the so called "plan 9" pattern documented [here](https://www.masteringemacs.org/article/complete-guide-mastering-eshell).

        > If smart display is enabled it will also let you review the output of long-running commands by using SPC to move down a page and BACKSPACE to move up a page. If any other key is pressed it will jump the end of the buffer, essentially acting in the same way as if smart display wasn’t enabled.
        >
        > Essentially, if Eshell detects that you want to review the last executed command, it will help you do so; if, on the other hand, you do not then Eshell will jump to the end of the buffer instead. It’s pretty clever about it, and there are switches you can toggle to fine-tune the behavior.

        Added in some pcomplete extensions for git from [Mastering Emacs](https://www.masteringemacs.org/article/pcomplete-context-sensitive-completion-emacs).

        ```emacs-lisp
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
        ```


### Docker

1.  docker

    A general interface to [docker](https://github.com/Silex/docker.el/tree/a2092b3b170214587127b6c05f386504cae6981b).

    ```emacs-lisp
    (use-package docker
      :after projectile
      :bind (:map projectile-mode-map ("C-c k" . docker)))
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

```emacs-lisp
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
```


### Buffer selection

1.  Bufler

    [Bufler](https://github.com/alphapapa/bufler.el) is like a butler for your buffers, presenting them to you in an organized way based on your instructions.

    ```emacs-lisp
    (use-package bufler
      :config
      (bufler-tabs-mode t)
      :bind
      ("C-x C-b" . 'bufler))
    ```


### dired - emacs file management     :dired:

1.  Dired Git Info

    ```emacs-lisp
    (use-package dired-git
      :config
      :hook ((dired-mode-hook . dired-git-mode)))
    ```

2.  dired hacks

    Collection of useful dired additions found on github [here](https://github.com/Fuco1/dired-hacks). Found out about it at the useful emacs resource [**Pragmatic Emacs**](http://pragmaticemacs.com/category/dired/).

    1.  dired subtree

        ```emacs-lisp
        (use-package dired-subtree
          :config
          (use-package dash)
          :bind (:map dired-mode-map
                      ("i" . dired-subtree-insert)
                      (";" . dired-subtree-remove)))
        ```

    2.  dired filter

        More dired based filtering see [dired-filter-prefix](dired-filter-prefix)

        ```emacs-lisp
        (use-package dired-filter
          :config
          (use-package dash)
          )
        ```

    3.  dired quicksort

        Dired sorting popup options from [Pragmatic Emacs](http://pragmaticemacs.com/emacs/speedy-sorting-in-dired-with-dired-quick-sort/). EDIT: cant clone from gitlab

        ```emacs-lisp
        (use-package dired-quick-sort
          :disabled t
          :config
          (dired-quick-sort-setup))
        ```


### PopUp Utilities

1.  posframe     :posframe:

    [Posframe](https://github.com/tumashu/posframe) can pop up a frame at point, this posframe is a child-frame connected to its root window's buffer.

    ```emacs-lisp
    (use-package posframe)
    ```

2.  popper     :popper:

    [Popper](https://github.com/karthink/popper) is a minor-mode to tame the flood of ephemeral windows Emacs produces, while still keeping them within arm’s reach. Designate any buffer to “popup” status, and it will stay out of your way.

    :CUSTOM\_ID: config-Buffers\_and\_Windows-PopUp\_Utilities-popper-d5aab8e6

    ```emacs-lisp
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

    [Ace-Window](https://github.com/abo-abo/ace-window) provides better window switching.

    ```emacs-lisp
    (use-package ace-window
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
      ("C-c j" . ace-jump-mode)
      )
    ```


### Elscreen

[Elscreen](https://github.com/knu/elscreen) provides tabs in Emacs.

```emacs-lisp
(use-package
  elscreen
  :disabled t
  :config (elscreen-start))
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


## Treemacs

Excellent [tree based navigation that works really well with projectile.](https://github.com/Alexander-Miller/treemacs)

```emacs-lisp
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

```


## Online Chats

Add a "-chat" [command line switch](https://www.gnu.org/software/emacs/manual/html_node/elisp/Command_002dLine-Arguments.html) to load the [Emacs IRC client](https://www.gnu.org/software/emacs/manual/html_mono/erc.html) and other such of choice. I created this as I tend to hop in and out of Emacs because of severe "[configuration fever](https://alhassy.github.io/init/)". You don't want to be hopping in and out of chat groups too.

```emacs-lisp
(defun rgr/load-chats(switch)
  (require  'rgr-chat))
(add-to-list 'command-switch-alist '("chat" . rgr/load-chats))
```

Small external script, [oneemacs-chat](./bin/oneemacs-chat), to create an erc instance if there isnt already one.

```shell
#!/bin/bash
WID=`xdotool search --name "Chat:"|head -1`
if [[ -z ${WID} ]]; then
    notify-send "Starting Chats in Emacs..."
    emacs -chat
else
    notify-send "restoring Chat instance..."
    xdotool windowactivate $WID
fi
```

```bash
#!/bin/bash
WID=`xdotool search --name "Chat:"|head -1`
if [[ -z ${WID} ]]; then
    notify-send "Starting Chats in Emacs..."
    emacs -chat
else
    notify-send "restoring Chat instance..."
    xdotool windowactivate $WID
fi
```

and the eshell func to call it:

```emacs-lisp
(defun eshell/chat-client
    (&rest
     args)
  "chat with emacs.."
  (interactive)
  (save-window-excursion (call-process "oneemacs-chat" nil 0)))

(global-set-key (kbd "C-c i") 'eshell/chat-client)
```

The code in [rgr-chat.el](./elisp/rgr-chat.el):

```emacs-lisp
(use-package erc :demand t
  :diminish erc-mode
  :config
  (require 'erc-replace)
  (defun textReadFont ()
    "Set font to a variable width (proportional) fonts in current buffer."
    (interactive)
    ;; (setq buffer-face-mode-face '(:family "arial"))
    ;; (buffer-face-mode)
    )

  (defun rgr/erc-switch-to-channel(&optional channel)
    (when (string= (or channel "#emacs") (buffer-name (current-buffer)))
      (switch-to-buffer (current-buffer))))

  (defun rgr/erc-quit()
    (when (get-buffer "irc.freenode.net:6667")
      (progn
        (switch-to-buffer(get-buffer "irc.freenode.net:6667"))
        (erc-cmd-QUIT "bye"))))

  (defun rgr/erc-start()
    (interactive)
    (unless(get-buffer "irc.freenode.net:6667")
      (progn
        (erc :server "irc.freenode.net"  :port 6667)
        (add-hook 'erc-join-hook 'rgr/erc-switch-to-channel))))

  (defun rgr/erc-format-nick (&optional user _channel-data)
    "Return the nickname of USER.
             See also `erc-format-nick-function'."
    ;; (when user (format "%-024s" (erc-server-user-nickname user))))
    (when user (format "%s" (erc-server-user-nickname user))))
  :hook
  (erc-mode . textReadFont))

(use-package slack :demand t
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (defhydra slack-hydra (:color gold :hint none)
    "
          team                       channel                   message              Entry
          -------------------------------------------------------------------------------------------------
          _c_: change current team   _s_: select channel       _i_: select im       _S_: start slack
          _r_: register team         _l_: channel list update  _u_: im list update  _C_: close connections
                                                                                    _q_: close hydra
          "
    ;; Team
    ("c" slack-change-current-team)
    ("r" slack-register-team)

    ;; Channel
    ("s" slack-channel-select)
    ("l" slack-channel-list-update)

    ;; Message
    ("i" slack-im-select)
    ("u" slack-im-list-update)

    ;; Entry
    ("S" slack-start)
    ("C" slack-ws-close)

    ("q"  nil                    "cancel"     :color orange))

  (slack-register-team
   :name "emacs-slack"
   :default t
   :token slack-api-token
   :subscribed-channels slack-subscribed-channels
   :full-and-display-names t)
  :bind
  ("C-c S" . slack-hydra/body))

(use-package gitter :demand d)


(defgroup rgr/chat-clients nil
  "Chat provides a standalone emacs instance with chat channels open"
  :group 'rgr)

(defcustom rgr/chat-functions nil
  "Start client functions"
  :group 'rgr/chat-clients
  :type '(repeat function))

(defcustom rgr/chat-close-functions nil
  "Close client functions"
  :group 'rgr/chat-clients
  :type '(repeat function))

(defun rgr/start-chats()
  (interactive)
  (dolist (fn rgr/chat-functions)
    (when (fboundp fn)
      (progn
        (message "Calling %s in start-chats" (symbol-name fn))
        (funcall fn))))
  (add-hook 'kill-emacs-hook #'rgr/close-chats))

(defun rgr/close-chats()
  (dolist (fn rgr/chat-close-functions)
    (when (fboundp fn)
      (progn
        (message "Calling %s in close-chats" (symbol-name fn))
        (funcall fn)))))

(recentf-mode -1)
(save-place-mode -1)
(savehist-mode -1)

(global-unset-key (kbd "C-c i"))
(global-set-key (kbd "C-c i") 'rgr/start-chats)

(when(yes-or-no-p "start chats?")
  (rgr/start-chats))

(provide 'rgr-chat)
```


### Slackware

1.  Emacs Slack

    [Slack](https://slack.com/intl/en-de/) interface for Emacs on [github](https://github.com/yuya373/emacs-slack). See [rgr-chat.el](./lisp/rgr-chat.el).

2.  Emacs Gitter

    [Gitter](https://gitter.im/) interface for Emacs on [github](https://github.com/xuchunyang/gitter.el). See [rgr-chat.el](./lisp/rgr-chat.el).


## Email, gmail, Gnus, mu4e


### gnus

```emacs-lisp
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
```


### [mu4e](https://www.emacswiki.org/emacs/mu4e) email client

1.  tasks

    1.  check straight releases to get the pre-build command so we dont have to do it manually.

2.  config

    uses mu4e-contrib to provide mark all unread as read. Using [pandoc](https://www.reddit.com/r/emacs/comments/8q84dl/tip_how_to_easily_manage_your_emails_with_mu4e/e0hrbfg?utm_source=share&utm_medium=web2x&context=3) for htlm viewing.

    ```emacs-lisp
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
    ```


### isync/mbsync

The [isync](https://wiki.archlinux.org/index.php/Isync) package provides the imap - maildir sync app mbsync.

1.  ~/.mbsyncrc

    mbsync gmail config file (maintained in linux-init-files repo)

    ```conf
    # Maintained in linux-init-files.org
    Create  Both
    Expunge Both
    SyncState *

    IMAPAccount gmx
    Host imap.gmx.com
    User rileyrg@gmx.de
    PassCmd "pass Email/gmx/apps/mbsync"
    SSLType IMAPS
    CertificateFile /etc/ssl/certs/ca-certificates.crt
    PipelineDepth 1

    IMAPStore gmx-remote
    Account gmx

    MaildirStore gmx-local
    Path ~/Maildir/gmx/
    Inbox ~/Maildir/gmx/INBOX
    SubFolders Legacy

    Channel gmx-inbox
    Master :gmx-remote:"INBOX"
    Slave :gmx-local:"INBOX"

    Channel gmx-sent
    Master :gmx-remote:"Gesendet"
    Slave :gmx-local:"Sent"

    Channel gmx-learning
    Master :gmx-remote:"Learning"
    Slave :gmx-local:"Learning"

    Channel gmx-drafts
    Master :gmx-remote:"Entw&APw-rfe"
    Slave :gmx-local:"Drafts"

    Channel gmx-bin
    Master :gmx-remote:"Gel&APY-scht"
    Slave :gmx-local:"Bin"

    Channel gmx-spam
    Master :gmx-remote:"Spamverdacht"
    Slave :gmx-local:"Spam"

    Channel gmx-archive
    Master :gmx-remote:"Archiv"
    Slave :gmx-local:"Archive"

    Group gmx
    Channel gmx-inbox
    Channel gmx-sent
    Channel gmx-drafts
    Channel gmx-bin
    Channel gmx-spam
    Channel gmx-archive

    Group gmx-special-interest
    Channel gmx-learning

    IMAPAccount gmail
    Host imap.gmail.com
    User rileyrg@gmail.com
    PassCmd "pass Email/gmail/apps/mbsync"
    SSLType IMAPS
    CertificateFile /etc/ssl/certs/ca-certificates.crt
    PipelineDepth 32

    IMAPStore gmail-remote
    Account gmail

    MaildirStore gmail-local
    Path ~/Maildir/gmail/
    Inbox ~/Maildir/gmail/INBOX
    SubFolders Legacy

    Channel gmail-inbox
    Master :gmail-remote:"INBOX"
    Slave :gmail-local:"INBOX"

    Channel gmail-sent
    Master :gmail-remote:"[Google Mail]/Sent Mail"
    Slave :gmail-local:"Sent"

    Channel gmail-drafts
    Master :gmail-remote:"[Google Mail]/Drafts"
    Slave :gmail-local:"Drafts"

    Channel gmail-bin
    Master :gmail-remote:"[Google Mail]/Bin"
    Slave :gmail-local:"Bin"

    Channel gmail-spam
    Master :gmail-remote:"[Google Mail]/Spam"
    Slave :gmail-local:"Spam"

    Channel gmail-archive
    Master :gmail-remote:"[Google Mail]/All Mail"
    Slave :gmail-local:"Archive"

    Channel gmail-gmx-archive
    Master :gmail-remote:"[Google Mail]/All Mail"
    Slave :gmx-local:"gmail/Archive"

    Group gmail
    Channel gmail-inbox
    Channel gmail-sent
    Channel gmail-drafts
    Channel gmail-bin
    Channel gmail-spam
    Channel gmail-archive

    Group gmail-gmx
    Channel gmail-gmx-archive
    ```

2.  sync and index

    and now we fetch our imap stuff into maildir and index it. The [Debian Wiki](https://wiki.debian.org/systemd/Services) documents how to use [systemd services](https://wiki.debian.org/systemd/Services) to and the [arch wiki](https://wiki.archlinux.org/) gives some concrete examples of how to use [mbsync user services](https://wiki.archlinux.org/index.php/Isync#With_a_timer) to keep you maildir upto date.

    ```bash
    cd ~
    mkdir -p ~/Maildir/gmail
    mbsync personal
    mu init --maildir=~/Maildir/gmail --rgr/address="$USEREMAIL"
    mu index
    ```

    1.  mu4e considerations using mbsync service

        Note that if using mu4e (which we are here ;)) then

        ```emacs-lisp
        (setq mu4e-get-mail-command "true")
        ```

        so that mu4e simply re-reads pre-indexed maildir as opposed fetching itself. (getsync got the mail).


## Screen recording


### camcorder

do screen recording directly from emacs

```emacs-lisp
(use-package camcorder
  :custom
  (camcorder-output-directory  "~/tmp"))
```


### Emacs screencasts

Package [keycast](https://github.com/tarsius/keycast) shows the keys pressed

```emacs-lisp
(use-package keycast
  )

```


## Pomodoro

[Pomidor](https://github.com/TatriX/pomidor) is a simple and cool pomodoro technique timer.

```emacs-lisp
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
```


## Programming Language related


### prog-mode hack

```emacs-lisp
(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))
```


### linum-mode Show Line numbers

:CUSTOM\_ID: Programming\_related-General-ac9e6746

```emacs-lisp
(global-set-key (kbd "S-<f2>") 'linum-mode)
(add-hook 'prog-mode-hook (lambda() (linum-mode t)))
```


### smartparens

```emacs-lisp
(use-package
  smartparens
  :disabled t
  :commands (smartparens-mode)
  :config (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  (sp-local-tag '(mhtml-mode html-mode) "b" "<span class=\"bold\">" "</span>")
  (smartparens-global-mode t))
```


### rainbow delimiters

```emacs-lisp
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

```


### Projectile

[Projectile](https://github.com/bbatsov/projectile) is all about being "project aware". Find files, grep and similar are aware of your [project root](https://projectile.readthedocs.io/en/latest/configuration/) making such tasks project local. Can't do without it.

```emacs-lisp
(use-package projectile
  :custom
  (projectile-completion-system 'default)
  :config
  (projectile-mode +1)
  :bind ("<f2>" . 'projectile-dired)
  ("<f12>" . projectile-run-eshell)
  ("M-<RET>" . projectile-run-eshell)
  (:map projectile-mode-map ( "C-c p" . projectile-command-map)))
```


### BASH     :bash:

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


### JSON, YAML Configuration files

1.  JSON Editing

    JSON editing using [json-mode](https://github.com/joshwnj/json-mode)

    ```emacs-lisp
    (use-package json-mode)
    ```

2.  YAML

    1.  Modes

        ```emacs-lisp
        (use-package
          yaml-mode
          :config
          (add-to-list 'auto-mode-alist '("\\.yml\\.yaml\\'" . yaml-mode))
          )
        ```


### Flycheck

On the fly [syntax checking](https://github.com/flycheck/flycheck) for GNU Emacs

```emacs-lisp
(use-package
  flycheck
  :config (use-package
            flycheck-pos-tip)
  (flycheck-pos-tip-mode)
  (global-flycheck-mode))
```


### Version Control

1.  It's [Magit](https://github.com/magit/magit)! A Git porcelain inside Emacs

    :CUSTOM\_ID: magit

    ```emacs-lisp
    ;; (use-package
    ;;   diff-hl
    ;;   :init (global-diff-hl-mode 1))

    (use-package
      magit
      :config
      (add-hook 'magit-post-commit-hook 'magit-mode-bury-buffer)
      :bind* ("C-x g" . magit-status))
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
      :after magit)
    ```

4.  [Orgit](https://github.com/magit/orgit) allows us to link to Magit buffers from Org documents

    ```emacs-lisp
    (use-package orgit
      :after magit
      :demand t)
    ```

5.  Git Gutter Mode

    [git-gutter.el](https://github.com/emacsorphanage/git-gutter) is an Emacs port of the Sublime Text plugin GitGutter.

    ```emacs-lisp
    (use-package git-gutter
      :demand t
      :config
      (global-git-gutter-mode +1)
      :bind
      ("C-x v ="  . git-gutter:popup-hunk))
    ```


### Javascript     :js:

```emacs-lisp

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
```


### RJSX

[rjsx-mode](https://github.com/felipeochoa/rjsx-mode) extends js2-mode to include jsx parsing.

```emacs-lisp
(use-package rjsx-mode
  :disabled t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
  )
```


### Typescript

```emacs-lisp
(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-mode))
  (defun rgr/ts-mode-hook ()
    (setq-local dash-docs-docsets '("React" "JavaScript")))
  (add-hook 'typescript-mode-hook 'rgr/ts-mode-hook))
```


### Tide Mode

```emacs-lisp
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
```


### Language Server Protocol (LSP)     :lsp:

[Emacs-lsp](https://github.com/emacs-lsp) : Language Server Protocol client for Emacs

Raw: [rgr/lsp](etc/elisp/rgr-lsp.el)

```emacs-lisp
(require 'rgr/lsp "rgr-lsp" 'NOERROR)
```

1.  library

    ```emacs-lisp
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
        ;; (use-package lsp-treemacs
        ;;   :config
        ;;   (lsp-treemacs-sync-mode 1))
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

        (defun rgr/lsp-ui-doc-glance (&optional w)
          "Trigger display hover information popup and hide it on next typing."
          (interactive)
          (lsp-describe-thing-at-point)
          ;; (message "lsp-ui-doc--displayed:%s" lsp-ui-doc--displayed)
          )
        (defun rgr/lsp-ui-mode-hook()
          (lsp-ui-sideline-mode -1)
          (lsp-ui-doc-mode -1)
          )

        (defun rgr/lsp-ui-imenu-view()
          (interactive)
          (lsp-ui-imenu--view)
          )


        :bind ((:map lsp-ui-mode-map
                     ;;("C-q"   . lsp-ui-doc-show)
                     ("C-S-<f10>" . lsp-ui-imenu)
                     ("C-<f8>" . rgr/dap-debug)
                     ("C-c S"   . toggle-lsp-ui-sideline)
                     ("C-c D"   . toggle-lsp-ui-doc))
               (:map lsp-ui-imenu-mode-map
                     ("<RET>" . rgr/lsp-ui-imenu-view)
                     ))

        :hook ((lsp-ui-mode . rgr/lsp-ui-mode-hook)))

      (use-package dap-mode
        :demand t
        :commands (rgr/dap-debug )
        :config
        (setq dap-ui-buffer-configurations
              `((,"*dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.50))) ;; changed this to 0.50
                (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.50)))
                (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.50)))
                (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
                (,"*debug-window*" . ((side . bottom) (slot . 3) (window-width . 0.20)))))


        (setq dap-auto-configure-features '(locals  tooltip))
        (require 'dap-gdb-lldb)
        (dap-gdb-lldb-setup)
        ;; (require 'dap-codelldb)
        ;;      (dap-codelldb-setup)
        (require 'dap-cpptools)
        (dap-cpptools-setup)
        ;; (add-hook 'dap-stopped-hook (lambda (arg)
        ;;                               (call-interactively #'dap-hydra)))

        (require 'dap-chrome)

        ;; (dap-register-debug-template "Chrome Browse rgr/project"
        ;;   (list :type "chrome"
        ;;         :cwd nil
        ;;         :mode "url"
        ;;         :request "launch"
        ;;         :webRoot "/home/rgr/Dropbox/homefiles/development/projects/react/rgr/app/"
        ;;         :url "http://localhost:3000"
        ;;         :sourceMap "true"
        ;;         :name "Chrome Browse rgr/project"))



        (defun rgr/dap-debug()
          (interactive)
          (if current-prefix-arg
              (call-interactively 'dap-debug)
            (dap-debug-last)))
        ;;    (dap-hydra))

        (defun rgr/lsp-mode-hook()
          (lsp-enable-which-key-integration))

        :bind (:map dap-mode-map
                    ("<f8>" . dap-continue)
                    ("C-S-<f8>" . dap-delete-session)
                    ("<f9>" . dap-hydra)
                    ("<f10>" . dap-next)
                    ("<f11>" . dap-step-in)
                    ("S-<f11>" . dap-step-out)
                    ))
      :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
             ((c-mode c++-mode js-mode php-mode gdscript-mode). lsp)
             ;; if you want which-key integration
             (lsp-mode . rgr/lsp-mode-hook)))

    (provide 'rgr/lsp)
    ```

    1.  [.dir-local.el](file:///home/rgr/development/thirdparty/godot/bin) config for a debug template

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


### Serial Port

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


### PlatformIO

[platformio-mode](https://github.com/emacsmirror/platformio-mode) is an Emacs minor mode which allows quick building and uploading of PlatformIO projects with a few short key sequences. The build and install process id documented [here](https://docs.platformio.org/en/latest/ide/emacs.html).

```emacs-lisp
(straight-use-package 'platformio-mode)
```


### Python     :python:

1.  General

    ```emacs-lisp
    (use-package python-mode
      :init
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
      )
    ```

2.  Elpy, the Emacs Python IDE     :elpy:

    ```emacs-lisp
    (use-package elpy
      :ensure t
      :defer t
      :custom
      (elpy-rpc-virtualenv-path 'current)
      :init
      (advice-add 'python-mode :before 'elpy-enable))
    ```

3.  virtualenvs

    1.  auto-virtualenv

        ```emacs-lisp
        (use-package  auto-virtualenv
          :init
          (add-hook 'python-mode-hook #'auto-virtualenv-set-virtualenv))
        ```

    2.  pyenv in a python project

        1.  creating the env in a project directory

            note standard is to call project local .venv and that can be detected by the likes of [auto-virtualenvwrapper](#config-Programming_Language_related-Python-virtualenvs-auto-virtualenvwrapper-bd500e27)

            ```bash
            python3 -m venv .venv
            ```


### C     :c:

1.  asm

    ```emacs-lisp
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
    ```

2.  Clang provides us with some industry standard code prettiers     :clang:

    ```emacs-lisp
    (straight-use-package 'clang-format)
    (setq clang-format-style-option "llvm")
    (fset 'c-indent-region 'clang-format-region)
    (fset 'c-indent-buffer 'clang-format-buffer)
    ```

3.  C  modes hooks

    ```emacs-lisp
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

    ```

    1.  look into rgr/c-setup and platformio code :tangle no


### Linux tools

1.  strace - highlight strace output

    ```emacs-lisp
    (use-package x86-lookup
      :custom
      ( x86-lookup-pdf  (expand-file-name "pdf/intel-x86.pdf" user-emacs-directory))
      )
    ```

2.  [logview](https://github.com/doublep/logview) - view system logfiles

    ```emacs-lisp
    (use-package logview
      :demand t
      :init
      (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
      (add-to-list 'auto-mode-alist '("log\\'" . logview-mode)))
    ```


### Assembler

1.  [x86Lookup](https://nullprogram.com/blog/2015/11/21/)

    ```emacs-lisp
    (use-package strace-mode)
    ```


### C++     :cpp:

```emacs-lisp
(defun rgr/c++-mode-hook ()
  (setq-local dash-docs-docsets '("C++")))
(add-hook 'c++-mode-hook 'rgr/c++-mode-hook)
```


### C#     :c#:

1.  Loading CSharp support

    1.  config

        ```emacs-lisp
        (use-package csharp-mode)
        (use-package omnisharp)
        ```


### Godot GDScript     :godot:

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

1.  19:03 godot scripts     :godot:

    <rgr\_> terminaolgy (and tools) : you run/play a scene which can be associated with scripts. Do you ever in the IDE run a script directly? <GodotDiscord> <Calinou> you can, using an option in the script editor menu <GodotDiscord> <Calinou> it's not very common though <GodotDiscord> <Calinou> <https://docs.godotengine.org/en/latest/tutorials/misc/running_code_in_the_editor.html> <GodotDiscord> <Calinou> see also the EditorScript class, which you need to extend from to use that feature: <https://docs.godotengine.org/en/latest/classes/class_editorscript.html> <rgr\_> thank you <rgr\_> now a niave Q as I dont have mono install installed. is the C# stuff also "script" and the files are "scripts"? <GodotDiscord> <Calinou> yes, "script" is a generic term <rgr\_> and can be run the same way during development?


### Web,Symfony and Twig

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


### elf-mode - view the symbol list in a binary

[https://oremacs.com/2016/08/28/elf-mode/](https://oremacs.com/2016/08/28/elf-mode/)

```emacs-lisp
(use-package elf-mode
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(?:a\\|so\\)\\'" . elf-mode)))
```


## Startup

```emacs-lisp
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
```


## Themes


### modus themes

<https://github.com/protesilaos/modus-themes>

```emacs-lisp
(use-package modus-themes
  :demand
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))  ;; (modus-themes-load-operandi))
```


## Late load

```emacs-lisp
(load-el-gpg (no-littering-expand-etc-file-name "late-load"))
```


# Associated emacs things


## Project version control, git, .gitignore


### .gitignore, exclusive

I like to exclude everything and then add in what is important. So the first line of my [gitignore](.gitignore) is "\*".

```git
*
*.*

!.projectile

!.gitignore
!.ignore

!emacs-config.org
!init.el
!README.md

!info
!info/*

!etc
!var

!etc/custom.el
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

!etc/straight
!etc/straight/versions
!etc/straight/versions/default.el


!var/lessons
!var/lessons/*.org

!var/lessons/bash-lessons
!var/lessons/bash-lessons/*.org

!var/lessons/python-lessons
!var/lessons/python-lessons/*.org

!editor-config
!editor-config/*

!bin
!bin/*

!exwm
!exwm/*


!images
!images/*

!straight
!straight/versions
!straight/versions/*

!linux-init
!linux-init/*
!linux-init/DotFiles/
!linux-init/DotFiles/*

!linux-init/test-files/
!linux-init/test-files/*
!linux-init/test-files/subdir
!linux-init/test-files/subdir/*


!build
!build/scripts
!build/scripts/*
```


### master branch, no commit

```bash
#!/bin/sh
branch="$(git rev-parse --abbrev-ref HEAD)"
if [ "$branch" = "master" ]; then
    echo "You can't commit directly to master branch"
    exit 1
fi
```


## Setting up emacs as a default editor using a dot desktop file and associated protocol handler


### [php.ini](editor-config/php.ini) changes e.g /etc/php/7.3/php.ini

`xdebug.file_link_format` is used by compliant apps to format a protocol uri. This is handled on my Linux system as a result of [emacsclient.desktop](editor-config/emacsclient.desktop) documented below.

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


### Gnome protocol handler desktop file

Copy [emacsclient.desktop](editor-config/emacsclient.desktop) to ~/.local/share/applications (Debian & Gnome - your mileage may vary&#x2026;)

```shell
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
    pop-window "Emacs:"
fi
```
