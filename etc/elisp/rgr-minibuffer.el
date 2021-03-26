;;(require 'tramp)
(use-package tramp
  :custom
  (tramp-default-method "ssh")
  )

(use-package fzf)

(defun maybe-read-only-mode()
  (when (cond ((eq major-mode 'org-mode) t))
    (message "Setting readonly mode for %s buffer" major-mode)
    (read-only-mode +1)))
                                        ;(add-hook 'find-file-hook 'maybe-read-only-mode)

(use-package sudo-edit)

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

(use-package selectrum
  :config
  (selectrum-mode +1)
  :bind ("C-x C-z" . #'selectrum-repeat))

(use-package prescient
  :config
  (prescient-persist-mode +1)
  (if (featurep 'selectrum)
      (use-package selectrum-prescient
        :config
        (selectrum-prescient-mode +1))))

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

(use-package embark
  :ensure t
  :bind
  (("C-S-a" . embark-act)       ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode)
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

(provide 'rgr/minibuffer)
