(use-package emacs
  :hook
  ((emacs-lisp-mode . electric-pair-mode)))

(defun rgr/elisp-edit-mode()
  "return non nil if this buffer edits elisp"
  (member major-mode '(emacs-lisp-mode lisp-interaction-mode)))

(use-package package-lint)

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

(use-package el-docstring-sap
  :straight (el-docstring-sap :local-repo "~/development/projects/emacs/el-docstring-sap" :type git :host github :repo "rileyrg/el-docstring-sap" )
  :hook
  (emacs-lisp-mode . el-docstring-sap-mode)
  :bind
  ("M-<f2>" . el-docstring-sap--display)
  ("M-<f1>" . el-docstring-sap-mode))

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

(provide 'rgr/elisp-utils)
