(defcustom rgr/emacs-source (no-littering-expand-var-file-name "emacs-source/current") "where the source is for the current emacs")
(setq source-directory rgr/emacs-source)

(defun rgr/elisp-edit-mode()
  "return non nil if this buffer edits elisp"
  (member major-mode '(emacs-lisp-mode lisp-interaction-mode)))

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

(defun rgr/elisp-write-var (f v)
  (with-temp-file f
    (prin1 v (current-buffer))))

(defun rgr/elisp-read-var (f)
  (with-temp-buffer
    (insert-file-contents f)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

(define-minor-mode rgr/contextual-help-mode
  "Show help for the elisp symbol at point in the current *Help* buffer.

Advises `eldoc-print-current-symbol-info'."
  :lighter " eldoc-ctx"
  :global t
  (require 'help-fns)
  (when (eq this-command 'rgr/contextual-help-mode)
    (message "Contextual help is %s" (if rgr/contextual-help-mode "on" "off")))
  (when rgr/contextual-help-mode
    (eldoc-mode 1)
    (rgr/contextual-help :force)))

(defadvice eldoc-print-current-symbol-info (before rgr/contextual-help activate)
  "Triggers contextual elisp *Help*. Enabled by `rgr/contextual-help-mode'."
  (and rgr/contextual-help-mode t
       ;;(derived-mode-p 'emacs-lisp-mode)
       (rgr/contextual-help)))

(defvar-local rgr/contextual-help-last-symbol nil
  ;; Using a buffer-local variable for this means that we can't
  ;; trigger changes to the help buffer simply by switching windows,
  ;; which seems generally preferable to the alternative.
  "The last symbol processed by `rgr/contextual-help' in this buffer.")

(defun rgr/help-visible-p ()
   (or (get-buffer-window (help-buffer)) (seq-some (lambda (buf) (and (get-buffer-window buf 0) (eq (buffer-local-value 'major-mode buf) 'helpful-mode) buf)) (buffer-list))))

(defun rgr/contextual-help (&optional force)
  "Describe function, variable, or face at point"

  (let ((help-visible-p (or force (rgr/help-visible-p)))
        (sym (symbol-at-point)))
    ;; We ignore keyword symbols, as their help is redundant.
    ;; If something else changes the help buffer contents, ensure we
    ;; don't immediately revert back to the current symbol's help.
    (when (and sym help-visible-p)
      (and (not (eq sym rgr/contextual-help-last-symbol))
           (not (keywordp sym))
           (setq rgr/contextual-help-last-symbol sym)
           (save-selected-window
             (if (fboundp 'helpful-symbol)
                   (helpful-symbol sym)
               (describe-symbol sym)))))))

(defun rgr/contextual-help-toggle ()
  "Intelligently enable or disable `rgr/contextual-help-mode'."
  (interactive)
  (rgr/contextual-help-mode 'toggle))

(rgr/contextual-help-mode +1)

(global-set-key (kbd "M-<f1>") #'rgr/contextual-help-toggle)

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
  auto-compile
  :demand
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

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
