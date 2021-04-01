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
