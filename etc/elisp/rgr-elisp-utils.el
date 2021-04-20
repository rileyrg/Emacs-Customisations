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

(use-package eldoc-box)

(use-package emacs
  :init
  (defcustom rgr/elisp-context--delay 2.5 "How long to delay before context display" :type 'float)
  (defcustom rgr/elisp-context--display-func 'rgr/elisp-context--display-posframe "The function called by `rgr/elisp-context-mode' timer `rgr/elisp-context-timer-func'." :type 'function)
  (defcustom rgr/elisp-context--posframe-buffer "*rgr/elisp-context**" "Buffer name for posframe elisp context help")
  (defvar rgr/elisp-context--timer nil  "Store the `rgr/elisp-context-mode' timer")

  :config
  (use-package posframe)

  (defun rgr/elisp-context--hide()
    "Hide the `rgr/elisp-context--posframe-buffer'"
    (posframe-hide rgr/elisp-context--posframe-buffer))

  (defun rgr/elisp-context--docstring(sym)
    "Return the docstring attached to the symbol SYM"
    (if (or (fboundp sym) (boundp sym))
        (let ((help-xref-following t))
          (save-window-excursion
            (with-temp-buffer
              (help-mode)
              (describe-symbol sym)
              (buffer-string))))
      nil))

  (defun rgr/elisp-context--display-posframe()
    "Show the docstring for the symbol at point in a posframe"
    (interactive)
    (let*((p (point))
          (sym (symbol-at-point))
          (docstring (if sym (rgr/elisp-context--docstring sym) nil)))
      (if docstring
          (posframe-show rgr/elisp-context--posframe-buffer
                         :string docstring
                         :left-fringe 8
                         :right-fringe 8
                         :internal-border-width 4
                         :internal-border-color "gray"
                         :border-width 1
                         :border-color "orange"
                         :position p
                         )
        (rgr/elisp-context--hide))))

  (defun rgr/elisp-context--timer-func()
    "function called every `rgr/elisp-context--delay' seconds when `rgr/elisp-context-mode is non-nil.
It calls out to `rgr/elisp-context--display-func'."
    (when rgr/elisp-context-mode
      (funcall rgr/elisp-context--display-func)))

  (define-minor-mode rgr/elisp-context-mode
    "minor-mode to popup help for the elisp symbol at point."
    nil
    :lighter " elisp-context"
    (unless rgr/elisp-context--timer
      (add-hook 'post-command-hook 'rgr/elisp-context--hide)
      (setq  rgr/elisp-context--timer
             (run-with-idle-timer
              rgr/elisp-context--delay t
              'rgr/elisp-context--timer-func))))

  :hook
  (emacs-lisp-mode . (lambda()(rgr/elisp-context-mode +1)))

  :bind
  ("M-<f2>" . (lambda()(interactive)(rgr/elisp-context--posframe-display)))
  ("M-<f1>" . (lambda()(interactive)(rgr/elisp-context-mode 'toggle))))

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
