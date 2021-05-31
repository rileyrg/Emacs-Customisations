;; if you want to change prefix for lsp-mode keybindings.
(use-package lsp-mode
  :custom
  (lsp-diagnostic-package :none)
  (lsp-auto-configure t)
  :config
  (use-package
    lsp-ui
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
      (lsp-ui-sideline-mode +1)
      (lsp-ui-doc-mode +1)
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
         ((c-mode c++-mode js-mode php-mode gdscript-mode). lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . rgr/lsp-mode-hook)))

(provide 'rgr/lsp)
