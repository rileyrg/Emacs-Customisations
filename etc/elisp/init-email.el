;; -*- lexical-binding: t; -*-
(emacs-alert "Starting Email - MU4E")
(use-package mu4e
  :ensure t
  ( :host github
                            :branch "release/1.10"
                            :repo "djcb/mu"
                            :files ("mu4e/*.el" "build/mu4e/mu4e-meta.el" "build/mu4e/mu4e-config.el" "build/mu4e/mu4e.info")
                            :main "mu4e/mu4e.el"
                            :pre-build (("./autogen.sh")
                                        ("ninja" "-C" "build")
                                        (make-symbolic-link (expand-file-name "./build/mu/mu")
                                                            (expand-file-name "~/bin/mu") 'ok-if-exists)))
  :commands (mu4e mu4e-update-index)
  :custom
  ( mayil-user-agent 'mu4e-user-agent )
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
  ( mu4e-mu-binary (expand-file-name "mu/build/mu/mu" elpaca-repos-directory) )
  ( mu4e-update-interval nil )
  ( mu4e-use-fancy-chars t )
  ( mu4e-view-prefer-html nil )
  ( mu4e-view-show-addresses t )
  ( smtpmail-smtp-service 587 )
  ( user-full-name "Richard G.Riley" )
  :config

  (defcustom email-gmail-email "foo@bar.bing" "gmail email address" :type '(string)
       :group 'rgr/email)
  (defcustom email-gmail-full-name "Mr Foo" "gmail full name" :type '(string)
       :group 'rgr/email)
  (defcustom email-gmail-signature "Bye" "gmail sign off signature" :type '(string)
       :group 'rgr/email)
  (defcustom email-gmx-email "wong@foo.bar" "gmx email address" :type '(string)
       :group 'rgr/email)
  (defcustom email-gmx-full-name "Mr Wong" "gmx full name" :type '(string)
       :group 'rgr/email)
  (defcustom email-gmx-signature "my most humble contrafibularities" "gmx sign off signature" :type '(string)
       :group 'rgr/email)
  
  (use-package mu4e-alert
    :init
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

  (use-package mu4e-column-faces
    :after mu4e
    :config (mu4e-column-faces-mode))

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "aGmx"
             :enter-func (lambda () (mu4e-message "gmx context")(mu4e-update-index))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/gmx" (mu4e-message-field msg :maildir))))
             :vars `( ( user-mail-address . ,email-gmx-email )
                      ( mu4e-get-mail-command . "getmails gmx gmx-special-interest")
                      ( user-full-name . ,email-gmx-full-name )
                      ( smtpmail-smtp-server . "smtp.gmail.com")
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
                        ,email-gmx-signature
                        )))
           ,(make-mu4e-context
             :name "bGmail"
             :enter-func (lambda () (mu4e-message "gmail context") (mu4e-update-index))
             ;; no leave-func
             ;; we match based on the maildir of the message
             ;; this matches maildir /Arkham and its sub-directories
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
             :vars `( ( user-mail-address . ,email-gmail-email  )
                      ( user-full-name . ,email-gmail-full-name )
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
                      ( mu4e-compose-signature . ,email-gmail-signature)))))

  (defun mu4e-smarter-compose ()
    "My settings for message composition."
    (set-fill-column 72)
    )

  (defun rgr/mu4e-default-context()
    (interactive)
    (mu4e)
    (mu4e-context-switch nil "bGmail"))

  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions
               '("XWidget View" . mu4e-action-view-with-xwidget) t)
  (add-to-list 'mu4e-view-actions
               '("Markall as read" . mu4e-headers-mark-all-unread-read) t)
  (require 'mu4e-contrib)
  :hook ((mu4e-view-rendered . visual-line-mode)
         (mu4e-compose-mode . mu4e-smarter-compose)
         (mu4e-view-rendered .
                             (lambda()
                               ;; try to emulate some of the eww key-bindings
                               (local-set-key (kbd "<tab>") 'shr-next-link)
                               (local-set-key (kbd "<backtab>") 'shr-previous-link))))
  :bind	  (("C-c u" .  rgr/mu4e-default-context)
           (:map mu4e-main-mode-map
                 ("m" . mu4e-compose-new))
           (:map mu4e-main-mode-map
                 ("u" . mu4e-update-index))
           (:map mu4e-headers-mode-map
                 ("v" . mu4e-view-action)
                 ("C-c u" . mu4e-headers-mark-all-unread-read))))
;;:map mu4e-view-mode-map
;;   ("V" . '(lambda()(message "%s" (mu4e-message-at-point))))))) ;; mu4e-action-view-in-browser))))
(add-hook 'server-after-make-frame-hook (lambda()(run-with-idle-timer 1 nil  (lambda()(rgr/mu4e-default-context)))))
