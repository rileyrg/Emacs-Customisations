(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'bully)
 '(TeX-macro-global
   '("~/texlive/2013" "~/texlive/2013/texmf-dist/bibtex/bst/" "~/texlive/2013/texmf-dist/tex/" "~/texlive/texmf-local/tex/"))
 '(ag-arguments '("--smart-case" "--stats" "--multiline" "-U"))
 '(ag-ignore-list '("value" "cache" "log" "data"))
 '(alert-default-style 'libnotify nil nil "Customized with use-package alert")
 '(alert-fade-time 15)
 '(apropos-do-all t)
 '(async-shell-command-buffer 'confirm-kill-process)
 '(auth-sources '("~/.gnupg/auth/authinfo.gpg" "~/.gnupg/auth/authirc.gpg"))
 '(auto-revert-check-vc-info t)
 '(auto-revert-remote-files t)
 '(backup-by-copying nil)
 '(backup-by-copying-when-linked t)
 '(backup-directory-alist '((".*" . "/tmp/")))
 '(bbdb-complete-mail-allow-cycling t)
 '(bbdb-complete-name-allow-cycling t t)
 '(bbdb-file "~/.bbdb" t)
 '(blink-cursor-interval 0.2)
 '(browse-url-browser-function 'browse-url-chrome)
 '(c-default-style
   '((c-mode . "k&r")
     (c++-mode . "k&r")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(calendar-mark-diary-entries-flag t)
 '(calendar-view-diary-initially-flag t)
 '(canlock-password "106923df36ef41eb263045249a447dac2d49649a")
 '(ccls-code-lens-position 'inplace)
 '(ccls-initialization-options '(request: (timeout:5000)))
 '(ccls-root-files '(".ccls-root" ".projectile"))
 '(ccls-sem-highlight-method 'overlay)
 '(centaur-tabs-cycle-scope 'tabs)
 '(clang-format-style "LLVM")
 '(company-box-enable-icon nil)
 '(company-c-headers-path-user
   '("~/.platformio/packages/framework-arduinoavr/cores/arduino/Arduino.h"))
 '(company-dabbrev-downcase nil)
 '(company-irony-ignore-case 'smart)
 '(company-lsp-cache-candidates 'auto)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-tooltip-limit 20)
 '(compilation-ask-about-save nil)
 '(compilation-error-regexp-alist
   '(bash-xset-x dotnet-warning dotnet-error msbuild-warning msbuild-error xbuild-warning xbuild-error dotnet-testfail typescript-nglint-warning typescript-nglint-error typescript-tslint typescript-tsc-pretty typescript-tsc absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek gradle-kotlin iar ibm irix java jikes-file maven jikes-line clang-include gcc-include ruby-Test::Unit gmake gnu lcc makepp mips-1 mips-2 omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line))
 '(compilation-message-face 'default)
 '(completion-styles '(orderless) nil nil "Customized with use-package orderless")
 '(confirm-nonexistent-file-or-buffer nil)
 '(create-lockfiles nil)
 '(ctrlf--restore-final-window-start-flag t)
 '(ctrlf-default-search-style 'fuzzy-regexp)
 '(cua-keep-region-after-copy nil)
 '(cursor-type 'box)
 '(custom-safe-themes
   '("8ca8fbaeaeff06ac803d7c42de1430b9765d22a439efc45b5ac572c2d9d09b16" "7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "972d69a06b8f6e5d43fb83ff59417511f09ba1a1783aab5e22c0e9cbd25ad458" "d234fa6da0282c254f00d534079374d1c6f3c3e600075bc65fe661fdd1947792" "7d6861c031212fca9b4a963ced6230be2aa3139570b85ea5e77619b1fd0351ad" "9b39b25c3a23b1be6e99a3648b91ebaf2a7efdde236e3472aa95f1708ec61d4f" "3f1dcd824a683e0ab194b3a1daac18a923eed4dba5269eecb050c718ab4d5a26" default))
 '(dap-default-terminal-kind "external")
 '(dap-external-terminal
   '("alacritty" "-t" "{display}" "-e" "zsh" "-c" "exec {command}"))
 '(dap-gdb-lldb-debug-program
   '("node" "/home/rgr/.vscode/extensions/webfreak.debug-0.25.0/out/src/lldb.js"))
 '(dap-gdb-lldb-path
   "/home/rgr/.vscode/extensions/webfreak.debug-0.25.0/vscode/webfreak.debug")
 '(dap-internal-terminal 'dap-internal-terminal-auto)
 '(dap-print-io t)
 '(dap-ui-controls-mode t nil (dap-ui))
 '(dap-utils-extension-path "/home/rgr/.vscode/extensions/webfreak.debug-0.25.0")
 '(delete-active-region 'kill)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(devdocs-alist
   '((c-mode . "c")
     (c++-mode . "c++")
     (clojure-mode . "clojure")
     (coffee-mode . "CoffeeScript")
     (common-lisp-mode . "lisp")
     (cperl-mode . "perl")
     (css-mode . "css")
     (elixir-mode . "elixir")
     (enh-ruby-mode . "ruby")
     (erlang-mode . "erlang")
     (gfm-mode . "markdown")
     (go-mode . "go")
     (groovy-mode . "groovy")
     (haskell-mode . "haskell")
     (html-mode . "html")
     (java-mode . "java")
     (js2-mode . "jquery")
     (js3-mode . "javascript")
     (less-css-mode . "less")
     (lua-mode . "lua")
     (markdown-mode . "markdown")
     (perl-mode . "perl")
     (php-mode . "php")
     (processing-mode . "processing")
     (puppet-mode . "puppet")
     (python-mode . "python")
     (ruby-mode . "ruby")
     (sass-mode . "sass")
     (scala-mode . "scala")
     (tcl-mode . "tcl")))
 '(diary-file "~/.diary")
 '(diary-list-include-blanks t)
 '(diary-mark-entries-hook '(diary-mark-included-diary-files))
 '(diary-number-of-entries 7)
 '(dictionary-server "localhost")
 '(diredp-hide-details-initially-flag nil)
 '(display-buffer-alist
   '(("\\*Symfony Web Server\\*.*" display-buffer-no-window)
     (popwin:display-buffer-condition popwin:display-buffer-action)))
 '(display-time-mode t)
 '(el-docstring-sap--display-func 'el-docstring-sap--quick-peek)
 '(el-docstring-sap--posframe-poshandler nil)
 '(eldoc-box-only-multi-line t)
 '(elpy-formatter 'autopep8)
 '(elscreen-display-tab nil)
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(enable-recursive-minibuffers t)
 '(epa-file-cache-passphrase-for-symmetric-encryption nil)
 '(erc-autojoin-channels-alist
   '(("irc.freenode.net" "#debian")
     ("irc.libera.chat" "#sway" "#emacs" "#linux")))
 '(erc-autojoin-delay 1)
 '(erc-autojoin-domain-only nil)
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing 'ident)
 '(erc-fill-column 128)
 '(erc-fill-function 'erc-fill-static)
 '(erc-fill-mode t)
 '(erc-fill-static-center 1)
 '(erc-header-line-format "")
 '(erc-hide-list '("353" "JOIN" "KICK" "NICK" "PART" "QUIT" "MODE"))
 '(erc-hide-timestamps nil)
 '(erc-input-line-position nil)
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-join-buffer 'bury)
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-match-mode t)
 '(erc-max-buffer-size 20000)
 '(erc-minibuffer-notice nil)
 '(erc-modules
   '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))
 '(erc-nick-uniquifier "_DUP")
 '(erc-notice-highlight-type 'prefix)
 '(erc-notifications-mode t)
 '(erc-pcomplete-mode t)
 '(erc-prompt "#>")
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-query-display 'window-noselect)
 '(erc-scrolltobottom-mode nil)
 '(erc-server-auto-reconnect t)
 '(erc-server-reconnect-attempts t)
 '(erc-stamp-mode t)
 '(erc-startup-file-list nil)
 '(erc-system-name nil)
 '(erc-timestamp-format "[%T]")
 '(erc-timestamp-format-right nil)
 '(erc-timestamp-only-if-changed-flag nil)
 '(erc-track-enable-keybindings nil)
 '(erc-track-exclude '("324" "332" "333" "353" "329"))
 '(erc-track-exclude-types
   '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477"))
 '(erc-truncate-buffer-on-save t)
 '(erc-try-new-nick-p t)
 '(erc-user-mode nil)
 '(erc-whowas-on-nosuchnick t)
 '(eshell-banner-message
   "
W.O.P.R Online...
Syncing. Please Wait.

Greetings Professor Falken.
Would you like to play a game?

")
 '(eshell-hist-ignoredups t)
 '(eshell-history-size nil)
 '(eval-expression-print-length nil)
 '(expand-region-smart-cursor t)
 '(f 'el-docstring-sap--display)
 '(fci-rule-color "#14151E" t)
 '(fill-column 256)
 '(gc-cons-threshold 100000000)
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(gdscript-docs-local-path
   "/home/rgr/Documents/Programming/Godot/www/docs.godotengine.org/en/stable")
 '(gdscript-docs-use-eww nil)
 '(geben-dbgp-default-port 8000)
 '(git-gutter:update-interval 2)
 '(google-translate-default-source-language "de")
 '(google-translate-default-target-language "en")
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "var" "cache"))
 '(gud-tooltip-modes
   '(gud-mode c-mode c++-mode fortran-mode python-mode emacs-lisp-mode))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(helpful-max-buffers nil)
 '(history-delete-duplicates t)
 '(ibuffer-deletion-face 'diredp-deletion-file-name)
 '(ibuffer-marked-face 'diredp-flag-mark)
 '(indent-tabs-mode nil)
 '(indium-chrome-executable "google-chrome")
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil)
 '(ispell-local-dictionary "british")
 '(ispell-program-name "/usr/bin/aspell")
 '(ispell-query-replace-choices t)
 '(ispell-silently-savep t)
 '(kept-new-versions 20)
 '(large-file-warning-threshold nil)
 '(org-M-RET-may-split-line nil nil nil "Customized with use-package org")
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)))
 '(org-agenda-skip-scheduled-if-deadline-is-shown t nil nil "Customized with use-package org")
 '(org-agenda-start-on-weekday 0 nil nil "Customized with use-package org")
 '(org-babel-load-languages '((emacs-lisp . t) (python . t) (shell . t) (css . t)))
 '(org-capture-templates
   '(("t" "Todo" entry
      (file+headline "refile.org" "Tasks To Refile")
      "* TODO %?
:PROPERTIES:
:DateCreated: %T
:END:
")
     ("n" "Quick note to refile later" entry
      (file+headline "refile.org" "Notes To Refile")
      "* %?
        :PROPERTIES:
        :DateCreated: %T
        :END:
%i
%a")
     ("k" "keep" entry
      (file "keep.org")
      "* %? :crypt:
        :PROPERTIES:
        :DateCreated: %T
        :END:
%i
%a")
     ("j" "journal" entry
      (file+olp+datetree "journal.org")
      "* %^{Title}
        :PROPERTIES:
        :DateCreated: %T
        :END:" :empty-lines 1)
     ("o" "Orders" entry
      (file "~/.emacs.d/var/org/orgfiles/orders.org")
      "* WAITING %^{Order name} :order:
SCHEDULED: %^{Delivery Date}t
:PROPERTIES:
:DateCreated: %T
:END:")))
 '(org-catch-invisible-edits 'error nil nil "Customized with use-package org")
 '(org-clock-into-drawer t nil nil "Customized with use-package org")
 '(org-clock-persist 'history nil nil "Customized with use-package org")
 '(org-clock-task-overrun-text "*LATE*")
 '(org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
 '(org-confirm-babel-evaluate nil nil nil "Customized with use-package org")
 '(org-crypt-key "rileyrg")
 '(org-ctrl-k-protect-subtree t nil nil "Customized with use-package org")
 '(org-default-notes-file "refile.org" nil nil "Customized with use-package org")
 '(org-directory "/home/rgr/.emacs.d/var/org/orgfiles" nil nil "Customized with use-package org")
 '(org-enforce-todo-dependencies t nil nil "Customized with use-package org")
 '(org-goto-interface 'outline-path-completion)
 '(org-goto-max-level 8)
 '(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
 '(org-log-done 'time nil nil "Customized with use-package org")
 '(org-log-into-drawer t nil nil "Customized with use-package org")
 '(org-modules
   '(org-crypt ol-docview ol-eww ol-info ol-irc ol-mhe ol-rmail ol-w3m ol-eshell ol-bookmark ol-elisp-symbol ol-man))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes 'confirm nil nil "Customized with use-package org")
 '(org-refile-targets
   '((org-agenda-files :tag . "refile")
     (org-agenda-files :maxlevel . 16)) nil nil "Customized with use-package org")
 '(org-refile-use-outline-path 'file nil nil "Customized with use-package org")
 '(org-reverse-note-order t nil nil "Customized with use-package org")
 '(org-src-window-setup 'current-window)
 '(org-structure-template-alist
   '(("py" . "src python")
     ("el" . "src emacs-lisp")
     ("sh" . "src shell")
     ("ba" . "src bash")
     ("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")))
 '(org-tag-alist
   '(("visa" . 118)
     ("bike" . 98)
     (:startgroup)
     ("@shamrock" . 115)
     ("@home" . 104)
     ("@dublin" . 100)
     (:endgroup)
     (:startgroup)
     ("online" . 111)
     ("offline" . 79)
     (:endgroup)
     (:startgroup)
     ("business" . 98)
     ("personal" . 112)
     (:endgroup)
     ("shopping" . 115)
     ("@caravan" . 99)
     ("emacs" . 101)
     ("vocab" . 118)
     ("programming" . 99)
     ("webs" . 119)
     ("idea" . 105)
     ("fitness" . 102)
     ("electronics" . 69)
     ("gardening" . 103)))
 '(org-tag-persistent-alist '(("noexport" . 110) ("trash" . 116) ("refile" . 114)) nil nil "Customized with use-package org")
 '(org-tags-exclude-from-inheritance '("PROJECT" "DEFAULTCLOCKTASK" "crypt") nil nil "Customized with use-package org")
 '(org-todo-keywords
   '((sequence "TODO(t!)" "STARTED(s!)" "|" "DONE(d!/!)")
     (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
     (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")))
 '(org-todo-state-tags-triggers
   '(("CANCELLED"
      ("CANCELLED" . t))
     ("WAITING"
      ("WAITING" . t)
      ("NEXT"))
     ("SOMEDAY"
      ("WAITING" . t))
     (done
      ("NEXT")
      ("WAITING"))
     ("TODO"
      ("WAITING")
      ("CANCELLED")
      ("NEXT"))
     ("STARTED"
      ("WAITING"))
     ("DONE"
      ("WAITING")
      ("CANCELLED")
      ("NEXT"))))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(php-manual-url "http://www.php.net")
 '(php-mode-speedbar-open nil)
 '(php-search-url "http://www.php.net/manual-lookup.php?pattern=")
 '(pomodoro-desktop-notification t)
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(ps-font-size '(12 . 12))
 '(ps-lpr-switches '("-o Duplex=DuplexNoTumble"))
 '(ps-paper-type 'a4)
 '(ps-spool-duplex t)
 '(python-indent-guess-indent-offset-verbose nil)
 '(rainbow-ansi-colors-major-mode-list '(sh-mode c-mode c++-mode php-mode emacs-lisp-mode))
 '(read-process-output-max 1048576 t)
 '(read-quoted-char-radix 16)
 '(rebox-style-loop '(27 25 21) t)
 '(recentf-exclude
   '("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'" "orgfiles/.*" "\\.md$" "recentf$" "config\\.el$" "bookmarks$" "history" "var/.*"))
 '(rgr/alert-learn-period 300)
 '(rgr/chat-close-functions '(slack-ws-close rgr/erc-quit))
 '(rgr/chat-functions '(rgr/erc-start))
 '(rgr/serialIOPort "/dev/ttyACM0")
 '(ring-bell-function 'ignore)
 '(rtags-completions-enabled t)
 '(rtags-periodic-reparse-timeout 5.0)
 '(safe-local-variable-values
   '((enable-local-variables: . all)
     (eval add-hook 'after-save-hook 'org-md-export-to-markdown nil t)
     (progn
       (make-local-variable 'process-environment)
       (setq process-environment
             (copy-sequence process-environment))
       (setenv "ARDUINO_SDK_PATH" "~/development/arduino/arduinoSDK"))
     (projectile-project-compilation-cmd . "cmake .. && make Blink_flash")
     (progn
       (make-local-variable 'process-environment)
       (setq process-environment
             (copy-sequence process-environment))
       (setenv "ARDUINO_SDK_PATH " "~/development/thirdparty/arduinoSDK"))
     (projectile-project-compilation-cmd . "cmake .. && make DigitalInputs_flash")
     (projectile-project-compilation-dir . "build")))
 '(save-interprogram-paste-before-kill t)
 '(savehist-additional-variables '(global-mark-ring kill-ring))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-step 1)
 '(send-mail-function 'smtpmail-send-it)
 '(set-mark-command-repeat-pop t)
 '(shell-switcher-ask-before-creating-new t)
 '(show-paren-delay 0.3)
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(sql-input-ring-file-name "~/.mysql_history")
 '(sql-port 5432)
 '(symfony-server-command "symfony serve")
 '(synonyms-cache-file "~/.config/emacs/syn.cache")
 '(synonyms-file "~/.config/emacs/mthesaur.txt")
 '(synosaurus-backend 'synosaurus-backend-openthesaurus)
 '(synosaurus-choose-method 'popup)
 '(tab-always-indent 'complete)
 '(tags-revert-without-query t)
 '(tags-table-list '("/home/rgr/Dropbox/homefiles/dot-config/emacs"))
 '(thesaurus-bhl-api-key "140ced1a60f9c434bb1dbc6831d2f6e9" t)
 '(twittering-connection-type-order '(wget curl urllib-http native urllib-https))
 '(twittering-mode-hook nil)
 '(twittering-status-format
   "%RT{%FACE[bold]{RT}}%i %S(%s),  %@:
%FOLD[  ]{%T // from %f%L%r%R%QT{
+----
%FOLD[|]{%i %s,  %@:
%FOLD[  ]{%T // from %f%L%r%R}}
+----}}
 ")
 '(twittering-use-master-password t)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(url-standalone-mode nil)
 '(user-full-name "Richard G.Riley" nil nil "Customized with use-package mu4e")
 '(user-mail-address "rileyrg@gmx.de")
 '(vc-follow-symlinks t)
 '(visible-bell t)
 '(vterm-always-compile-module t)
 '(warning-minimum-level :error)
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp)))
 '(wg-session-file "~/.config/emacs/emacs_workgroups")
 '(whitespace-style '(tabs trailing lines-tail empty))
 '(windmove-wrap-around t)
 '(xref-prompt-for-identifier nil)
 '(yank-pop-change-selection t)
 '(zone-programs [zone-matrix] t))



(message "Custom loading completed.")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 105))))
 '(tab-bar ((t (:background "gray24" :foreground "#ffffff"))))
 '(tab-bar-tab ((t (:background "black" :foreground "#ffffff"))))
 '(tab-bar-tab-inactive ((t (:background "gray24" :foreground "#ffffff")))))