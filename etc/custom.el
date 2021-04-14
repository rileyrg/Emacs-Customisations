(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'bully)
 '(TeX-macro-global
   '("~/texlive/2013" "~/texlive/2013/texmf-dist/bibtex/bst/" "~/texlive/2013/texmf-dist/tex/" "~/texlive/texmf-local/tex/"))
 '(ack-and-a-half-prompt-for-directory t t)
 '(ack-and-a-half-regexp-search nil t)
 '(ack-executable "/usr/bin/ack" t)
 '(ack-prompt-for-directory t t)
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
 '(company-box-enable-icon nil)
 '(company-c-headers-path-user
   '("~/.platformio/packages/framework-arduinoavr/cores/arduino/Arduino.h"))
 '(company-dabbrev-downcase nil)
 '(company-irony-ignore-case 'smart)
 '(company-lsp-cache-candidates 'auto)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-tooltip-limit 20)
 '(compilation-error-regexp-alist
   '(bash-xset-x dotnet-warning dotnet-error msbuild-warning msbuild-error xbuild-warning xbuild-error dotnet-testfail typescript-nglint-warning typescript-nglint-error typescript-tslint typescript-tsc-pretty typescript-tsc absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek gradle-kotlin iar ibm irix java jikes-file maven jikes-line clang-include gcc-include ruby-Test::Unit gmake gnu lcc makepp mips-1 mips-2 omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line))
 '(compilation-message-face 'default)
 '(confirm-nonexistent-file-or-buffer nil)
 '(cquery-executable "~/.bin/cquery")
 '(cquery-project-root-matchers
   '(projectile-project-root "compile_commands.json" ".cquery" "build/compile_commands.json"))
 '(cquery-project-roots '("~/development/projects/arduino/Blink-cmake-ng"))
 '(create-lockfiles nil)
 '(cua-keep-region-after-copy nil)
 '(cursor-type 'box)
 '(custom-safe-themes
   '("7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "972d69a06b8f6e5d43fb83ff59417511f09ba1a1783aab5e22c0e9cbd25ad458" "d234fa6da0282c254f00d534079374d1c6f3c3e600075bc65fe661fdd1947792" "7d6861c031212fca9b4a963ced6230be2aa3139570b85ea5e77619b1fd0351ad" "9b39b25c3a23b1be6e99a3648b91ebaf2a7efdde236e3472aa95f1708ec61d4f" "3f1dcd824a683e0ab194b3a1daac18a923eed4dba5269eecb050c718ab4d5a26" default))
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
 '(elpy-formatter 'autopep8)
 '(elscreen-display-tab nil)
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(enable-recursive-minibuffers t)
 '(epa-file-cache-passphrase-for-symmetric-encryption nil)
 '(erc-autojoin-channels-alist
   '(("irc.freenode.net" "##php" "#emacs" "#symfony" "#elisp" "#arduino" "#guitarix" "#ardour" "#debian" "##programming" "##linux")
     ("oftc.net" "#debian-next")))
 '(erc-autojoin-delay 1)
 '(erc-autojoin-domain-only nil)
 '(erc-autojoin-mode t)
 '(erc-autojoin-timing 'ident)
 '(erc-fill-column 128)
 '(erc-fill-function 'erc-fill-static)
 '(erc-fill-mode t)
 '(erc-fill-static-center 1)
 '(erc-format-nick-function 'rgr/erc-format-nick)
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
   '(autojoin button fill irccontrols match move-to-prompt noncommands readonly ring services smiley stamp track truncate))
 '(erc-nick-uniquifier "_DUP")
 '(erc-nickserv-identify-mode 'autodetect)
 '(erc-notice-highlight-type 'prefix)
 '(erc-notifications-mode t)
 '(erc-pcomplete-mode t)
 '(erc-prompt "#>")
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-query-display 'window-noselect)
 '(erc-scrolltobottom-mode nil)
 '(erc-server-auto-reconnect nil)
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
 '(fci-rule-color "#14151E" t)
 '(fill-column 256)
 '(find-file-visit-truename t)
 '(frecentf-ignore-paths
   '("/home/rgr/.config/emacs/persp-confs" "/home/rgr/.config/emacs/orgfiles"))
 '(gc-cons-threshold 100000000)
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(gdscript-docs-local-path
   "/home/rgr/Documents/Programming/Godot/www/docs.godotengine.org/en/stable")
 '(gdscript-docs-use-eww nil)
 '(geben-dbgp-default-port 8000)
 '(git-gutter:update-interval 2)
 '(gnus-activate-level 3)
 '(gnus-article-sort-functions '((not gnus-article-sort-by-date)))
 '(gnus-desktop-notify-exec-program "notify-send")
 '(gnus-desktop-notify-groups 'gnus-desktop-notify-explicit)
 '(gnus-directory "~/.News/")
 '(gnus-group-default-list-level 3)
 '(gnus-group-line-format "%M%S%5y/%-8t: %G
")
 '(gnus-group-mode-hook '(gnus-topic-mode))
 '(gnus-group-use-permanent-levels nil)
 '(gnus-interactive-catchup nil)
 '(gnus-interactive-exit nil)
 '(gnus-message-archive-group nil)
 '(gnus-permanently-visible-groups "INBOX\\|Google\\ Mail")
 '(gnus-save-newsrc-file t)
 '(gnus-select-method '(nnnil "news"))
 '(gnus-show-threads t)
 '(gnus-thread-sort-functions '((not gnus-thread-sort-by-date)))
 '(gnus-use-dribble-file nil)
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
 '(lookup-reference-functions
   '(rgr/describe-symbol goldendict-dwim rgr/dictionary-search rgr/linguee-lookup rgr/jquery-lookup google-this-search))
 '(lsp-auto-guess-root t)
 '(org-M-RET-may-split-line nil nil nil "Customized with use-package org")
 '(org-agenda-files "/home/rgr/.emacs.d/etc/org/agenda-files.txt" nil nil "Customized with use-package org")
 '(org-agenda-skip-scheduled-if-deadline-is-shown t nil nil "Customized with use-package org")
 '(org-agenda-start-on-weekday 0 nil nil "Customized with use-package org")
 '(org-babel-load-languages '((emacs-lisp . t) (python . t) (shell . t)) nil nil "Customized with use-package org")
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
      (file "~/.config/emacs/orgfiles/orders.org")
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
 '(org-todo-keyword-faces
   '(("TODO" :foreground "red" :weight bold)
     ("STARTED" :foreground "green" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("SOMEDAY" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("QUOTE" :foreground "red" :weight bold)
     ("QUOTED" :foreground "magenta" :weight bold)
     ("APPROVED" :foreground "forest green" :weight bold)
     ("EXPIRED" :foreground "forest green" :weight bold)
     ("REJECTED" :foreground "forest green" :weight bold)
     ("OPEN" :foreground "blue" :weight bold)))
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
   '("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'" "orgfiles/.*" "\\.md$" "recentf$" "config\\.el$" "bookmarks$"))
 '(red "#ffffff" t)
 '(rgr/alert-learn-history
   '("Warum überquerte das Huhn die Straße?
Richard M. Nixon:
  Das Huhn hat die Straße nicht überquert. Ich wiederhole, das Huhn
  hat die Straße NICHT überquert.
" "Man tut oft bloß stolz, weil man vermutet, der andere denke stolz.
		-- Jean Paul
" "Die Geheimnisse der Lebenspfade darf und kann man nicht
offenbaren; es gibt Steine des Anstoßes, über die ein jeder Wanderer
stolpern muss. Der Poet aber deutet auf die Stelle hin.
		-- Goethe, Maximen und Reflektionen, Nr. 96
" "Mit Gateways wächst zusammen, was nicht zusammen gehört.
		-- Heiko Schlichting
" "Erfolgreich zu sein heißt, anders als die anderen zu sein.
		-- Woody Allen (eigentlich: Woody Stewart)
" "Wenn der Jüngling absurd ist,
Fällt er darüber in lange Pein;
Der Alte soll nicht absurd sein,
Weil das Leben ihm kurz ist.
		-- Johann Wolfgang von Goethe (Zahme Xenien)
" "Die christliche Religion ist eine intentionierte politische
Revolution, die, verfehlt, nachher moralisch geworden ist.
		-- Goethe, Maximen und Reflektionen, Nr. 626
" "Bei Gelegenheit der berlinischen Vorbilder für Fabrikanten kam
zur Sprache, ob so großer Aufwand auf die höchste Ausführung der
Blätter wäre nötig gewesen. Wobei sich ergab, dass gerade den
talentvollen jungen Künstler und Handwerker die Ausführung am meisten
reizt, und dass er durch Beachtung und Nachbildung derselben erst
befähigt wird, das Ganze und den Wert der Formen zu begreifen.
		-- Goethe, Maximen und Reflektionen, Nr. 1202
" "Polizisten sind wie Schnittlauch: innen hohl, Augen grün, und sie treten
meistens in Bündeln auf.
" "Es ist der Zweck der Physik, Ingenieure erkennen zu lassen, daß sie
nicht perfekt sind und den Rest der Menschheit erkennen zu lassen,
daß sie keine Ingenieure sind.
" "Staatsmänner schweben mit beiden Beinen fest über den Tatsachen.
		-- Oliver Hassencamp
" "Geselligkeit lag in meiner Natur; deswegen ich bei vielfachem
Unternehmen mir Mitarbeiter gewann und mich ihnen zum Mitarbeiter
bildete und so das Glück erreichte, mich in ihnen und sie in mir
fortleben zu sehn.
		-- Goethe, Maximen und Reflektionen, Nr. 464
" "Der Mensch ist das einzige Tier, das arbeiten muß.
		-- Immanuel Kant
" "Wie kann auch die Alleinherrschaft etwas Gutes sein, die tun
kann, was ihr beliebt, ohne Verantwortlichkeit?
		-- Herodot
" "Hinweis für U-Boot-Passagiere:
Zählen Sie, wie oft Sie abtauchen,
zählen Sie, wie oft Sie auftauchen,
Addieren Sie die beiden Zahlen,
teilen Sie die Summe durch 2,
und wenn ein Rest bleibt, auf keinen Fall die Luke öffnen.
" "DER NICHTBUS
Der Nichtbus darf nicht mit dem Nachtbus verwechselt werden.  Der
Nachtbus kommt nachts, während der Nichtbus, wie der Name schon sagt,
NICHT kommt. Ein typischer Fahrplan eines Nichtbus sieht etwa so aus:
werktags, samstags, sonntags KEIN BETRIEB
Es gibt Nichtbusse, die alle 10 Minuten NICHT kommen, aber auch
solche, die nur alle Stunde nicht kommen, oder solche, die alle Minute
nicht kommen, wurden beobachtet.
Erstaunlicherweise hat dies überhaupt keinen Effekt darauf, daß sie
nicht kommen, ein ontologisches Problem, welches schon viele
Philosophen dazu veranlaßte, lieber zu Fuß zu gehen, wie schon dieses
Gedicht von Heinz Erhardt bezeugt:
Jaja, der alte Archimedes ging nur zu Fuß, fuhr nie Mercedes.
Kommen wir nun zu den Vorteilen des Nichtbus. Zu nennen wäre der
äußerst einfach zu merkende Fahrplan und die Tatsache, daß man keine
Angst davor haben muß, daß einem ein Nichtbus vor der Nase
wegfährt. Auch ist die nicht zu überbietende Ökobilanz des Nichtbus
bemerkenswert: Er verbraucht KEINEN Treibstoff und verursacht auch
keine Staus. Pingelige Leute könnten einwenden, der Nichtbus habe den
Nachteil, daß er völlig ungeeignet dazu ist, von A nach B zu
kommen. Aber was will man schon in B?  Ohne Beschränkung der
Gemeinheit hätte man in diesem Beispiel auch A B nennen können und
umgekehrt, woraus logisch unwiderleglich folgt, daß alle Orte eh
gleich sind und man auch zu Hause bleiben kann.
Ich möchte schließen mit einem Auszug aus einem weiteren Gedicht:
Denn wenn man wohin eilig muß, geht man besser gleich zu Fuß.
" "Beamte sind oft wie die Bücher in einer Bibliothek: Die überflüssigsten
stehen ganz weit oben.
" "Windows 98 bringt die Leistung von gestern auf die Computer von heute.
") t)
 '(rgr/alert-learn-period 300)
 '(rgr/chat-close-functions '(slack-ws-close rgr/erc-quit))
 '(rgr/chat-functions '(rgr/erc-start slack-start))
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
 '(session-save-file "~/.config/emacs/.session" t)
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
 '(tooltip-frame-parameters
   '((name . "tooltip")
     (internal-border-width . 2)
     (border-width . 1)
     (no-special-glyphs . t)
     (alpha . 70)))
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
 '(user-mail-address "rileyrg@gmail.com")
 '(vc-annotate-background nil t)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3")) t)
 '(vc-annotate-very-old-color nil t)
 '(vc-follow-symlinks t)
 '(visible-bell t)
 '(warning-minimum-level :error)
 '(wg-session-file "~/.config/emacs/emacs_workgroups")
 '(whitespace-style '(tabs trailing lines-tail empty))
 '(windmove-wrap-around t)
 '(yank-pop-change-selection t)
 '(zone-programs [zone-matrix] t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 80))))
 '(centaur-tabs-active-bar-face ((t (:background "orange"))))
 '(company-box-annotation ((t (:inherit company-tooltip-annotation :height 113))) t)
 '(company-box-candidate ((t (:foreground "white" :height 113))) t)
 '(company-box-scrollbar ((t nil)))
 '(company-tooltip ((t nil)))
 '(company-tooltip-annotation ((t (:foreground "gold"))))
 '(company-tooltip-selection ((t (:background "SeaGreen4"))))
 '(ctrlf-highlight-active ((t (:inherit nil :background "gold" :foreground "dim gray"))))
 '(ctrlf-highlight-passive ((t (:inherit nil :background "gray13" :foreground "white"))))
 '(cursor ((t (:background "gold" :foreground "black"))))
 '(dired-directory ((t (:foreground "burlywood" :weight extra-bold))))
 '(dired-symlink ((t (:inherit nil :foreground "yellow"))))
 '(erc-current-nick-face ((t (:background "dim gray" :height 148 :foreground "red" :weight bold))))
 '(erc-nick-default-face ((t (:weight light))))
 '(erc-timestamp-face ((t (:inherit erc-default-face :foreground "green" :weight semi-light))))
 '(font-lock-doc-face ((t (:foreground "light gray"))))
 '(font-lock-keyword-face ((t (:foreground "white smoke" :weight bold))))
 '(font-lock-type-face ((t (:foreground "light goldenrod"))))
 '(git-commit-summary ((t (:inherit nil))))
 '(gnus-group-mail-3-empty ((t (:foreground "gray"))))
 '(hl-line ((t (:inherit nil :extend t :background "gray10"))))
 '(internal-border ((t (:foreground "orange" :width normal))))
 '(lsp-face-highlight-read ((t (:inherit highlight :background "gray32" :underline t))))
 '(lsp-face-highlight-write ((t (:inherit nil :background "dim gray" :foreground "gold"))))
 '(lsp-ui-doc-background ((t nil)))
 '(lsp-ui-sideline-code-action ((t (:foreground "red"))))
 '(lsp-ui-sideline-current-symbol ((t (:foreground "tomato" :box (:line-width -1 :color "light gray") :weight ultra-bold :height 0.99))))
 '(lsp-ui-sideline-symbol ((t (:foreground "dark gray" :box (:line-width -1 :color "white smoke") :height 0.99))))
 '(org-archived ((t (:background "#181a20" :foreground "#a8a8a8" :height 0.8))))
 '(org-block-begin-line ((t nil)))
 '(org-block-end-line ((t (:inherit nil :extend t))))
 '(org-level-1 ((t (:inherit bold :foreground "yellow" :height 1.3))))
 '(org-scheduled-previously ((t (:background "gray8" :foreground "yellow" :inverse-video nil))))
 '(org-tag ((t (:background "#373844" :foreground "dark orange" :weight bold))))
 '(php-$this ((t (:foreground "gray"))))
 '(php-$this-sigil ((t (:inherit php-constant :foreground "gray88"))))
 '(php-class ((t nil)))
 '(php-doc-annotation-tag ((t (:inherit font-lock-constant-face :foreground "chartreuse"))))
 '(php-function-name ((t (:foreground "orange1"))))
 '(php-keyword ((t nil)))
 '(php-method-call ((t (:inherit php-function-call :foreground "chartreuse"))))
 '(php-string ((t (:inherit nil :foreground "lemon chiffon"))))
 '(php-type ((t (:inherit font-lock-type-face :foreground "pale goldenrod"))))
 '(php-variable-name ((t (:inherit font-lock-variable-name-face :foreground "antique white"))))
 '(php-variable-sigil ((t (:foreground "dark gray" :weight semi-light))))
 '(popup-face ((t (:inherit default))))
 '(rainbow-delimiters-base-error-face ((t (:inherit rainbow-delimiters-base-face :background "gainsboro" :foreground "red"))))
 '(selectrum-current-candidate ((t (:inherit bold :extend t :background "#2e2e2e" :foreground "#ffffff" :underline "burlywood"))))
 '(tooltip ((t (:inherit variable-pitch :background "dim gray" :foreground "orange"))))
 '(treemacs-git-modified-face ((t (:inherit font-lock-variable-name-face :background "yellow" :foreground "red")))))


(message "Custom loading completed.")
