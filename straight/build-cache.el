
:tanat

"29.4"

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("org-elpa" ("2024-11-07 19:34:26" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2024-11-07 19:34:27" nil (:type git :host github :repo "melpa/melpa" :build nil :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2024-11-07 19:34:27" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :build nil :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "nongnu-elpa" ("2024-11-07 19:34:27" nil (:type git :repo "https://git.savannah.gnu.org/git/emacs/nongnu.git" :depth (full single-branch) :local-repo "nongnu-elpa" :build nil :package "nongnu-elpa")) "el-get" ("2024-11-07 19:34:27" nil (:type git :host github :repo "dimitri/el-get" :build nil :files (:defaults "methods" ("recipes" "recipes/el-get.rcp") "el-get-pkg.el") :flavor melpa :package "el-get" :local-repo "el-get")) "emacsmirror-mirror" ("2024-11-07 19:34:27" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :build nil :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2024-11-07 19:34:27" ("emacs") (:type git :host github :repo "radian-software/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "no-littering" ("2024-11-07 19:34:27" ("emacs" "compat") (:type git :flavor melpa :host github :repo "emacscollective/no-littering" :package "no-littering" :local-repo "no-littering")) "compat" ("2024-11-07 19:34:27" ("emacs" "seq") (:type git :host github :repo "emacs-straight/compat" :files ("*" (:exclude ".git")) :package "compat" :local-repo "compat")) "seq" ("2024-11-07 19:34:27" nil (:type git :host github :repo "emacs-straight/seq" :files ("*" (:exclude ".git")) :package "seq" :local-repo "seq")) "org" ("2024-11-07 19:34:27" ("emacs") (:type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")) :package "org")) "project" ("2024-11-07 19:34:27" ("emacs" "xref") (:type git :host github :repo "emacs-straight/project" :files ("*" (:exclude ".git")) :package "project" :local-repo "project")) "xref" ("2024-11-07 19:34:27" ("emacs") (:type git :host github :repo "emacs-straight/xref" :files ("*" (:exclude ".git")) :package "xref" :local-repo "xref")) "flymake" ("2024-11-07 19:34:27" ("emacs" "eldoc" "project") (:type git :host github :repo "emacs-straight/flymake" :files ("*" (:exclude ".git")) :package "flymake" :local-repo "flymake")) "eldoc" ("2024-11-07 19:34:27" ("emacs") (:type git :host github :repo "emacs-straight/eldoc" :files ("*" (:exclude ".git")) :package "eldoc" :local-repo "eldoc")) "pass" ("2024-11-07 19:34:28" ("emacs" "password-store" "password-store-otp" "f") (:type git :flavor melpa :host github :repo "NicolasPetton/pass" :package "pass" :local-repo "pass")) "password-store" ("2024-11-07 19:34:28" ("emacs" "with-editor") (:type git :flavor melpa :files ("contrib/emacs/*.el" "password-store-pkg.el") :host github :repo "zx2c4/password-store" :package "password-store" :local-repo "password-store")) "with-editor" ("2024-11-07 19:34:28" ("emacs" "compat") (:type git :flavor melpa :host github :repo "magit/with-editor" :package "with-editor" :local-repo "with-editor")) "password-store-otp" ("2024-11-07 19:34:28" ("emacs" "s" "password-store") (:type git :flavor melpa :host github :repo "volrath/password-store-otp.el" :package "password-store-otp" :local-repo "password-store-otp.el")) "s" ("2024-11-07 19:34:28" nil (:type git :flavor melpa :host github :repo "magnars/s.el" :package "s" :local-repo "s.el")) "f" ("2024-11-07 19:34:28" ("emacs" "s" "dash") (:type git :flavor melpa :host github :repo "rejeep/f.el" :package "f" :local-repo "f.el")) "dash" ("2024-11-07 19:34:28" ("emacs") (:type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el" :package "dash" :local-repo "dash.el")) "gif-screencast" ("2024-11-07 19:34:28" ("emacs") (:type git :flavor melpa :host gitlab :repo "Ambrevar/emacs-gif-screencast" :package "gif-screencast" :local-repo "emacs-gif-screencast")) "lazy-lang-learn" ("2024-11-07 19:34:28" ("emacs" "google-translate" "alert") (:local-repo "~/development/projects/emacs/lazy-lang-learn" :type git :host github :repo "rileyrg/lazy-lang-learn" :package "lazy-lang-learn")) "google-translate" ("2024-11-07 19:34:28" ("emacs" "popup") (:type git :flavor melpa :host github :repo "atykhonov/google-translate" :package "google-translate" :local-repo "google-translate")) "popup" ("2024-11-07 19:34:28" ("emacs") (:type git :flavor melpa :host github :repo "auto-complete/popup-el" :package "popup" :local-repo "popup-el")) "alert" ("2024-11-07 19:34:28" ("gntp" "log4e" "cl-lib") (:type git :flavor melpa :host github :repo "jwiegley/alert" :package "alert" :local-repo "alert")) "gntp" ("2024-11-07 19:34:28" nil (:type git :flavor melpa :host github :repo "tekai/gntp.el" :package "gntp" :local-repo "gntp.el")) "log4e" ("2024-11-07 19:34:28" nil (:type git :flavor melpa :host github :repo "aki2o/log4e" :package "log4e" :local-repo "log4e")) "browse-url-dwim" ("2024-11-07 19:34:28" ("string-utils") (:type git :flavor melpa :host github :repo "rolandwalker/browse-url-dwim" :package "browse-url-dwim" :local-repo "browse-url-dwim")) "string-utils" ("2024-11-07 19:34:28" ("list-utils") (:type git :flavor melpa :host github :repo "rolandwalker/string-utils" :package "string-utils" :local-repo "string-utils")) "list-utils" ("2024-11-07 19:34:28" nil (:type git :flavor melpa :host github :repo "rolandwalker/list-utils" :package "list-utils" :local-repo "list-utils")) "posframe" ("2024-11-07 19:34:28" ("emacs") (:type git :flavor melpa :host github :repo "tumashu/posframe" :package "posframe" :local-repo "posframe")) "ace-window" ("2024-11-07 19:34:28" ("avy") (:type git :flavor melpa :host github :repo "abo-abo/ace-window" :package "ace-window" :local-repo "ace-window")) "avy" ("2024-11-07 19:34:28" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "abo-abo/avy" :package "avy" :local-repo "avy")) "ace-link" ("2024-11-07 19:34:28" ("avy") (:type git :flavor melpa :host github :repo "abo-abo/ace-link" :package "ace-link" :local-repo "ace-link")) "ace-jump-mode" ("2024-11-07 19:34:28" nil (:type git :flavor melpa :host github :repo "winterTTr/ace-jump-mode" :package "ace-jump-mode" :local-repo "ace-jump-mode")) "golden-ratio" ("2024-11-07 19:34:28" nil (:type git :flavor melpa :host github :repo "roman/golden-ratio.el" :package "golden-ratio" :local-repo "golden-ratio.el")) "pulsar" ("2024-11-07 19:34:28" ("emacs") (:type git :host github :repo "emacs-straight/pulsar" :files ("*" (:exclude ".git")) :package "pulsar" :local-repo "pulsar")) "blackout" ("2024-11-07 19:34:28" ("emacs") (:host github :repo "raxod502/blackout" :flavor melpa :package "blackout" :type git :local-repo "blackout")) "boxquote" ("2024-11-07 19:34:28" ("cl-lib") (:branch "main" :flavor melpa :repo "davep/boxquote.el" :host github :package "boxquote" :type git :local-repo "boxquote.el")) "dpaste" ("2024-11-07 19:34:28" nil (:type git :flavor melpa :host github :repo "gregnewman/dpaste.el" :package "dpaste" :local-repo "dpaste.el")) "darkroom" ("2024-11-07 19:34:28" ("cl-lib") (:type git :host github :repo "emacs-straight/darkroom" :files ("*" (:exclude ".git")) :package "darkroom" :local-repo "darkroom")) "bookmark+" ("2024-11-07 19:34:28" nil (:type git :host github :repo "emacsmirror/bookmark-plus" :files (:defaults) :package "bookmark+" :local-repo "bookmark-plus")) "emojify" ("2024-11-07 19:34:28" ("seq" "ht" "emacs") (:type git :flavor melpa :files (:defaults "data" "images" "emojify-pkg.el") :host github :repo "iqbalansari/emacs-emojify" :package "emojify" :local-repo "emacs-emojify")) "ht" ("2024-11-07 19:34:28" ("dash") (:type git :flavor melpa :host github :repo "Wilfred/ht.el" :package "ht" :local-repo "ht.el")) "multiple-cursors" ("2024-11-07 19:34:29" ("cl-lib") (:type git :flavor melpa :host github :repo "magnars/multiple-cursors.el" :package "multiple-cursors" :local-repo "multiple-cursors.el")) "jinx" ("2024-11-07 19:34:29" ("emacs" "compat") (:type git :flavor melpa :files (:defaults "jinx-mod.c" "emacs-module.h" "jinx-pkg.el") :host github :repo "minad/jinx" :package "jinx" :local-repo "jinx")) "ripgrep" ("2024-11-07 19:34:29" nil (:type git :flavor melpa :files ("ripgrep.el" "ripgrep-pkg.el") :host github :repo "nlamirault/ripgrep.el" :package "ripgrep" :local-repo "ripgrep.el")) "sudo-edit" ("2024-11-07 19:34:29" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "nflath/sudo-edit" :package "sudo-edit" :local-repo "sudo-edit")) "prescient" ("2024-11-07 19:34:29" ("emacs") (:type git :flavor melpa :files ("prescient.el" "prescient-pkg.el") :host github :repo "radian-software/prescient.el" :package "prescient" :local-repo "prescient.el")) "consult" ("2024-11-07 19:34:29" ("emacs" "compat") (:type git :flavor melpa :host github :repo "minad/consult" :package "consult" :local-repo "consult")) "marginalia" ("2024-11-07 19:34:29" ("emacs" "compat") (:type git :flavor melpa :host github :repo "minad/marginalia" :package "marginalia" :local-repo "marginalia")) "all-the-icons" ("2024-11-07 19:34:29" ("emacs") (:type git :flavor melpa :files (:defaults "data" "all-the-icons-pkg.el") :host github :repo "domtronn/all-the-icons.el" :package "all-the-icons" :local-repo "all-the-icons.el")) "all-the-icons-completion" ("2024-11-07 19:34:29" ("emacs" "all-the-icons") (:type git :flavor melpa :host github :repo "iyefrat/all-the-icons-completion" :package "all-the-icons-completion" :local-repo "all-the-icons-completion")) "all-the-icons-dired" ("2024-11-07 19:34:29" ("emacs" "all-the-icons") (:type git :flavor melpa :host github :repo "wyuenho/all-the-icons-dired" :package "all-the-icons-dired" :local-repo "all-the-icons-dired")) "corfu" ("2024-11-07 19:34:29" ("emacs" "compat") (:type git :flavor melpa :files (:defaults "extensions/corfu-*.el" "corfu-pkg.el") :host github :repo "minad/corfu" :package "corfu" :local-repo "corfu")) "orderless" ("2024-11-07 19:34:29" ("emacs" "compat") (:type git :flavor melpa :host github :repo "oantolin/orderless" :package "orderless" :local-repo "orderless")) "cape" ("2024-11-07 19:34:29" ("emacs" "compat") (:type git :flavor melpa :host github :repo "minad/cape" :package "cape" :local-repo "cape")) "which-key" ("2024-11-07 19:34:29" ("emacs") (:type git :flavor melpa :host github :repo "justbur/emacs-which-key" :package "which-key" :local-repo "emacs-which-key")) "yasnippet" ("2024-11-07 19:34:30" ("cl-lib" "emacs") (:type git :flavor melpa :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :host github :repo "joaotavora/yasnippet" :package "yasnippet" :local-repo "yasnippet")) "yasnippet-snippets" ("2024-11-07 19:34:30" ("yasnippet") (:type git :flavor melpa :files ("*.el" "snippets" ".nosearch" "yasnippet-snippets-pkg.el") :host github :repo "AndreaCrotti/yasnippet-snippets" :package "yasnippet-snippets" :local-repo "yasnippet-snippets")) "vertico" ("2024-11-07 19:34:30" ("emacs" "compat") (:type git :flavor melpa :files (:defaults "extensions/vertico-*.el" "vertico-pkg.el") :host github :repo "minad/vertico" :package "vertico" :local-repo "vertico")) "org-contrib" ("2024-11-07 19:34:30" ("emacs" "org") (:type git :includes (ob-csharp ob-eukleides ob-fomus ob-julia ob-mathomatic ob-oz ob-stata ob-tcl ob-vbnet ol-bookmark ol-elisp-symbol ol-git-link ol-man ol-mew ol-vm ol-wl org-annotate-file org-bibtex-extras org-checklist org-choose org-collector org-contribdir org-depend org-effectiveness org-eldoc org-eval org-eval-light org-expiry org-interactive-query org-invoice org-learn org-license org-mac-iCal org-mairix org-panel org-registry org-screen org-screenshot org-secretary org-static-mathjax org-sudoku orgtbl-sqlinsert org-toc org-track org-wikinodes ox-bibtex ox-confluence ox-deck ox-extra ox-freemind ox-groff ox-koma-letter ox-s5 ox-taskjuggler) :repo "https://git.sr.ht/~bzg/org-contrib" :files (:defaults "lisp/*.el") :package "org-contrib" :local-repo "org-contrib")) "ob-async" ("2024-11-07 19:34:30" ("async" "org" "emacs" "dash") (:type git :flavor melpa :host github :repo "astahlman/ob-async" :package "ob-async" :local-repo "ob-async")) "async" ("2024-11-07 19:34:30" ("emacs") (:type git :flavor melpa :host github :repo "jwiegley/emacs-async" :package "async" :local-repo "emacs-async")) "org-super-agenda" ("2024-11-07 19:34:30" ("emacs" "compat" "s" "dash" "org" "ht" "ts") (:type git :flavor melpa :host github :repo "alphapapa/org-super-agenda" :package "org-super-agenda" :local-repo "org-super-agenda")) "ts" ("2024-11-07 19:34:30" ("emacs" "dash" "s") (:type git :flavor melpa :host github :repo "alphapapa/ts.el" :package "ts" :local-repo "ts.el")) "ox-gfm" ("2024-11-07 19:34:31" nil (:type git :flavor melpa :host github :repo "larstvei/ox-gfm" :package "ox-gfm" :local-repo "ox-gfm")) "gptel" ("2024-11-07 19:34:31" ("emacs" "transient" "compat") (:type git :flavor melpa :host github :repo "karthink/gptel" :package "gptel" :local-repo "gptel")) "transient" ("2024-11-07 19:34:31" ("emacs" "compat" "seq") (:type git :flavor melpa :host github :repo "magit/transient" :package "transient" :local-repo "transient")) "ellama" ("2024-11-07 19:34:31" ("emacs" "llm" "spinner" "compat") (:type git :flavor melpa :host github :repo "s-kostyaev/ellama" :package "ellama" :local-repo "ellama")) "llm" ("2024-11-07 19:34:31" ("emacs" "plz") (:type git :host github :repo "emacs-straight/llm" :files ("*" (:exclude ".git")) :package "llm" :local-repo "llm")) "plz" ("2024-11-07 19:34:31" ("emacs") (:type git :host github :repo "emacs-straight/plz" :files ("*" (:exclude ".git")) :package "plz" :local-repo "plz")) "spinner" ("2024-11-07 19:34:31" ("emacs") (:type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git")) :package "spinner" :local-repo "spinner")) "go-translate" ("2024-11-07 19:34:31" ("emacs") (:type git :flavor melpa :host github :repo "lorniu/go-translate" :package "go-translate" :local-repo "go-translate")) "dictionary" ("2024-11-07 19:34:31" ("connection" "link") (:type git :flavor melpa :files ("dictionary.el" "dictionary-pkg.el") :host github :repo "myrkr/dictionary-el" :package "dictionary" :local-repo "dictionary-el")) "connection" ("2024-11-07 19:34:31" nil (:flavor melpa :files ("connection.el" "connection-pkg.el") :package "connection" :local-repo "dictionary-el" :type git :repo "myrkr/dictionary-el" :host github)) "link" ("2024-11-07 19:34:31" nil (:flavor melpa :files ("link.el" "link-pkg.el") :package "link" :local-repo "dictionary-el" :type git :repo "myrkr/dictionary-el" :host github)) "goldendict" ("2024-11-07 19:34:31" ("emacs" "cl-lib") (:type git :host github :repo "emacsmirror/goldendict" :package "goldendict" :local-repo "goldendict")) "devdocs-browser" ("2024-11-07 19:34:31" ("emacs") (:type git :flavor melpa :host github :repo "blahgeek/emacs-devdocs-browser" :package "devdocs-browser" :local-repo "emacs-devdocs-browser")) "elfeed" ("2024-11-07 19:34:31" ("emacs") (:type git :flavor melpa :files (:defaults "README.md" "elfeed-pkg.el") :host github :repo "skeeto/elfeed" :package "elfeed" :local-repo "elfeed")) "pdf-tools" ("2024-11-07 19:34:31" ("emacs" "tablist" "let-alist") (:type git :flavor melpa :files (:defaults "README" ("build" "Makefile") ("build" "server") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools" :package "pdf-tools" :local-repo "pdf-tools")) "tablist" ("2024-11-07 19:34:31" ("emacs") (:type git :flavor melpa :host github :repo "emacsorphanage/tablist" :package "tablist" :local-repo "tablist")) "let-alist" ("2024-11-07 19:34:31" ("emacs") (:type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git")) :package "let-alist" :local-repo "let-alist")) "eat" ("2024-11-07 19:34:31" ("emacs" "compat") (:type git :host codeberg :repo "akib/emacs-eat" :files ("*.el" ("term" "term/*.el") "*.texi" "*.ti" ("terminfo/e" "terminfo/e/*") ("terminfo/65" "terminfo/65/*") ("integration" "integration/*") (:exclude ".dir-locals.el" "*-tests.el")) :package "eat" :local-repo "emacs-eat")) "vterm" ("2024-11-07 19:34:31" ("emacs") (:type git :flavor melpa :files ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el" "vterm-module.c" "vterm-module.h" "vterm-pkg.el") :host github :repo "akermu/emacs-libvterm" :package "vterm" :local-repo "emacs-libvterm")) "notmuch" ("2024-11-07 19:34:32" nil (:type git :flavor melpa :files ("emacs/*.el" "emacs/*.svg" "notmuch-pkg.el") :repo "https://git.notmuchmail.org/git/notmuch" :package "notmuch" :local-repo "notmuch")) "mu4e" ("2024-11-07 19:34:32" nil (:host github :branch "release/1.10" :repo "djcb/mu" :files ("mu4e/*.el" "build/mu4e/mu4e-meta.el" "build/mu4e/mu4e-config.el" "build/mu4e/mu4e.info") :main "mu4e/mu4e.el" :pre-build (("./autogen.sh") ("ninja" "-C" "build") (make-symbolic-link (expand-file-name "./build/mu/mu") (expand-file-name "~/bin/mu") 'ok-if-exists)) :package "mu4e" :type git :local-repo "mu")) "erc" ("2024-11-07 19:34:32" ("emacs" "compat") (:type git :host github :repo "emacs-straight/erc" :files ("*" (:exclude ".git")) :package "erc" :local-repo "erc")) "eldoc-box" ("2024-11-07 19:34:32" ("emacs") (:type git :flavor melpa :host github :repo "casouri/eldoc-box" :package "eldoc-box" :local-repo "eldoc-box")) "json-mode" ("2024-11-07 19:34:32" ("json-snatcher" "emacs") (:type git :flavor melpa :host github :repo "json-emacs/json-mode" :package "json-mode" :local-repo "json-mode")) "json-snatcher" ("2024-11-07 19:34:32" ("emacs") (:type git :flavor melpa :host github :repo "Sterlingg/json-snatcher" :package "json-snatcher" :local-repo "json-snatcher")) "jsonrpc" ("2024-11-07 19:34:32" ("emacs") (:type git :host github :repo "emacs-straight/jsonrpc" :files ("*" (:exclude ".git")) :package "jsonrpc" :local-repo "jsonrpc")) "treemacs" ("2024-11-07 19:34:32" ("emacs" "cl-lib" "dash" "s" "ace-window" "pfuture" "hydra" "ht" "cfrs") (:type git :flavor melpa :files (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el" "src/scripts/treemacs*.py" (:exclude "src/extra/*") "treemacs-pkg.el") :host github :repo "Alexander-Miller/treemacs" :package "treemacs" :local-repo "treemacs")) "pfuture" ("2024-11-07 19:34:32" ("emacs") (:type git :flavor melpa :host github :repo "Alexander-Miller/pfuture" :package "pfuture" :local-repo "pfuture")) "hydra" ("2024-11-07 19:34:32" ("cl-lib" "lv") (:type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra" :package "hydra" :local-repo "hydra")) "lv" ("2024-11-07 19:34:32" nil (:flavor melpa :files ("lv.el" "lv-pkg.el") :package "lv" :local-repo "hydra" :type git :repo "abo-abo/hydra" :host github)) "cfrs" ("2024-11-07 19:34:32" ("emacs" "dash" "s" "posframe") (:type git :flavor melpa :host github :repo "Alexander-Miller/cfrs" :package "cfrs" :local-repo "cfrs")) "duplicate-thing" ("2024-11-07 19:34:32" nil (:type git :flavor melpa :host github :repo "ongaeshi/duplicate-thing" :package "duplicate-thing" :local-repo "duplicate-thing")) "breadcrumb" ("2024-11-07 19:34:32" ("emacs" "project") (:local-repo "~/development/projects/emacs/breadcrumb" :files ("*" (:exclude ".git")) :repo "emacs-straight/breadcrumb" :host github :package "breadcrumb" :type git)) "rmsbolt" ("2024-11-07 19:34:32" ("emacs") (:type git :flavor melpa :files (:defaults "starters" "rmsbolt-pkg.el") :host gitlab :repo "jgkamat/rmsbolt" :package "rmsbolt" :local-repo "rmsbolt")) "parrot" ("2024-11-07 19:34:32" ("emacs") (:type git :flavor melpa :files (:defaults "img" "parrot-pkg.el") :host github :repo "dp12/parrot" :package "parrot" :local-repo "parrot")) "php-mode" ("2024-11-07 19:34:32" ("emacs") (:type git :flavor melpa :host github :repo "emacs-php/php-mode" :package "php-mode" :local-repo "php-mode")) "yaml-mode" ("2024-11-07 19:34:32" ("emacs") (:type git :flavor melpa :host github :repo "yoshiki/yaml-mode" :package "yaml-mode" :local-repo "yaml-mode")) "json-reformat" ("2024-11-07 19:34:32" ("emacs") (:type git :flavor melpa :host github :repo "gongo/json-reformat" :package "json-reformat" :local-repo "json-reformat")) "flymake-diagnostic-at-point" ("2024-11-07 19:34:32" ("emacs" "popup") (:type git :flavor melpa :host github :repo "meqif/flymake-diagnostic-at-point" :package "flymake-diagnostic-at-point" :local-repo "flymake-diagnostic-at-point")) "magit" ("2024-11-07 19:34:32" ("emacs" "compat" "dash" "magit-section" "seq" "transient" "with-editor") (:type git :flavor melpa :files ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "magit-pkg.el" (:exclude "lisp/magit-section.el") "magit-pkg.el") :host github :repo "magit/magit" :package "magit" :local-repo "magit")) "magit-section" ("2024-11-07 19:34:32" ("emacs" "compat" "dash" "seq") (:flavor melpa :files ("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el" "magit-section-pkg.el") :package "magit-section" :local-repo "magit" :type git :repo "magit/magit" :host github)) "magit-filenotify" ("2024-11-07 19:34:32" ("magit" "emacs") (:type git :flavor melpa :host github :repo "ruediger/magit-filenotify" :package "magit-filenotify" :local-repo "magit-filenotify")) "sqlite3" ("2024-11-07 19:34:33" ("emacs") (:type git :flavor melpa :files (:defaults "Makefile" "consts.c" "emacs-module.h" "sqlite3-api.c" "sqlite3-pkg.el") :host github :repo "pekingduck/emacs-sqlite3-api" :package "sqlite3" :local-repo "emacs-sqlite3-api")) "diff-hl" ("2024-11-07 19:34:33" ("cl-lib" "emacs") (:type git :flavor melpa :host github :repo "dgutov/diff-hl" :package "diff-hl" :local-repo "diff-hl")) "treesit-auto" ("2024-11-07 19:34:33" ("emacs") (:type git :flavor melpa :host github :repo "renzmann/treesit-auto" :package "treesit-auto" :local-repo "treesit-auto")) "eglot" ("2024-11-07 19:34:33" ("emacs" "compat" "eldoc" "external-completion" "flymake" "jsonrpc" "project" "seq" "track-changes" "xref") (:type git :host github :repo "emacs-straight/eglot" :files ("*" (:exclude ".git")) :package "eglot" :local-repo "eglot")) "external-completion" ("2024-11-07 19:34:33" nil (:type git :host github :repo "emacs-straight/external-completion" :files ("*" (:exclude ".git")) :package "external-completion" :local-repo "external-completion")) "track-changes" ("2024-11-07 19:34:33" ("emacs") (:type git :host github :repo "emacs-straight/track-changes" :files ("*" (:exclude ".git")) :package "track-changes" :local-repo "track-changes")) "dape" ("2024-11-07 19:34:34" ("emacs" "jsonrpc") (:type git :host github :repo "emacs-straight/dape" :files ("*" (:exclude ".git")) :package "dape" :local-repo "dape")) "auto-virtualenv" ("2024-11-07 19:34:34" ("cl-lib" "pyvenv" "s") (:type git :flavor melpa :host github :repo "marcwebbie/auto-virtualenv" :package "auto-virtualenv" :local-repo "auto-virtualenv")) "pyvenv" ("2024-11-07 19:34:34" nil (:type git :flavor melpa :host github :repo "jorgenschaefer/pyvenv" :package "pyvenv" :local-repo "pyvenv")) "lldb-voltron" ("2024-11-07 19:34:34" nil (:local-repo "~/development/projects/emacs/emacs-lldb-voltron" :type git :host github :repo "rileyrg/emacs-lldb-voltron" :package "lldb-voltron")) "rust-mode" ("2024-11-07 19:34:34" ("emacs") (:type git :flavor melpa :host github :repo "rust-lang/rust-mode" :package "rust-mode" :local-repo "rust-mode")) "rustic" ("2024-11-07 19:34:34" ("emacs" "rust-mode" "dash" "f" "let-alist" "markdown-mode" "project" "s" "spinner" "xterm-color" "flycheck") (:type git :flavor melpa :host github :repo "emacs-rustic/rustic" :package "rustic" :local-repo "rustic")) "markdown-mode" ("2024-11-07 19:34:34" ("emacs") (:type git :flavor melpa :host github :repo "jrblevin/markdown-mode" :package "markdown-mode" :local-repo "markdown-mode")) "xterm-color" ("2024-11-07 19:34:34" ("emacs") (:type git :flavor melpa :host github :repo "atomontage/xterm-color" :package "xterm-color" :local-repo "xterm-color")) "flycheck" ("2024-11-07 19:34:34" ("emacs") (:type git :flavor melpa :host github :repo "flycheck/flycheck" :package "flycheck" :local-repo "flycheck")) "logview" ("2024-11-07 19:34:34" ("emacs" "datetime" "extmap") (:type git :flavor melpa :host github :repo "doublep/logview" :package "logview" :local-repo "logview")) "datetime" ("2024-11-07 19:34:34" ("emacs" "extmap") (:type git :flavor melpa :files (:defaults "*.extmap" "datetime-pkg.el") :host github :repo "doublep/datetime" :package "datetime" :local-repo "datetime")) "extmap" ("2024-11-07 19:34:34" ("emacs") (:type git :flavor melpa :host github :repo "doublep/extmap" :package "extmap" :local-repo "extmap")) "strace-mode" ("2024-11-07 19:34:34" nil (:type git :flavor melpa :host github :repo "pkmoore/strace-mode" :package "strace-mode" :local-repo "strace-mode")) "elf-mode" ("2024-11-07 19:34:34" ("emacs") (:type git :flavor melpa :host github :repo "abo-abo/elf-mode" :package "elf-mode" :local-repo "elf-mode")) "rgr-kill-dwim" ("2024-11-07 19:34:34" nil (:local-repo "~/development/projects/emacs/rgr-kill-dwim" :type git :host github :repo "rileyrg/rgr-kill-dwim" :package "rgr-kill-dwim")) "package-lint" ("2024-11-07 19:34:34" ("emacs" "let-alist") (:type git :flavor melpa :files (:defaults "data" (:exclude "*flymake.el") "package-lint-pkg.el") :host github :repo "purcell/package-lint" :package "package-lint" :local-repo "package-lint")) "helpful" ("2024-11-07 19:34:35" ("emacs" "dash" "s" "f" "elisp-refs") (:type git :flavor melpa :host github :repo "Wilfred/helpful" :package "helpful" :local-repo "helpful")) "elisp-refs" ("2024-11-07 19:34:35" ("dash" "s") (:type git :flavor melpa :files (:defaults (:exclude "elisp-refs-bench.el") "elisp-refs-pkg.el") :host github :repo "Wilfred/elisp-refs" :package "elisp-refs" :local-repo "elisp-refs")) "el-docstring-sap" ("2024-11-07 19:34:35" ("emacs" "posframe") (:local-repo "~/development/projects/emacs/el-docstring-sap" :type git :host github :repo "rileyrg/el-docstring-sap" :package "el-docstring-sap")) "quick-peek" ("2024-11-07 19:34:35" ("emacs") (:type git :flavor melpa :host github :repo "cpitclaudel/quick-peek" :package "quick-peek" :local-repo "quick-peek")) "edebug-x" ("2024-11-07 19:34:35" nil (:type git :flavor melpa :host github :repo "ScottyB/edebug-x" :package "edebug-x" :local-repo "edebug-x")) "elisp-format" ("2024-11-07 19:34:35" nil (:type git :flavor melpa :host github :repo "Yuki-Inoue/elisp-format" :package "elisp-format" :local-repo "elisp-format")) "modus-themes" ("2024-11-07 19:34:35" ("emacs") (:type git :flavor melpa :host github :repo "protesilaos/modus-themes" :package "modus-themes" :local-repo "modus-themes")) "mu4e-alert" ("2024-11-07 19:32:52" ("alert" "s" "ht" "emacs") (:type git :flavor melpa :host github :repo "xzz53/mu4e-alert" :package "mu4e-alert" :local-repo "mu4e-alert")) "mu4e-column-faces" ("2024-11-07 19:32:56" ("emacs") (:type git :flavor melpa :host github :repo "Alexander-Miller/mu4e-column-faces" :package "mu4e-column-faces" :local-repo "mu4e-column-faces"))))

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight-autoloads straight-ert-print-hack straight-x straight) (autoload 'straight-remove-unused-repos "straight" "Remove unused repositories from the repos and build directories.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos.

(fn &optional FORCE)" t) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

Optional arg FILTER must be a unary function.
It takes a package name as its sole argument.
If it returns nil the candidate is excluded.

(fn &optional SOURCES ACTION FILTER)" t) (autoload 'straight-visit-package-website "straight" "Visit the package RECIPE's website.

(fn RECIPE)" t) (autoload 'straight-visit-package "straight" "Open PACKAGE's local repository directory.
When BUILD is non-nil visit PACKAGE's build directory.

(fn PACKAGE &optional BUILD)" t) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil when package is initially installed, nil otherwise.

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded.") (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted.") (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t) (autoload 'straight-bug-report "straight" "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package \\='(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function 0) (autoload 'straight-dependencies "straight" "Return a list of PACKAGE's dependencies.

(fn &optional PACKAGE)" t) (autoload 'straight-dependents "straight" "Return a list of PACKAGE's dependents.

(fn &optional PACKAGE)" t) (register-definition-prefixes "straight" '("straight-")) (register-definition-prefixes "straight-ert-print-hack" '("+without-print-limits")) (defvar straight-x-pinned-packages nil "List of pinned packages.") (register-definition-prefixes "straight-x" '("straight-x-")) (provide 'straight-autoloads)) "seq" ((seq seq-pkg seq-25 seq-24 seq-autoloads) (register-definition-prefixes "seq-24" '("seq")) (autoload 'seq-subseq "seq-25" "Return the sequence of elements of SEQUENCE from START to END.
END is exclusive.

If END is omitted, it defaults to the length of the sequence.  If
START or END is negative, it counts from the end.  Signal an
error if START or END are outside of the sequence (i.e too large
if positive or too small if negative).

(fn SEQUENCE START &optional END)") (autoload 'seq-take "seq-25" "Return the sequence made of the first N elements of SEQUENCE.
The result is a sequence of the same type as SEQUENCE.

If N is a negative integer or zero, an empty sequence is
returned.

(fn SEQUENCE N)") (autoload 'seq-sort-by "seq-25" "Sort SEQUENCE transformed by FUNCTION using PRED as the comparison function.
Elements of SEQUENCE are transformed by FUNCTION before being
sorted.  FUNCTION must be a function of one argument.

(fn FUNCTION PRED SEQUENCE)") (autoload 'seq-filter "seq-25" "Return a list of all the elements in SEQUENCE for which PRED returns non-nil.

(fn PRED SEQUENCE)") (autoload 'seq-remove "seq-25" "Return a list of all the elements in SEQUENCE for which PRED returns nil.

(fn PRED SEQUENCE)") (autoload 'seq-remove-at-position "seq-25" "Return a copy of SEQUENCE with the element at index N removed.

N is the (zero-based) index of the element that should not be in
the result.

The result is a sequence of the same type as SEQUENCE.

(fn SEQUENCE N)") (autoload 'seq-reduce "seq-25" "Reduce the function FUNCTION across SEQUENCE, starting with INITIAL-VALUE.

Return the result of calling FUNCTION with INITIAL-VALUE and the
first element of SEQUENCE, then calling FUNCTION with that result
and the second element of SEQUENCE, then with that result and the
third element of SEQUENCE, etc.  FUNCTION will be called with
INITIAL-VALUE (and then the accumulated value) as the first
argument, and the elements from SEQUENCE as the second argument.

If SEQUENCE is empty, return INITIAL-VALUE and FUNCTION is not called.

(fn FUNCTION SEQUENCE INITIAL-VALUE)") (autoload 'seq-every-p "seq-25" "Return non-nil if PRED returns non-nil for all the elements of SEQUENCE.

(fn PRED SEQUENCE)") (autoload 'seq-some "seq-25" "Return non-nil if PRED returns non-nil for at least one element of SEQUENCE.
If the value is non-nil, it is the first non-nil value returned by PRED.

(fn PRED SEQUENCE)") (autoload 'seq-find "seq-25" "Return the first element in SEQUENCE for which PRED returns non-nil.
If no such element is found, return DEFAULT.

Note that `seq-find' has an ambiguity if the found element is
identical to DEFAULT, as in that case it is impossible to know
whether an element was found or not.

(fn PRED SEQUENCE &optional DEFAULT)") (autoload 'seq-position "seq-25" "Return the (zero-based) index of the first element in SEQUENCE \"equal\" to ELT.
\"Equality\" is defined by the function TESTFN, which defaults to `equal'.

(fn SEQUENCE ELT &optional TESTFN)") (autoload 'seq-positions "seq-25" "Return list of indices of SEQUENCE elements for which TESTFN returns non-nil.

TESTFN is a two-argument function which is called with each element of
SEQUENCE as the first argument and ELT as the second.
TESTFN defaults to `equal'.

The result is a list of (zero-based) indices.

(fn SEQUENCE ELT &optional TESTFN)") (autoload 'seq-uniq "seq-25" "Return a list of the elements of SEQUENCE with duplicates removed.
TESTFN is used to compare elements, and defaults to `equal'.

(fn SEQUENCE &optional TESTFN)") (autoload 'seq-union "seq-25" "Return a list of all the elements that appear in either SEQUENCE1 or SEQUENCE2.
\"Equality\" of elements is defined by the function TESTFN, which
defaults to `equal'.

(fn SEQUENCE1 SEQUENCE2 &optional TESTFN)") (autoload 'seq-intersection "seq-25" "Return a list of all the elements that appear in both SEQUENCE1 and SEQUENCE2.
\"Equality\" of elements is defined by the function TESTFN, which
defaults to `equal'.

(fn SEQUENCE1 SEQUENCE2 &optional TESTFN)") (autoload 'seq-group-by "seq-25" "Apply FUNCTION to each element of SEQUENCE.
Separate the elements of SEQUENCE into an alist using the results as
keys.  Keys are compared using `equal'.

(fn FUNCTION SEQUENCE)") (autoload 'seq-max "seq-25" "Return the largest element of SEQUENCE.
SEQUENCE must be a sequence of numbers or markers.

(fn SEQUENCE)") (autoload 'seq-random-elt "seq-25" "Return a randomly chosen element from SEQUENCE.
Signal an error if SEQUENCE is empty.

(fn SEQUENCE)") (register-definition-prefixes "seq-25" '("seq-")) (provide 'seq-autoloads)) "compat" ((compat-30 compat-pkg compat-25 compat-28 compat compat-29 compat-macs compat-autoloads compat-27 compat-26) (register-definition-prefixes "compat" '("compat-")) (register-definition-prefixes "compat-macs" '("compat-")) (provide 'compat-autoloads)) "no-littering" ((no-littering no-littering-autoloads) (autoload 'no-littering-expand-etc-file-name "no-littering" "Expand filename FILE relative to `no-littering-etc-directory'.

(fn FILE)") (autoload 'no-littering-expand-var-file-name "no-littering" "Expand filename FILE relative to `no-littering-var-directory'.

(fn FILE)") (autoload 'no-littering-theme-backups "no-littering" "Theme locations where backups of various sorts are created.

The purpose of this package is to store data files of various
sorts in a handful of central locations, instead of spreading
them all over the place.  When doing that for temporary files,
which contain backups of some sort, that increases the odds that
sensitive data is written to disk in clear text and/or that such
clear text files persist longer, if they would be created anyway.

Because of that, simply loading `no-littering' does not theme
certain, potentially unsafe variables.  Instead, this function is
provided, so that you can decide whether to take the risk or not.

Calling this function sets these variables:
- `auto-save-file-name-transforms' (built-in)
- `backup-directory-alist' (built-in)
- `undo-tree-history-directory-alist' (from `undo-tree')

The default values of these variables cause additional files to
be created in the same directories as the files that are being
visited.  Calling this function changes the values of these
variables, so that this is only done for visited files located in
certain directories.  For all other visited files, the additional
files are created in files inside `no-littering-var-directory'.

Additional files are created in the same directory as the visited
file, for files located in:
- \"/tmp/\"
- \"/dev/shm\"
- `temporary-file-directory'

With these settings it is still possible that sensitive data is
written to additional files, but you are more likely to spot it,
and because these directories usually use a `tmpfs' file-system,
the leaked secrets should not persist after a reboot.

If you do *not* call this function, then these additional files
are always created in the same directory as the visited files,
regardless of the location of the visited files.  In other words,
even when using the default values, there is a significant risk
of leaking sensitive data, and if you want to reduce that, then
you must turn of these features completely.") (register-definition-prefixes "no-littering" '("emacs-session-filename" "no-littering-")) (provide 'no-littering-autoloads)) "org" ((ox-icalendar ob-matlab org-fold org-macro ol-info ob-lilypond ox-ascii ob-haskell ol-bbdb ob-emacs-lisp ob-makefile ox-md org-lint oc-natbib org-persist ol-mhe org-goto org-compat org-attach-git ox-man org-keys org-tempo org-mouse ob-perl ob-java org-plot org-footnote org-attach org-crypt org-element org ob-sed ol-w3m ob-ruby org-colview ob-table ob-lob org-datetree ol-docview org-loaddefs ol-irc ob-comint org-list ob-C org-inlinetask oc ol-rmail ol-eww ob-gnuplot ob-screen ob-calc org-mobile org-duration ob-js org-src org-faces ob-plantuml org-timer oc-basic ob-R ob-sass oc-bibtex ob-dot ob-shell org-macs org-version ob-tangle ob-exp ob-core ol-gnus ol-man ob-lisp ol-eshell ob-fortran org-fold-core org-archive org-agenda ox-html org-id ox-org ob-sql org-element-ast org-num ox-texinfo ox-beamer ob-clojure ol-bibtex ox ox-odt ob-eshell ob-octave oc-csl ox-publish ob-forth org-table ox-koma-letter ob-python ob-maxima org-cycle org-indent ob-groovy ob ol ob-lua org-pcomplete ob-ref ob-ditaa org-habit org-entities org-clock org-ctags org-capture ob-processing ob-ocaml ob-sqlite ox-latex org-protocol org-refile ob-julia ob-org ob-css ob-latex oc-biblatex ol-doi ob-awk org-feed ob-eval ob-scheme)) "xref" ((xref-autoloads xref xref-pkg) (autoload 'xref-find-backend "xref") (define-obsolete-function-alias 'xref-pop-marker-stack #'xref-go-back "29.1") (autoload 'xref-go-back "xref" "Go back to the previous position in xref history.
To undo, use \\[xref-go-forward]." t) (autoload 'xref-go-forward "xref" "Go to the point where a previous \\[xref-go-back] was invoked." t) (autoload 'xref-marker-stack-empty-p "xref" "Whether the xref back-history is empty.") (autoload 'xref-forward-history-empty-p "xref" "Whether the xref forward-history is empty.") (autoload 'xref-show-xrefs "xref" "Display some Xref values produced by FETCHER using DISPLAY-ACTION.
The meanings of both arguments are the same as documented in
`xref-show-xrefs-function'.

(fn FETCHER DISPLAY-ACTION)") (autoload 'xref-find-definitions "xref" "Find the definition of the identifier at point.
With prefix argument or when there's no identifier at point,
prompt for it.

If sufficient information is available to determine a unique
definition for IDENTIFIER, display it in the selected window.
Otherwise, display the list of the possible definitions in a
buffer where the user can select from the list.

Use \\[xref-go-back] to return back to where you invoked this command.

(fn IDENTIFIER)" t) (autoload 'xref-find-definitions-other-window "xref" "Like `xref-find-definitions' but switch to the other window.

(fn IDENTIFIER)" t) (autoload 'xref-find-definitions-other-frame "xref" "Like `xref-find-definitions' but switch to the other frame.

(fn IDENTIFIER)" t) (autoload 'xref-find-references "xref" "Find references to the identifier at point.
This command might prompt for the identifier as needed, perhaps
offering the symbol at point as the default.
With prefix argument, or if `xref-prompt-for-identifier' is t,
always prompt for the identifier.  If `xref-prompt-for-identifier'
is nil, prompt only if there's no usable symbol at point.

(fn IDENTIFIER)" t) (autoload 'xref-find-definitions-at-mouse "xref" "Find the definition of identifier at or around mouse click.
This command is intended to be bound to a mouse event.

(fn EVENT)" t) (autoload 'xref-find-references-at-mouse "xref" "Find references to the identifier at or around mouse click.
This command is intended to be bound to a mouse event.

(fn EVENT)" t) (autoload 'xref-find-apropos "xref" "Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'.
See `tags-apropos-additional-actions' for how to augment the
output of this command when the backend is etags.

(fn PATTERN)" t) (define-key esc-map "." #'xref-find-definitions) (define-key esc-map "," #'xref-go-back) (define-key esc-map [67108908] #'xref-go-forward) (define-key esc-map "?" #'xref-find-references) (define-key esc-map [67108910] #'xref-find-apropos) (define-key ctl-x-4-map "." #'xref-find-definitions-other-window) (define-key ctl-x-5-map "." #'xref-find-definitions-other-frame) (autoload 'xref-references-in-directory "xref" "Find all references to SYMBOL in directory DIR.
Return a list of xref values.

This function uses the Semantic Symbol Reference API, see
`semantic-symref-tool-alist' for details on which tools are used,
and when.

(fn SYMBOL DIR)") (autoload 'xref-matches-in-directory "xref" "Find all matches for REGEXP in directory DIR.
Return a list of xref values.
Only files matching some of FILES and none of IGNORES are searched.
FILES is a string with glob patterns separated by spaces.
IGNORES is a list of glob patterns for files to ignore.

(fn REGEXP FILES DIR IGNORES)") (autoload 'xref-matches-in-files "xref" "Find all matches for REGEXP in FILES.
Return a list of xref values.
FILES must be a list of absolute file names.

See `xref-search-program' and `xref-search-program-alist' for how
to control which program to use when looking for matches.

(fn REGEXP FILES)") (register-definition-prefixes "xref" '("xref-")) (provide 'xref-autoloads)) "project" ((project-autoloads project project-pkg) (autoload 'project-current "project" "Return the project instance in DIRECTORY, defaulting to `default-directory'.

When no project is found in that directory, the result depends on
the value of MAYBE-PROMPT: if it is nil or omitted, return nil,
else prompt the user for the project to use.  To prompt for a
project, call the function specified by `project-prompter', which
returns the directory in which to look for the project.  If no
project is found in that directory, return a \"transient\"
project instance.

The \"transient\" project instance is a special kind of value
which denotes a project rooted in that directory and includes all
the files under the directory except for those that match entries
in `vc-directory-exclusion-list' or `grep-find-ignored-files'.

See the doc string of `project-find-functions' for the general form
of the project instance object.

(fn &optional MAYBE-PROMPT DIRECTORY)") (put 'project-vc-ignores 'safe-local-variable (lambda (val) (and (listp val) (not (memq nil (mapcar #'stringp val)))))) (put 'project-vc-merge-submodules 'safe-local-variable #'booleanp) (put 'project-vc-include-untracked 'safe-local-variable #'booleanp) (put 'project-vc-name 'safe-local-variable #'stringp) (put 'project-vc-extra-root-markers 'safe-local-variable (lambda (val) (and (listp val) (not (memq nil (mapcar #'stringp val)))))) (defvar project-prefix-map (let ((map (make-sparse-keymap))) (define-key map "!" 'project-shell-command) (define-key map "&" 'project-async-shell-command) (define-key map "f" 'project-find-file) (define-key map "F" 'project-or-external-find-file) (define-key map "b" 'project-switch-to-buffer) (define-key map "s" 'project-shell) (define-key map "d" 'project-find-dir) (define-key map "D" 'project-dired) (define-key map "v" 'project-vc-dir) (define-key map "c" 'project-compile) (define-key map "e" 'project-eshell) (define-key map "k" 'project-kill-buffers) (define-key map "p" 'project-switch-project) (define-key map "g" 'project-find-regexp) (define-key map "G" 'project-or-external-find-regexp) (define-key map "r" 'project-query-replace-regexp) (define-key map "x" 'project-execute-extended-command) (define-key map "o" 'project-any-command) (define-key map "" 'project-list-buffers) map) "Keymap for project commands.") (define-key ctl-x-map "p" project-prefix-map) (autoload 'project-other-window-command "project" "Run project command, displaying resultant buffer in another window.

The following commands are available:

\\{project-prefix-map}
\\{project-other-window-map}" t) (define-key ctl-x-4-map "p" #'project-other-window-command) (autoload 'project-other-frame-command "project" "Run project command, displaying resultant buffer in another frame.

The following commands are available:

\\{project-prefix-map}
\\{project-other-frame-map}" t) (define-key ctl-x-5-map "p" #'project-other-frame-command) (autoload 'project-other-tab-command "project" "Run project command, displaying resultant buffer in a new tab.

The following commands are available:

\\{project-prefix-map}" t) (when (bound-and-true-p tab-prefix-map) (define-key tab-prefix-map "p" #'project-other-tab-command)) (autoload 'project-find-regexp "project" "Find all matches for REGEXP in the current project's roots.
With \\[universal-argument] prefix, you can specify the directory
to search in, and the file name pattern to search for.  The
pattern may use abbreviations defined in `grep-files-aliases',
e.g. entering `ch' is equivalent to `*.[ch]'.  As whitespace
triggers completion when entering a pattern, including it
requires quoting, e.g. `\\[quoted-insert]<space>'.

(fn REGEXP)" t) (autoload 'project-or-external-find-regexp "project" "Find all matches for REGEXP in the project roots or external roots.

(fn REGEXP)" t) (autoload 'project-find-file "project" "Visit a file (with completion) in the current project.

The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\".  If none, the current
buffer's file name is used.

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files under the project root, except
for VCS directories listed in `vc-directory-exclusion-list'.

(fn &optional INCLUDE-ALL)" t) (autoload 'project-or-external-find-file "project" "Visit a file (with completion) in the current project or external roots.

The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\".  If none, the current
buffer's file name is used.

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files under the project root, except
for VCS directories listed in `vc-directory-exclusion-list'.

(fn &optional INCLUDE-ALL)" t) (autoload 'project-find-dir "project" "Start Dired in a directory inside the current project.

The current buffer's `default-directory' is available as part of
\"future history\"." t) (autoload 'project-dired "project" "Start Dired in the current project's root." t) (autoload 'project-vc-dir "project" "Run VC-Dir in the current project's root." t) (autoload 'project-shell "project" "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists." t) (autoload 'project-eshell "project" "Start Eshell in the current project's root directory.
If a buffer already exists for running Eshell in the project's root,
switch to it.  Otherwise, create a new Eshell buffer.
With \\[universal-argument] prefix arg, create a new Eshell buffer even
if one already exists." t) (autoload 'project-async-shell-command "project" "Run `async-shell-command' in the current project's root directory." t) (function-put 'project-async-shell-command 'interactive-only 'async-shell-command) (autoload 'project-shell-command "project" "Run `shell-command' in the current project's root directory." t) (function-put 'project-shell-command 'interactive-only 'shell-command) (autoload 'project-search "project" "Search for REGEXP in all the files of the project.
Stops when a match is found.
To continue searching for the next match, use the
command \\[fileloop-continue].

(fn REGEXP)" t) (autoload 'project-query-replace-regexp "project" "Query-replace REGEXP in all the files of the project.
Stops when a match is found and prompts for whether to replace it.
At that prompt, the user must type a character saying what to do
with the match.  Type SPC or `y' to replace the match,
DEL or `n' to skip and go to the next match.  For more directions,
type \\[help-command] at that time.
If you exit the `query-replace', you can later continue the
`query-replace' loop using the command \\[fileloop-continue].

(fn FROM TO)" t) (autoload 'project-compile "project" "Run `compile' in the project root." t) (function-put 'project-compile 'interactive-only 'compile) (autoload 'project-switch-to-buffer "project" "Display buffer BUFFER-OR-NAME in the selected window.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

(fn BUFFER-OR-NAME)" t) (autoload 'project-display-buffer "project" "Display BUFFER-OR-NAME in some window, without selecting it.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer' as a subroutine, which see
for how it is determined where the buffer will be displayed.

(fn BUFFER-OR-NAME)" t) (autoload 'project-display-buffer-other-frame "project" "Display BUFFER-OR-NAME preferably in another frame.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer-other-frame' as a subroutine,
which see for how it is determined where the buffer will be
displayed.

(fn BUFFER-OR-NAME)" t) (autoload 'project-list-buffers "project" "Display a list of project buffers.
The list is displayed in a buffer named \"*Buffer List*\".

By default, all project buffers are listed except those whose names
start with a space (which are for internal use).  With prefix argument
ARG, show only buffers that are visiting files.

(fn &optional ARG)" t) (put 'project-kill-buffers-display-buffer-list 'safe-local-variable #'booleanp) (autoload 'project-kill-buffers "project" "Kill the buffers belonging to the current project.
Two buffers belong to the same project if their project
instances, as reported by `project-current' in each buffer, are
identical.  Only the buffers that match a condition in
`project-kill-buffer-conditions' will be killed.  If NO-CONFIRM
is non-nil, the command will not ask the user for confirmation.
NO-CONFIRM is always nil when the command is invoked
interactively.

Also see the `project-kill-buffers-display-buffer-list' variable.

(fn &optional NO-CONFIRM)" t) (autoload 'project-remember-project "project" "Add project PR to the front of the project list.
Save the result in `project-list-file' if the list of projects
has changed, and NO-WRITE is nil.

(fn PR &optional NO-WRITE)") (autoload 'project-forget-project "project" "Remove directory PROJECT-ROOT from the project list.
PROJECT-ROOT is the root directory of a known project listed in
the project list.

(fn PROJECT-ROOT)" t) (autoload 'project-known-project-roots "project" "Return the list of root directories of all known projects.") (autoload 'project-execute-extended-command "project" "Execute an extended command in project root." t) (function-put 'project-execute-extended-command 'interactive-only 'command-execute) (autoload 'project-any-command "project" "Run the next command in the current project.

If the command name starts with `project-', or its symbol has
property `project-aware', it gets passed the project to use
with the variable `project-current-directory-override'.
Otherwise, `default-directory' is temporarily set to the current
project's root.

If OVERRIDING-MAP is non-nil, it will be used as
`overriding-terminal-local-map' to provide shorter bindings
from that map which will take priority over the global ones.

(fn &optional OVERRIDING-MAP PROMPT-FORMAT)" t) (autoload 'project-prefix-or-any-command "project" "Run the next command in the current project.
Works like `project-any-command', but also mixes in the shorter
bindings from `project-prefix-map'." t) (autoload 'project-switch-project "project" "\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'.

When called in a program, it will use the project corresponding
to directory DIR.

(fn DIR)" t) (autoload 'project-uniquify-dirname-transform "project" "Uniquify name of directory DIRNAME using `project-name', if in a project.

If you set `uniquify-dirname-transform' to this function,
slash-separated components from `project-name' will be appended to
the buffer's directory name when buffers from two different projects
would otherwise have the same name.

(fn DIRNAME)") (defvar project-mode-line nil "Whether to show current project name and Project menu on the mode line.
This feature requires the presence of the following item in
`mode-line-format': `(project-mode-line project-mode-line-format)'; it
is part of the default mode line beginning with Emacs 30.") (custom-autoload 'project-mode-line "project" t) (register-definition-prefixes "project" '("project-")) (provide 'project-autoloads)) "eldoc" ((eldoc eldoc-pkg eldoc-autoloads) (defvar eldoc-minor-mode-string (purecopy " ElDoc") "String to display in mode line when ElDoc Mode is enabled; nil for none.") (custom-autoload 'eldoc-minor-mode-string "eldoc" t) (autoload 'eldoc-mode "eldoc" "Toggle echo area display of Lisp objects at point (ElDoc mode).

ElDoc mode is a buffer-local minor mode.  When enabled, the echo
area displays information about a function or variable in the
text where point is.  If point is on a documented variable, it
displays the first line of that variable's doc string.  Otherwise
it displays the argument list of the function called in the
expression point is on.

This is a minor mode.  If called interactively, toggle the `Eldoc
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `eldoc-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-eldoc-mode 'globalized-minor-mode t) (defcustom global-eldoc-mode t "Non-nil if Global Eldoc mode is enabled.
See the `global-eldoc-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-eldoc-mode'." :set #'custom-set-minor-mode :initialize 'custom-initialize-delay :type 'boolean) (custom-autoload 'global-eldoc-mode "eldoc" nil) (autoload 'global-eldoc-mode "eldoc" "Toggle Eldoc mode in all buffers.
With prefix ARG, enable Global Eldoc mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Eldoc mode is enabled in all buffers where `turn-on-eldoc-mode' would
do it.

See `eldoc-mode' for more information on Eldoc mode.

(fn &optional ARG)" t) (autoload 'turn-on-eldoc-mode "eldoc" "Turn on `eldoc-mode' if the buffer has ElDoc support enabled.
See `eldoc-documentation-strategy' for more detail.") (register-definition-prefixes "eldoc" '("eldoc")) (provide 'eldoc-autoloads)) "flymake" ((flymake-pkg flymake-autoloads flymake) (autoload 'flymake-log "flymake" "Log, at level LEVEL, the message MSG formatted with ARGS.
LEVEL is passed to `display-warning', which is used to display
the warning.  If this form is included in a file,
the generated warning contains an indication of the file that
generated it.

(fn LEVEL MSG &rest ARGS)" nil t) (autoload 'flymake-make-diagnostic "flymake" "Make a Flymake diagnostic for LOCUS's region from BEG to END.
LOCUS is a buffer object or a string designating a file name.

TYPE is a diagnostic symbol and TEXT is string describing the
problem detected in this region.  DATA is any object that the
caller wishes to attach to the created diagnostic for later
retrieval with `flymake-diagnostic-data'.

If LOCUS is a buffer BEG and END should be buffer positions
inside it.  If LOCUS designates a file, BEG and END should be a
cons (LINE . COL) indicating a file position.  In this second
case, END may be omitted in which case the region is computed
using `flymake-diag-region' if the diagnostic is appended to an
actual buffer.

OVERLAY-PROPERTIES is an alist of properties attached to the
created diagnostic, overriding the default properties and any
properties listed in the `flymake-overlay-control' property of
the diagnostic's type symbol.

(fn LOCUS BEG END TYPE TEXT &optional DATA OVERLAY-PROPERTIES)") (autoload 'flymake-diagnostics "flymake" "Get Flymake diagnostics in region determined by BEG and END.

If neither BEG or END is supplied, use whole accessible buffer,
otherwise if BEG is non-nil and END is nil, consider only
diagnostics at BEG.

(fn &optional BEG END)") (autoload 'flymake-diag-region "flymake" "Compute BUFFER's region (BEG . END) corresponding to LINE and COL.
If COL is nil, return a region just for LINE.  Return nil if the
region is invalid.  This function saves match data.

(fn BUFFER LINE &optional COL)") (autoload 'flymake-mode "flymake" "Toggle Flymake mode on or off.

Flymake is an Emacs minor mode for on-the-fly syntax checking.
Flymake collects diagnostic information from multiple sources,
called backends, and visually annotates the buffer with the
results.

Flymake performs these checks while the user is editing.
The customization variables `flymake-start-on-flymake-mode',
`flymake-no-changes-timeout' determine the exact circumstances
whereupon Flymake decides to initiate a check of the buffer.

The commands `flymake-goto-next-error' and
`flymake-goto-prev-error' can be used to navigate among Flymake
diagnostics annotated in the buffer.

By default, `flymake-mode' doesn't override the \\[next-error] command, but
if you're using Flymake a lot (and don't use the regular compilation
mechanisms that often), it can be useful to put something like
the following in your init file:

  (setq next-error-function \\='flymake-goto-next-error)

The visual appearance of each type of diagnostic can be changed
by setting properties `flymake-overlay-control', `flymake-bitmap'
and `flymake-severity' on the symbols of diagnostic types (like
`:error', `:warning' and `:note').

Activation or deactivation of backends used by Flymake in each
buffer happens via the special hook
`flymake-diagnostic-functions'.

Some backends may take longer than others to respond or complete,
and some may decide to disable themselves if they are not
suitable for the current buffer.  The commands
`flymake-running-backends', `flymake-disabled-backends' and
`flymake-reporting-backends' summarize the situation, as does the
special *Flymake log* buffer.

This is a minor mode.  If called interactively, toggle the
`Flymake mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `flymake-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'flymake-mode-on "flymake" "Turn Flymake mode on.") (autoload 'flymake-mode-off "flymake" "Turn Flymake mode off.") (register-definition-prefixes "flymake" '("flymake-")) (provide 'flymake-autoloads)) "with-editor" ((with-editor with-editor-autoloads) (autoload 'with-editor-export-editor "with-editor" "Teach subsequent commands to use current Emacs instance as editor.

Set and export the environment variable ENVVAR, by default
\"EDITOR\".  The value is automatically generated to teach
commands to use the current Emacs instance as \"the editor\".

This works in `shell-mode', `term-mode', `eshell-mode' and
`vterm'.

(fn &optional (ENVVAR \"EDITOR\"))" t) (autoload 'with-editor-export-git-editor "with-editor" "Like `with-editor-export-editor' but always set `$GIT_EDITOR'." t) (autoload 'with-editor-export-hg-editor "with-editor" "Like `with-editor-export-editor' but always set `$HG_EDITOR'." t) (defvar shell-command-with-editor-mode nil "Non-nil if Shell-Command-With-Editor mode is enabled.
See the `shell-command-with-editor-mode' command
for a description of this minor mode.") (custom-autoload 'shell-command-with-editor-mode "with-editor" nil) (autoload 'shell-command-with-editor-mode "with-editor" "Teach `shell-command' to use current Emacs instance as editor.

Teach `shell-command', and all commands that ultimately call that
command, to use the current Emacs instance as editor by executing
\"EDITOR=CLIENT COMMAND&\" instead of just \"COMMAND&\".

CLIENT is automatically generated; EDITOR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming no other variable overrides the effect of \"$EDITOR\".
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Alternatively you can use the `with-editor-async-shell-command',
which also allows the use of another variable instead of
\"EDITOR\".

This is a global minor mode.  If called interactively, toggle the
`Shell-Command-With-Editor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='shell-command-with-editor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'with-editor-async-shell-command "with-editor" "Like `async-shell-command' but with `$EDITOR' set.

Execute string \"ENVVAR=CLIENT COMMAND\" in an inferior shell;
display output, if any.  With a prefix argument prompt for an
environment variable, otherwise the default \"EDITOR\" variable
is used.  With a negative prefix argument additionally insert
the COMMAND's output at point.

CLIENT is automatically generated; ENVVAR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming it respects ENVVAR as an \"EDITOR\"-like variable.
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Also see `async-shell-command' and `shell-command'.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t) (autoload 'with-editor-shell-command "with-editor" "Like `shell-command' or `with-editor-async-shell-command'.
If COMMAND ends with \"&\" behave like the latter,
else like the former.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t) (register-definition-prefixes "with-editor" '("server-" "shell-command" "start-file-process" "with-editor")) (provide 'with-editor-autoloads)) "password-store" ((password-store-autoloads password-store) (autoload 'password-store-edit "password-store" "Edit password for ENTRY.

(fn ENTRY)" t) (autoload 'password-store-get "password-store" "Return password for ENTRY.

Returns the first line of the password data.  When CALLBACK is
non-`NIL', call CALLBACK with the first line instead.

(fn ENTRY &optional CALLBACK)") (autoload 'password-store-get-field "password-store" "Return FIELD for ENTRY.
FIELD is a string, for instance \"url\".  When CALLBACK is
non-`NIL', call it with the line associated to FIELD instead.  If
FIELD equals to symbol secret, then this function reduces to
`password-store-get'.

(fn ENTRY FIELD &optional CALLBACK)") (autoload 'password-store-clear "password-store" "Clear secret in the kill ring.

Optional argument FIELD, a symbol or a string, describes the
stored secret to clear; if nil, then set it to 'secret.  Note,
FIELD does not affect the function logic; it is only used to
display the message:

(message \"Field %s cleared from kill ring and system clipboard.\" field).

(fn &optional FIELD)" t) (autoload 'password-store-copy "password-store" "Add password for ENTRY into the kill ring.

Clear previous password from the kill ring.  Pointer to the kill
ring is stored in `password-store-kill-ring-pointer'.  Password
is cleared after `password-store-time-before-clipboard-restore'
seconds.

(fn ENTRY)" t) (autoload 'password-store-copy-field "password-store" "Add FIELD for ENTRY into the kill ring.

Clear previous secret from the kill ring.  Pointer to the kill
ring is stored in `password-store-kill-ring-pointer'.  Secret
field is cleared after
`password-store-time-before-clipboard-restore' seconds.  If FIELD
equals to symbol secret, then this function reduces to
`password-store-copy'.

(fn ENTRY FIELD)" t) (autoload 'password-store-init "password-store" "Initialize new password store and use GPG-ID for encryption.

Separate multiple IDs with spaces.

(fn GPG-ID)" t) (autoload 'password-store-insert "password-store" "Insert a new ENTRY containing PASSWORD.

(fn ENTRY PASSWORD)" t) (autoload 'password-store-generate "password-store" "Generate a new password for ENTRY with PASSWORD-LENGTH.

Default PASSWORD-LENGTH is `password-store-password-length'.

(fn ENTRY &optional PASSWORD-LENGTH)" t) (autoload 'password-store-generate-no-symbols "password-store" "Generate a new password without symbols for ENTRY with PASSWORD-LENGTH.

Default PASSWORD-LENGTH is `password-store-password-length'.

(fn ENTRY &optional PASSWORD-LENGTH)" t) (autoload 'password-store-remove "password-store" "Remove ENTRY.

(fn ENTRY)" t) (autoload 'password-store-rename "password-store" "Rename ENTRY to NEW-ENTRY.

(fn ENTRY NEW-ENTRY)" t) (autoload 'password-store-version "password-store" "Show version of `password-store-executable'." t) (autoload 'password-store-url "password-store" "Load URL for ENTRY.

(fn ENTRY)" t) (register-definition-prefixes "password-store" '("password-store-")) (provide 'password-store-autoloads)) "s" ((s-autoloads s) (register-definition-prefixes "s" '("s-")) (provide 's-autoloads)) "password-store-otp" ((password-store-otp-autoloads password-store-otp) (autoload 'password-store-otp-token-copy "password-store-otp" "Copy an OTP token from ENTRY to clipboard.

(fn ENTRY)" t) (autoload 'password-store-otp-uri-copy "password-store-otp" "Copy an OTP URI from ENTRY to clipboard.

(fn ENTRY)" t) (autoload 'password-store-otp-insert "password-store-otp" "Insert a new ENTRY containing OTP-URI.

(fn ENTRY OTP-URI)" t) (autoload 'password-store-otp-append "password-store-otp" "Append to an ENTRY the given OTP-URI.

(fn ENTRY OTP-URI)" t) (autoload 'password-store-otp-append-from-image "password-store-otp" "Check clipboard for an image and scan it to get an OTP URI, append it to ENTRY.

(fn ENTRY)" t) (register-definition-prefixes "password-store-otp" '("password-store-otp-")) (provide 'password-store-otp-autoloads)) "dash" ((dash dash-autoloads) (autoload 'dash-fontify-mode "dash" "Toggle fontification of Dash special variables.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'.

This is a minor mode.  If called interactively, toggle the
`Dash-Fontify mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `dash-fontify-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-dash-fontify-mode 'globalized-minor-mode t) (defvar global-dash-fontify-mode nil "Non-nil if Global Dash-Fontify mode is enabled.
See the `global-dash-fontify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dash-fontify-mode'.") (custom-autoload 'global-dash-fontify-mode "dash" nil) (autoload 'global-dash-fontify-mode "dash" "Toggle Dash-Fontify mode in all buffers.
With prefix ARG, enable Global Dash-Fontify mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Dash-Fontify mode is enabled in all buffers where `dash--turn-on-fontify-mode' would do it.

See `dash-fontify-mode' for more information on Dash-Fontify mode.

(fn &optional ARG)" t) (autoload 'dash-register-info-lookup "dash" "Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol]." t) (register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-juxt" "-keep" "-l" "-m" "-no" "-o" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-")) (provide 'dash-autoloads)) "f" ((f-shortdoc f f-autoloads) (register-definition-prefixes "f" '("f-")) (provide 'f-autoloads)) "pass" ((pass-autoloads pass) (autoload 'pass "pass" "Open the password-store buffer." t) (register-definition-prefixes "pass" '("pass-")) (provide 'pass-autoloads)) "gif-screencast" ((gif-screencast gif-screencast-autoloads) (autoload 'gif-screencast "gif-screencast" "Start recording the GIF.
A screenshot is taken before every command runs." t) (register-definition-prefixes "gif-screencast" '("gif-screencast-")) (provide 'gif-screencast-autoloads)) "popup" ((popup popup-autoloads) (register-definition-prefixes "popup" '("popup-")) (provide 'popup-autoloads)) "google-translate" ((google-translate-default-ui google-translate-core-ui google-translate-autoloads google-translate-backend google-translate google-translate-smooth-ui google-translate-core) (register-definition-prefixes "google-translate-backend" '("google-translate-backend-")) (register-definition-prefixes "google-translate-core" '("google-translate-")) (register-definition-prefixes "google-translate-core-ui" '("google-translate-")) (autoload 'google-translate-query-translate "google-translate-default-ui" "Interactively translate text with Google Translate.

Query a text (a word or a phrase), and pop up a buffer named *Google
Translate* displaying available translations of the text.

If no defaults for the source and target languages are specified (by
setting the variables `google-translate-default-source-language' and
`google-translate-default-target-language'), interactively query the
missing parts.  For example, a reasonable option may be to specify a
default for the target language and always be queried for the source
language.

With a `C-u' prefix argument, query the source and target languages,
even if any defaults are specified.  For example, you may frequently
need to translate from English to Russian, and you may choose to set
the default source and target languages to \"en\" and  \"ru\", resp.
However, occasionally you may also need to translate from Russian to
English.  With a `C-u' prefix argument you can override the defaults
and specify the source and target languages explicitly.

The languages are queried with completion, and the null input at the
source language prompt is considered as an instruction for Google
Translate to detect the source language.

(fn &optional OVERRIDE-P)" t) (autoload 'google-translate-query-translate-reverse "google-translate-default-ui" "Like `google-translate-query-translate', but performs translation
in the reverse direction.

The value of the variable `google-translate-default-source-language'
(if set) becomes the target language, and the value of the variable
`google-translate-default-target-language' (if also set) becomes the
source language.

In particular, when both variables are set, translation is performed
in the reverse direction.

(fn &optional OVERRIDE-P)" t) (autoload 'google-translate-at-point "google-translate-default-ui" "Translate the word at point or the words in the active region.

For the meaning of OVERRIDE-P, see `google-translate-query-translate'.

(fn &optional OVERRIDE-P)" t) (autoload 'google-translate-at-point-reverse "google-translate-default-ui" "Like `google-translate-at-point', but performs translation in the
reverse direction.

(fn &optional OVERRIDE-P)" t) (autoload 'google-translate-buffer "google-translate-default-ui" "Translate current buffer.

For the meaning of OVERRIDE-P, see `google-translate-query-translate'.

(fn &optional OVERRIDE-P REVERSE-P)" t) (autoload 'google-translate-paragraphs-overlay "google-translate-default-ui" "Translate current buffer with paragraph by paragraph and SHOW results in overlay below paragraph.
This command also specificly support org-mode.

(fn &optional OVERRIDE-P REVERSE-P)" t) (autoload 'google-translate-paragraphs-insert "google-translate-default-ui" "Translate current buffer with paragraph by paragraph and INSERT results below paragraph.
This command does NOT support document format like org-mode.

(fn &optional OVERRIDE-P REVERSE-P)" t) (register-definition-prefixes "google-translate-default-ui" '("%google-translate-" "google-translate-")) (autoload 'google-translate-smooth-translate "google-translate-smooth-ui" "Translate a text using translation directions.

Make a prompt in minibuffer for a text to translate. Default text
is word at point.

In case of `google-translate-translation-directions-alist' is
empty list then after inputed translating text prompts for source
language and then for target languages.

In case of `google-translate-translation-directions-alist' is not
empty list takes current translation direction and makes
appropriate translation. Current translation direction indicates
in the minibuffers' prompt.

A current translation direction could be changed directly in the
minibuffer by means of key bindings such as C-n and C-p for
changing to the next translation direction and to the previous
one respectively." t) (register-definition-prefixes "google-translate-smooth-ui" '("google-translate-")) (provide 'google-translate-autoloads)) "gntp" ((gntp gntp-autoloads) (autoload 'gntp-notify "gntp" "Send notification NAME with TITLE, TEXT, PRIORITY and ICON to SERVER:PORT.
PORT defaults to `gntp-server-port'

(fn NAME TITLE TEXT SERVER &optional PORT PRIORITY ICON)") (register-definition-prefixes "gntp" '("gntp-")) (provide 'gntp-autoloads)) "log4e" ((log4e log4e-autoloads) (autoload 'log4e-mode "log4e" "Major mode for browsing a buffer made by log4e.

\\<log4e-mode-map>
\\{log4e-mode-map}

(fn)" t) (autoload 'log4e:insert-start-log-quickly "log4e" "Insert logging statment for trace level log at start of current function/macro." t) (register-definition-prefixes "log4e" '("log4e")) (provide 'log4e-autoloads)) "alert" ((alert-autoloads alert) (autoload 'alert-add-rule "alert" "Programmatically add an alert configuration rule.

Normally, users should custoimze `alert-user-configuration'.
This facility is for module writers and users that need to do
things the Lisp way.

Here is a rule the author currently uses with ERC, so that the
fringe gets colored whenever people chat on BitlBee:

(alert-add-rule :status   \\='(buried visible idle)
                :severity \\='(moderate high urgent)
                :mode     \\='erc-mode
                :predicate
                #\\='(lambda (info)
                    (string-match (concat \"\\\\`[^&].*@BitlBee\\\\\\='\")
                                  (erc-format-target-and/or-network)))
                :persistent
                #\\='(lambda (info)
                    ;; If the buffer is buried, or the user has been
                    ;; idle for `alert-reveal-idle-time' seconds,
                    ;; make this alert persistent.  Normally, alerts
                    ;; become persistent after
                    ;; `alert-persist-idle-time' seconds.
                    (memq (plist-get info :status) \\='(buried idle)))
                :style \\='fringe
                :continue t)

(fn &key SEVERITY STATUS MODE CATEGORY TITLE MESSAGE PREDICATE ICON (STYLE alert-default-style) PERSISTENT CONTINUE NEVER-PERSIST APPEND)") (autoload 'alert "alert" "Alert the user that something has happened.
MESSAGE is what the user will see.  You may also use keyword
arguments to specify additional details.  Here is a full example:

(alert \"This is a message\"
       :severity \\='high            ;; The default severity is `normal'
       :title \"Title\"              ;; An optional title
       :category \\='example         ;; A symbol to identify the message
       :mode \\='text-mode           ;; Normally determined automatically
       :buffer (current-buffer)      ;; This is the default
       :data nil                     ;; Unused by alert.el itself
       :persistent nil               ;; Force the alert to be persistent;
                                     ;; it is best not to use this
       :never-persist nil            ;; Force this alert to never persist
       :id \\='my-id)                ;; Used to replace previous message of
                                     ;; the same id in styles that support it
       :style \\='fringe)            ;; Force a given style to be used;
                                     ;; this is only for debugging!
       :icon \\=\"mail-message-new\" ;; if style supports icon then add icon
                                     ;; name or path here

If no :title is given, the buffer-name of :buffer is used.  If
:buffer is nil, it is the current buffer at the point of call.

:data is an opaque value which modules can pass through to their
own styles if they wish.

Here are some more typical examples of usage:

  ;; This is the most basic form usage
  (alert \"This is an alert\")

  ;; You can adjust the severity for more important messages
  (alert \"This is an alert\" :severity \\='high)

  ;; Or decrease it for purely informative ones
  (alert \"This is an alert\" :severity \\='trivial)

  ;; Alerts can have optional titles.  Otherwise, the title is the
  ;; buffer-name of the (current-buffer) where the alert originated.
  (alert \"This is an alert\" :title \"My Alert\")

  ;; Further, alerts can have categories.  This allows users to
  ;; selectively filter on them.
  (alert \"This is an alert\" :title \"My Alert\"
         :category \\='some-category-or-other)

(fn MESSAGE &key (SEVERITY \\='normal) TITLE ICON CATEGORY BUFFER MODE DATA STYLE PERSISTENT NEVER-PERSIST ID)") (register-definition-prefixes "alert" '("alert-" "x-urgen")) (provide 'alert-autoloads)) "lazy-lang-learn" ((lazy-lang-learn lazy-lang-learn-autoloads) (autoload 'lazy-lang-learn-new "lazy-lang-learn" "Create a new snippet with `lazy-lang-learn--function'.
Display with `lazy-lang-learn--display-function' and
save it in `lazy-lang-learn--history-file'." t) (autoload 'lazy-lang-learn-swap-languages "lazy-lang-learn" nil t) (autoload 'lazy-lang-learn-translate "lazy-lang-learn" "Translate LEARN or if PREFIX then random element in `lazy-lang-learn--history'.

(fn &optional LEARN)" t) (autoload 'lazy-lang-learn-translate-random "lazy-lang-learn" "Translate  random element in `lazy-lang-learn--history'." t) (autoload 'lazy-lang-learn-translate-from-history "lazy-lang-learn" "Prompt for a learn item from history.  Stick it as the first entry." t) (defvar lazy-lang-learn-mode nil "Non-nil if Lazy-Lang-Learn mode is enabled.
See the `lazy-lang-learn-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lazy-lang-learn-mode'.") (custom-autoload 'lazy-lang-learn-mode "lazy-lang-learn" nil) (autoload 'lazy-lang-learn-mode "lazy-lang-learn" "A global minor-mode to periodically fetch something to learn using `lazy-lang-learn-new'.

This is a global minor mode.  If called interactively, toggle the
`Lazy-Lang-Learn mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='lazy-lang-learn-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "lazy-lang-learn" '("lazy-lang-learn--")) (provide 'lazy-lang-learn-autoloads)) "list-utils" ((list-utils-autoloads list-utils) (let ((loads (get 'list-utils 'custom-loads))) (if (member '"list-utils" loads) nil (put 'list-utils 'custom-loads (cons '"list-utils" loads)) (put 'extensions 'custom-loads (cons 'list-utils (get 'extensions 'custom-loads))))) (require 'cl-macs) (cl-defstruct tconc head tail) (autoload 'tconc-list "list-utils" "Efficiently append LIST to TC.

TC is a data structure created by `make-tconc'.

(fn TC LIST)") (autoload 'tconc "list-utils" "Efficiently append ARGS to TC.

TC is a data structure created by `make-tconc'

Without ARGS, return the list held by TC.

(fn TC &rest ARGS)") (autoload 'list-utils-cons-cell-p "list-utils" "Return non-nil if CELL holds a cons cell rather than a proper list.

A proper list is defined as a series of cons cells in which the
cdr slot of each cons holds a pointer to the next element of the
list, and the cdr slot in the final cons holds nil.

A plain cons cell, for the purpose of this function, is a single
cons in which the cdr holds data rather than a pointer to the
next cons cell, eg

    '(1 . 2)

In addition, a list which is not nil-terminated is not a proper
list and will be recognized by this function as a cons cell.
Such a list is printed using dot notation for the last two
elements, eg

    '(1 2 3 4 . 5)

Such improper lists are produced by `cl-list*'.

(fn CELL)") (autoload 'list-utils-make-proper-copy "list-utils" "Copy a cons cell or improper LIST into a proper list.

If optional TREE is non-nil, traverse LIST, making proper
copies of any improper lists contained within.

Optional RECUR-INTERNAL is for internal use only.

Improper lists consist of proper lists consed to a final
element, and are produced by `cl-list*'.

(fn LIST &optional TREE RECUR-INTERNAL)") (autoload 'list-utils-make-proper-inplace "list-utils" "Make a cons cell or improper LIST into a proper list.

Improper lists consist of proper lists consed to a final
element, and are produced by `cl-list*'.

If optional TREE is non-nil, traverse LIST, making any
improper lists contained within into proper lists.

Optional RECUR-INTERNAL is for internal use only.

Modifies LIST and returns the modified value.

(fn LIST &optional TREE RECUR-INTERNAL)") (autoload 'list-utils-make-improper-copy "list-utils" "Copy a proper LIST into an improper list.

Improper lists consist of proper lists consed to a final
element, and are produced by `cl-list*'.

If optional TREE is non-nil, traverse LIST, making proper
copies of any improper lists contained within.

Optional RECUR-INTERNAL is for internal use only.

(fn LIST &optional TREE RECUR-INTERNAL)") (autoload 'list-utils-make-improper-inplace "list-utils" "Make proper LIST into an improper list.

Improper lists consist of proper lists consed to a final
element, and are produced by `cl-list*'.

If optional TREE is non-nil, traverse LIST, making any
proper lists contained within into improper lists.

Optional RECUR-INTERNAL is for internal use only.

Modifies LIST and returns the modified value.

(fn LIST &optional TREE RECUR-INTERNAL)") (autoload 'list-utils-linear-subseq "list-utils" "Return the linear elements from a partially cyclic LIST.

If there is no cycle in LIST, return LIST.  If all elements of
LIST are included in a cycle, return nil.

As an optimization, CYCLE-LENGTH may be specified if the length
of the cyclic portion is already known.  Otherwise it will be
calculated from LIST.

(fn LIST &optional CYCLE-LENGTH)") (autoload 'list-utils-cyclic-subseq "list-utils" "Return any cyclic elements from LIST as a circular list.

The first element of the cyclic structure is not guaranteed to be
first element of the return value unless FROM-START is non-nil.

To linearize the return value, use `list-utils-make-linear-inplace'.

If there is no cycle in LIST, return nil.

(fn LIST &optional FROM-START)") (autoload 'list-utils-cyclic-length "list-utils" "Return the number of cyclic elements in LIST.

If some portion of LIST is linear, only the cyclic
elements will be counted.

If LIST is completely linear, return 0.

(fn LIST)") (autoload 'list-utils-cyclic-p "list-utils" "Return non-nil if LIST contains any cyclic structures.

If optional PERFECT is set, only return non-nil if LIST is a
perfect non-branching cycle in which the last element points
to the first.

(fn LIST &optional PERFECT)") (autoload 'list-utils-linear-p "list-utils" "Return non-nil if LIST is linear (no cyclic structure).

(fn LIST)") (defalias 'list-utils-improper-p 'list-utils-cons-cell-p) (autoload 'list-utils-safe-length "list-utils" "Return the number of elements in LIST.

LIST may be linear or cyclic.

If LIST is not really a list, returns 0.

If LIST is an improper list, return the number of proper list
elements, like `safe-length'.

(fn LIST)") (autoload 'list-utils-flat-length "list-utils" "Count simple elements from the beginning of LIST.

Stop counting when a cons is reached.  nil is not a cons,
and is considered to be a \"simple\" element.

If the car of LIST is a cons, return 0.

(fn LIST)") (autoload 'list-utils-make-linear-copy "list-utils" "Return a linearized copy of LIST, which may be cyclic.

If optional TREE is non-nil, traverse LIST, substituting
linearized copies of any cyclic lists contained within.

(fn LIST &optional TREE)") (autoload 'list-utils-make-linear-inplace "list-utils" "Linearize LIST, which may be cyclic.

Modifies LIST and returns the modified value.

If optional TREE is non-nil, traverse LIST, linearizing any
cyclic lists contained within.

(fn LIST &optional TREE)") (autoload 'list-utils-safe-equal "list-utils" "Compare LIST-1 and LIST-2, which may be cyclic lists.

LIST-1 and LIST-2 may also contain cyclic lists, which are
each traversed and compared.  This function will not infloop
when cyclic lists are encountered.

Non-nil is returned only if the leaves of LIST-1 and LIST-2 are
`equal' and the structure is identical.

Optional TEST specifies a test, defaulting to `equal'.

If LIST-1 and LIST-2 are not actually lists, they are still
compared according to TEST.

(fn LIST-1 LIST-2 &optional TEST)") (autoload 'list-utils-depth "list-utils" "Find the depth of LIST, which may contain other lists.

If LIST is not a list or is an empty list, returns a depth
of 0.

If LIST is a cons cell or a list which does not contain other
lists, returns a depth of 1.

(fn LIST)") (autoload 'list-utils-flatten "list-utils" "Return a flattened copy of LIST, which may contain other lists.

This function flattens cons cells as lists, and
flattens circular list structures.

(fn LIST)") (autoload 'list-utils-insert-before "list-utils" "Look in LIST for ELEMENT and insert NEW-ELEMENT before it.

Optional TEST sets the test used for a matching element, and
defaults to `equal'.

LIST is modified and the new value is returned.

(fn LIST ELEMENT NEW-ELEMENT &optional TEST)") (autoload 'list-utils-insert-after "list-utils" "Look in LIST for ELEMENT and insert NEW-ELEMENT after it.

Optional TEST sets the test used for a matching element, and
defaults to `equal'.

LIST is modified and the new value is returned.

(fn LIST ELEMENT NEW-ELEMENT &optional TEST)") (autoload 'list-utils-insert-before-pos "list-utils" "Look in LIST for position POS, and insert NEW-ELEMENT before.

POS is zero-indexed.

LIST is modified and the new value is returned.

(fn LIST POS NEW-ELEMENT)") (autoload 'list-utils-insert-after-pos "list-utils" "Look in LIST for position POS, and insert NEW-ELEMENT after.

LIST is modified and the new value is returned.

(fn LIST POS NEW-ELEMENT)") (autoload 'list-utils-and "list-utils" "Return the elements of LIST1 which are present in LIST2.

This is similar to `cl-intersection' (or `intersection') from
the cl library, except that `list-utils-and' preserves order,
does not uniquify the results, and exhibits more predictable
performance for large lists.

Order will follow LIST1.  Duplicates may be present in the result
as in LIST1.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
the list to be hashed (LIST2 unless FLIP is set).

When optional FLIP is set, the sense of the comparison is
reversed.  When FLIP is set, LIST2 will be the guide for the
order of the result, and will determine whether duplicates may
be returned.  Since this function preserves duplicates, setting
FLIP can change the number of elements in the result.

Performance: `list-utils-and' and friends use a general-purpose
hashing approach.  `intersection' and friends use pure iteration.
Iteration can be much faster in a few special cases, especially
when the number of elements is small.  In other scenarios,
iteration can be much slower.  Hashing has no worst-case
performance scenario, although it uses much more memory.  For
heavy-duty list operations, performance may be improved by
`let'ing `gc-cons-threshold' to a high value around sections that
make frequent use of this function.

(fn LIST1 LIST2 &optional TEST HINT FLIP)") (autoload 'list-utils-not "list-utils" "Return the elements of LIST1 which are not present in LIST2.

This is similar to `cl-set-difference' (or `set-difference') from
the cl library, except that `list-utils-not' preserves order and
exhibits more predictable performance for large lists.  Order will
follow LIST1.  Duplicates may be present as in LIST1.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
the list to be hashed (LIST2 unless FLIP is set).

When optional FLIP is set, the sense of the comparison is
reversed, returning elements of LIST2 which are not present in
LIST1.  When FLIP is set, LIST2 will be the guide for the order
of the result, and will determine whether duplicates may be
returned.

Performance: see notes under `list-utils-and'.

(fn LIST1 LIST2 &optional TEST HINT FLIP)") (autoload 'list-utils-xor "list-utils" "Return elements which are only present in either LIST1 or LIST2.

This is similar to `cl-set-exclusive-or' (or `set-exclusive-or')
from the cl library, except that `list-utils-xor' preserves order,
and exhibits more predictable performance for large lists.  Order
will follow LIST1, then LIST2.  Duplicates may be present as in
LIST1 or LIST2.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
the list to be hashed (LIST2 unless FLIP is set).

When optional FLIP is set, the sense of the comparison is
reversed, causing order and duplicates to follow LIST2, then
LIST1.

Performance: see notes under `list-utils-and'.

(fn LIST1 LIST2 &optional TEST HINT FLIP)") (autoload 'list-utils-uniq "list-utils" "Return a uniquified copy of LIST, preserving order.

This is similar to `cl-remove-duplicates' (or `remove-duplicates')
from the cl library, except that `list-utils-uniq' preserves order,
and exhibits more predictable performance for large lists.  Order
will follow LIST.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

(fn LIST &optional TEST HINT)") (autoload 'list-utils-dupes "list-utils" "Return only duplicated elements from LIST, preserving order.

Duplicated elements may still exist in the result: this function
removes singlets.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

(fn LIST &optional TEST HINT)") (autoload 'list-utils-singlets "list-utils" "Return only singlet elements from LIST, preserving order.

Duplicated elements may not exist in the result.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

(fn LIST &optional TEST HINT)") (autoload 'list-utils-partition-dupes "list-utils" "Partition LIST into duplicates and singlets, preserving order.

The return value is an alist with two keys: 'dupes and 'singlets.
The two values of the alist are lists which, if combined, comprise
a complete copy of the elements of LIST.

Duplicated elements may still exist in the 'dupes partition.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

(fn LIST &optional TEST HINT)") (autoload 'list-utils-alist-or-flat-length "list-utils" "Count simple or cons-cell elements from the beginning of LIST.

Stop counting when a proper list of non-zero length is reached.

If the car of LIST is a list, return 0.

(fn LIST)") (autoload 'list-utils-alist-flatten "list-utils" "Flatten LIST, which may contain other lists.  Do not flatten cons cells.

It is not guaranteed that the result contains *only* cons cells.
The result could contain other data types present in LIST.

This function simply avoids flattening single conses or improper
lists where the last two elements would be expressed as a dotted
pair.

(fn LIST)") (autoload 'list-utils-plist-reverse "list-utils" "Return reversed copy of property-list PLIST, maintaining pair associations.

(fn PLIST)") (autoload 'list-utils-plist-del "list-utils" "Delete from PLIST the property PROP and its associated value.

When PROP is not present in PLIST, there is no effect.

The new plist is returned; use `(setq x (list-utils-plist-del x prop))'
to be sure to use the new value.

This functionality overlaps with the undocumented `cl-do-remf'.

(fn PLIST PROP)") (register-definition-prefixes "list-utils" '("list-utils-htt-")) (provide 'list-utils-autoloads)) "string-utils" ((string-utils-autoloads string-utils) (autoload 'string-utils-stringify-anything "string-utils" "Coerce any object OBJ into a string.

Contrary to usual conventions, return the empty string for nil.

Sequences are flattened down to atoms and joined with string
SEPARATOR, which defaults to a single space.  Cyclic lists
may give unpredictable results (similar to `format') unless
list-utils.el is installed.

When INTS-ARE-CHARS is non-nil, interpret positive integers in
OBJ as characters.

Optional RECORD-SEPARATOR is a string (defaulting to the value of
SEPARATOR) which delimits end-of-record for paired data types
such as hash tables.

This is not a pretty-printer for OBJ, but a way to look at
the *contents* of OBJ (so much as is possible) as if it was
an ordinary string.

(fn OBJ &optional SEPARATOR INTS-ARE-CHARS RECORD-SEPARATOR)") (autoload 'string-utils-has-darkspace-p "string-utils" "Test whether OBJ, when coerced to a string, has any non-whitespace characters.

Returns the position of the first non-whitespace character
on success.

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'.

(fn OBJ &optional WHITESPACE-TYPE)") (autoload 'string-utils-has-whitespace-p "string-utils" "Test whether OBJ, when coerced to a string, has any whitespace characters.

Returns the position of the first whitespace character on
success.

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'.

(fn OBJ &optional WHITESPACE-TYPE)") (autoload 'string-utils-trim-whitespace "string-utils" "Return STR-VAL with leading and trailing whitespace removed.

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'.

If optional MULTI-LINE is set, trim spaces at starts and
ends of all lines throughout STR-VAL.

(fn STR-VAL &optional WHITESPACE-TYPE MULTI-LINE)") (autoload 'string-utils-compress-whitespace "string-utils" "Return STR-VAL with all contiguous whitespace compressed to SEPARATOR.

The default value of SEPARATOR is a single space: \" \".

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'.

(fn STR-VAL &optional WHITESPACE-TYPE SEPARATOR)") (autoload 'string-utils-string-repeat "string-utils" "Return a new string formed by repeating STR-VAL, N times.

STR-VAL may be of any length.

(fn STR-VAL N)") (autoload 'string-utils-escape-double-quotes "string-utils" "Return STR-VAL with every double-quote escaped with backslash.

(fn STR-VAL)") (autoload 'string-utils-quotemeta "string-utils" "Return STR-VAL with all non-word characters escaped with backslash.

This is more vigorous than `shell-quote-argument'.

(fn STR-VAL)") (autoload 'string-utils-pad "string-utils" "Pad STR-VAL to WIDTH.

Optional MODE defaults to 'right, but may be 'left, 'center, or
an integer.

When MODE is 'left, padding characters are prepended.  When MODE
is 'center, padding characters are both appended and prepended so
that STR-VAL is centered within WIDTH.

When MODE is a positive integer, the behavior is fixed-position
padding.  Similar to 'center, padding may be added on the right
and on the left.  Exactly MODE-many padding characters are
added on the left before padding to the full WIDTH on the right.
When MODE is a negative integer, the behavior is the same, except
that MODE fixes the right-side padding.

Optional CHAR sets the padding character (defaults to space).

Optional THROW-ERROR throws an error if the length of STR-VAL
already exceeds WIDTH, or if the fixed-position padding requested
would cause the result to exceed WIDTH.  When THROW-ERROR is not
set (the default), a best-attempt result is always returned.

Tabs are expanded to spaces according to the value of
`tab-width'.

Returns a padded copy of string STR-VAL.

(fn STR-VAL WIDTH &optional MODE CHAR THROW-ERROR)") (autoload 'string-utils-pad-list "string-utils" "Pad each member of STR-LIST to match the longest width.

ADDITIONAL-WIDTH sets a relative amount to pad beyond the longest
length.

TARGET-WIDTH sets an absolute target width, causing maximum
string length and ADDITIONAL-WIDTH to be ignored.

Optional MODE, CHAR, and THROW-ERROR are as for `string-utils-pad'.
Fixed-position MODE will attempt to pad all entries consistently,
based on any adjustments made to the longest member of STR-LIST.

Tabs are expanded to spaces according to the value of
`tab-width'.

Returns padded STR-LIST.

(fn STR-LIST &optional ADDITIONAL-WIDTH TARGET-WIDTH MODE CHAR THROW-ERROR)") (autoload 'string-utils-propertize-fillin "string-utils" "Return a copy of STR-VAL with text properties added, without overriding.

Works exactly like `propertize', except that (character-by-character)
already existing properties are respected.

STR-VAL and PROPERTIES are treated as documented for the STRING
and PROPERTIES arguments to `propertize'.

(fn STR-VAL &rest PROPERTIES)") (autoload 'string-utils-plural-ending "string-utils" "Return \"s\" or \"\", depending on whether NUM requires a plural in English.

Intended to be used in a format string as follows:

    (message \"%s item%s deleted\" del-counter (string-utils-plural-ending del-counter))

(fn NUM)") (autoload 'string-utils-squeeze-filename "string-utils" "Intelligibly squeeze file-name or buffer-name NAME to fit within MAXLEN.

When shortening file or buffer names for presentation to human
readers, it is often preferable not to truncate the ends, but to
remove leading or middle portions of the string.

This function keeps basename intact, and (failing that) the
beginning and end of the basename, so that a shortened file or
buffer name is more identifiable to a human reader.

The heuristic

   1.  Works equally for file names or buffer names.

   2.  Applies abbreviations to file names such as \"~\" for home
       directory.

   3.  Selectively removes the longest leading directory
       components from a path, preferring to keep the rightmost
       components, leaving a single ellipsis where any number of
       path elements were removed.

   4.  Shortens the basename of NAME if needed, preserving the
       meaningful file extension.

The string returned is as long as MAXLEN or shorter.

When PATH-REMOVAL is non nil, it is permitted to shorten a
pathname by removing the directory components completely,
substituting no ellipsis.

ELLIPSIS is a string inserted wherever characters were removed.
It defaults to the UCS character \"Horizontal Ellipsis\", or
\"...\" if extended characters are not displayable.

If NO-TAIL is set, do not preserve the trailing letters of
a filename unless there is a dotted extension.

(fn NAME MAXLEN &optional PATH-REMOVAL ELLIPSIS NO-TAIL)") (autoload 'string-utils-squeeze-url "string-utils" "Intelligibly squeeze string URL to fit within MAXLEN.

Fit URL within MAXLEN for presentation to a human reader.
Follows rules similar to `string-utils-squeeze-filename'.

ELLIPSIS is a string inserted wherever characters were removed.
It defaults to the UCS character \"Horizontal Ellipsis\", or
\"...\" if extended characters are not displayable.

(fn URL MAXLEN &optional ELLIPSIS)") (autoload 'string-utils-split "string-utils" "Like `split-string', with additional options.

STRING, SEPARATORS, and OMIT-NULLS are as documented at `split-string'.

INCLUDE-SEPARATORS is currently unimplemented.

When RESPECT-ESCAPES is set, STRING is not split where the
separator is escaped with backslash.  This currently has the
limitation that SEPARATORS must be an explicit string rather than
a regular expression.

(fn STRING &optional SEPARATORS OMIT-NULLS INCLUDE-SEPARATORS RESPECT-ESCAPES)") (autoload 'string-utils-truncate-to "string-utils" "Truncate STRING to MAXLEN.

The returned value is of length MAXLEN or less, including
ELLIPSIS.

ELLIPSIS is a string inserted wherever characters were removed.
It defaults to the UCS character \"Horizontal Ellipsis\", or
\"...\" if extended characters are not displayable.

(fn STR-VAL MAXLEN &optional ELLIPSIS)") (register-definition-prefixes "string-utils" '("string-utils-")) (provide 'string-utils-autoloads)) "browse-url-dwim" ((browse-url-dwim browse-url-dwim-autoloads) (let ((loads (get 'browse-url-dwim 'custom-loads))) (if (member '"browse-url-dwim" loads) nil (put 'browse-url-dwim 'custom-loads (cons '"browse-url-dwim" loads)) (put 'external 'custom-loads (cons 'browse-url-dwim (get 'external 'custom-loads))))) (let ((loads (get 'browse-url-dwim-keys 'custom-loads))) (if (member '"browse-url-dwim" loads) nil (put 'browse-url-dwim-keys 'custom-loads (cons '"browse-url-dwim" loads)) (put 'browse-url-dwim 'custom-loads (cons 'browse-url-dwim-keys (get 'browse-url-dwim 'custom-loads))))) (defvar browse-url-dwim-mode nil "Non-nil if Browse-Url-Dwim mode is enabled.
See the `browse-url-dwim-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `browse-url-dwim-mode'.") (custom-autoload 'browse-url-dwim-mode "browse-url-dwim" nil) (autoload 'browse-url-dwim-mode "browse-url-dwim" "Turn on `browse-url-dwim-mode'.

Turning on `browse-url-dwim' will activate keybindings as defined
in `customize'.  It may also install a command alias for `browse'
and `google' as controlled by `browse-url-dwim-install-aliases'.

When called interactively with no prefix argument this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle.

This is a global minor mode.  If called interactively, toggle the
`Browse-Url-Dwim mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='browse-url-dwim-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'browse-url-dwim "browse-url-dwim" "Opens a URL in an external browser.

When called interactively, `browse-url-dwim-get-url' will be
used to find an appropriate URL.

The browser used is as configured for `browse-url'.

(fn URL)" t) (autoload 'browse-url-dwim-search "browse-url-dwim" "Perform an Internet search for TEXT, or region, or interactive input.

If TEXT is a URL, browse to page directly.  Otherwise
invoke an Internet search using TEXT.  When called interactively,
TEXT may be taken from the region or entered at a prompt.

Optional SEARCH-URL specifies the URL fragment used to construct
the search request.  If not specified, the customizable variable
`browse-url-dwim-search-url' is used.

If GUESS is non-nil, an attempt will be made to extract a URL
from the context around the point.  If successful, this command
is equivalent to `browse-url-dwim'.

(fn &optional TEXT SEARCH-URL GUESS)" t) (autoload 'browse-url-dwim-guess "browse-url-dwim" "Perform Internet search or browse to URL under point, according to context.

Identical to calling `browse-url-dwim-search' with GUESS set
to non-nil.

Optional TEXT is a string to be submitted to the search
engine.

Optional SEARCH-URL overrides the default search engine
URL.

(fn &optional TEXT SEARCH-URL)" t) (register-definition-prefixes "browse-url-dwim" '("browse-url-")) (provide 'browse-url-dwim-autoloads)) "posframe" ((posframe-autoloads posframe-benchmark posframe) (autoload 'posframe-workable-p "posframe" "Test posframe workable status.") (autoload 'posframe-show "posframe" "Pop up a posframe to show STRING at POSITION.

 (1) POSITION

POSITION can be:
1. An integer, meaning point position.
2. A cons of two integers, meaning absolute X and Y coordinates.
3. Other type, in which case the corresponding POSHANDLER should be
   provided.

 (2) POSHANDLER

POSHANDLER is a function of one argument returning an actual
position.  Its argument is a plist of the following form:

  (:position xxx
   :poshandler xxx
   :font-height xxx
   :font-width xxx
   :posframe xxx
   :posframe-width xxx
   :posframe-height xxx
   :posframe-buffer xxx
   :parent-frame xxx
   :parent-window-start xxx
   :parent-window-end xxx
   :parent-window-left xxx
   :parent-window-top xxx
   :parent-frame-width xxx
   :parent-frame-height xxx
   :parent-window xxx
   :parent-window-width  xxx
   :parent-window-height xxx
   :mouse-x xxx
   ;mouse-y xxx
   :minibuffer-height xxx
   :mode-line-height  xxx
   :header-line-height xxx
   :tab-line-height xxx
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

By default, poshandler is auto-selected based on the type of POSITION,
but the selection can be overridden using the POSHANDLER argument.

The builtin poshandler functions are listed below:

1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-top-left-or-right-other-corner'
6.  `posframe-poshandler-frame-bottom-center'
7.  `posframe-poshandler-frame-bottom-left-corner'
8.  `posframe-poshandler-frame-bottom-right-corner'
9.  `posframe-poshandler-window-center'
10.  `posframe-poshandler-window-top-center'
11. `posframe-poshandler-window-top-left-corner'
12. `posframe-poshandler-window-top-right-corner'
13. `posframe-poshandler-window-bottom-center'
14. `posframe-poshandler-window-bottom-left-corner'
15. `posframe-poshandler-window-bottom-right-corner'
16. `posframe-poshandler-point-top-left-corner'
17. `posframe-poshandler-point-bottom-left-corner'
18. `posframe-poshandler-point-bottom-left-corner-upward'
19. `posframe-poshandler-point-window-center'
20. `posframe-poshandler-point-frame-center'

 (3) POSHANDLER-EXTRA-INFO

POSHANDLER-EXTRA-INFO is a plist, which will prepend to the
argument of poshandler function: `info', it will *OVERRIDE* the
exist key in `info'.

 (4) BUFFER-OR-NAME

This posframe's buffer is BUFFER-OR-NAME, which can be a buffer
or a name of a (possibly nonexistent) buffer.

buffer name can prefix with space, for example \" *mybuffer*\", so
the buffer name will hide for ibuffer and `list-buffers'.

 (5) NO-PROPERTIES

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before being shown in posframe.

 (6) HEIGHT, MAX-HEIGHT, MIN-HEIGHT, WIDTH, MAX-WIDTH and MIN-WIDTH

These arguments are specified in the canonical character width
and height of posframe, more details can be found in docstring of
function `fit-frame-to-buffer',

 (7) LEFT-FRINGE and RIGHT-FRINGE

If LEFT-FRINGE or RIGHT-FRINGE is a number, left fringe or
right fringe with be shown with the specified width.

 (8) BORDER-WIDTH, BORDER-COLOR, INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR

By default, posframe shows no borders, but users can specify
borders by setting BORDER-WIDTH to a positive number.  Border
color can be specified by BORDER-COLOR.

INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR are same as
BORDER-WIDTH and BORDER-COLOR, but do not suggest to use for the
reason:

   Add distinct controls for child frames' borders (Bug#45620)
   http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ff7b1a133bfa7f2614650f8551824ffaef13fadc

 (9) FONT, FOREGROUND-COLOR and BACKGROUND-COLOR

Posframe's font as well as foreground and background colors are
derived from the current frame by default, but can be overridden
using the FONT, FOREGROUND-COLOR and BACKGROUND-COLOR arguments,
respectively.

 (10) CURSOR and WINDOW-POINT

By default, cursor is not showed in posframe, user can let cursor
showed with this argument help by set its value to a `cursor-type'.

When cursor need to be showed in posframe, user may need to set
WINDOW-POINT to the point of BUFFER, which can let cursor showed
at this point.

 (11) RESPECT-HEADER-LINE and RESPECT-MODE-LINE

By default, posframe will display no header-line, mode-line and
tab-line.  In case a header-line, mode-line or tab-line is
desired, users can set RESPECT-HEADER-LINE and RESPECT-MODE-LINE
to t.

 (12) INITIALIZE

INITIALIZE is a function with no argument.  It will run when
posframe buffer is first selected with `with-current-buffer'
in `posframe-show', and only run once (for performance reasons).

 (13) LINES-TRUNCATE

If LINES-TRUNCATE is non-nil, then lines will truncate in the
posframe instead of wrap.

 (14) OVERRIDE-PARAMETERS

OVERRIDE-PARAMETERS is very powful, *all* the valid frame parameters
used by posframe's frame can be overridden by it.

NOTE: some `posframe-show' arguments are not frame parameters, so they
can not be overrided by this argument.

 (15) TIMEOUT

TIMEOUT can specify the number of seconds after which the posframe
will auto-hide.

 (15) REFRESH

If REFRESH is a number, posframe's frame-size will be re-adjusted
every REFRESH seconds.

 (17) ACCEPT-FOCUS

When ACCEPT-FOCUS is non-nil, posframe will accept focus.
be careful, you may face some bugs when set it to non-nil.

 (18) HIDEHANDLER

HIDEHANDLER is a function, when it return t, posframe will be
hide, this function has a plist argument:

  (:posframe-buffer xxx
   :posframe-parent-buffer xxx)

The builtin hidehandler functions are listed below:

1. `posframe-hidehandler-when-buffer-switch'

 (19) REFPOSHANDLER

REFPOSHANDLER is a function, a reference position (most is
top-left of current frame) will be returned when call this
function.

when it is nil or it return nil, child-frame feature will be used
and reference position will be deal with in Emacs.

The user case I know at the moment is let ivy-posframe work well
in EXWM environment (let posframe show on the other application
window).

         DO NOT USE UNLESS NECESSARY!!!

An example parent frame poshandler function is:

1. `posframe-refposhandler-xwininfo'

 (19) Others

You can use `posframe-delete-all' to delete all posframes.

(fn BUFFER-OR-NAME &key STRING POSITION POSHANDLER POSHANDLER-EXTRA-INFO WIDTH HEIGHT MAX-WIDTH MAX-HEIGHT MIN-WIDTH MIN-HEIGHT X-PIXEL-OFFSET Y-PIXEL-OFFSET LEFT-FRINGE RIGHT-FRINGE BORDER-WIDTH BORDER-COLOR INTERNAL-BORDER-WIDTH INTERNAL-BORDER-COLOR FONT CURSOR WINDOW-POINT FOREGROUND-COLOR BACKGROUND-COLOR RESPECT-HEADER-LINE RESPECT-MODE-LINE INITIALIZE NO-PROPERTIES KEEP-RATIO LINES-TRUNCATE OVERRIDE-PARAMETERS TIMEOUT REFRESH ACCEPT-FOCUS HIDEHANDLER REFPOSHANDLER &allow-other-keys)") (autoload 'posframe-hide-all "posframe" "Hide all posframe frames." t) (autoload 'posframe-delete-all "posframe" "Delete all posframe frames and buffers." t) (register-definition-prefixes "posframe" '("posframe-")) (autoload 'posframe-benchmark "posframe-benchmark" "Benchmark tool for posframe." t) (register-definition-prefixes "posframe-benchmark" '("posframe-benchmark-alist")) (provide 'posframe-autoloads)) "avy" ((avy avy-autoloads) (autoload 'avy-process "avy" "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)") (autoload 'avy-goto-char "avy" "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-char-in-line "avy" "Jump to the currently visible CHAR in the current line.

(fn CHAR)" t) (autoload 'avy-goto-char-2 "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn CHAR1 CHAR2 &optional ARG BEG END)" t) (autoload 'avy-goto-char-2-above "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t) (autoload 'avy-goto-char-2-below "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t) (autoload 'avy-isearch "avy" "Jump to one of the current isearch candidates." t) (autoload 'avy-goto-word-0 "avy" "Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t) (autoload 'avy-goto-whitespace-end "avy" "Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t) (autoload 'avy-goto-word-1 "avy" "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

(fn CHAR &optional ARG BEG END SYMBOL)" t) (autoload 'avy-goto-word-1-above "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-word-1-below "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1 "avy" "Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1-above "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1-below "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-subword-0 "avy" "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

(fn &optional ARG PREDICATE BEG END)" t) (autoload 'avy-goto-subword-1 "avy" "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-word-or-subword-1 "avy" "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t) (autoload 'avy-goto-line "avy" "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

(fn &optional ARG)" t) (autoload 'avy-goto-line-above "avy" "Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t) (autoload 'avy-goto-line-below "avy" "Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t) (autoload 'avy-goto-end-of-line "avy" "Call `avy-goto-line' and move to the end of the line.

(fn &optional ARG)" t) (autoload 'avy-copy-line "avy" "Copy a selected line above the current line.
ARG lines can be used.

(fn ARG)" t) (autoload 'avy-move-line "avy" "Move a selected line above the current line.
ARG lines can be used.

(fn ARG)" t) (autoload 'avy-copy-region "avy" "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t) (autoload 'avy-move-region "avy" "Select two lines and move the text between them above the current line." t) (autoload 'avy-kill-region "avy" "Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t) (autoload 'avy-kill-ring-save-region "avy" "Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn ARG)" t) (autoload 'avy-kill-whole-line "avy" "Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

(fn ARG)" t) (autoload 'avy-kill-ring-save-whole-line "avy" "Select line and save the whole selected line as if killed, but don’t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

(fn ARG)" t) (autoload 'avy-setup-default "avy" "Setup the default shortcuts.") (autoload 'avy-goto-char-timer "avy" "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn &optional ARG)" t) (autoload 'avy-transpose-lines-in-region "avy" "Transpose lines in the active region." t) (register-definition-prefixes "avy" '("avy-")) (provide 'avy-autoloads)) "ace-window" ((ace-window-autoloads ace-window ace-window-posframe) (autoload 'ace-select-window "ace-window" "Ace select window." t) (autoload 'ace-delete-window "ace-window" "Ace delete window." t) (autoload 'ace-swap-window "ace-window" "Ace swap window." t) (autoload 'ace-delete-other-windows "ace-window" "Ace delete other windows." t) (autoload 'ace-display-buffer "ace-window" "Make `display-buffer' and `pop-to-buffer' select using `ace-window'.
See sample config for `display-buffer-base-action' and `display-buffer-alist':
https://github.com/abo-abo/ace-window/wiki/display-buffer.

(fn BUFFER ALIST)") (autoload 'ace-window "ace-window" "Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.
See `aw-scope' which extends it to work with frames.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window.

(fn ARG)" t) (defvar ace-window-display-mode nil "Non-nil if Ace-Window-Display mode is enabled.
See the `ace-window-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-display-mode'.") (custom-autoload 'ace-window-display-mode "ace-window" nil) (autoload 'ace-window-display-mode "ace-window" "Minor mode for showing the ace window key in the mode line.

This is a global minor mode.  If called interactively, toggle the
`Ace-Window-Display mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='ace-window-display-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "ace-window" '("ace-window-mode" "aw-")) (defvar ace-window-posframe-mode nil "Non-nil if Ace-Window-Posframe mode is enabled.
See the `ace-window-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-posframe-mode'.") (custom-autoload 'ace-window-posframe-mode "ace-window-posframe" nil) (autoload 'ace-window-posframe-mode "ace-window-posframe" "Minor mode for showing the ace window key with child frames.

This is a global minor mode.  If called interactively, toggle the
`Ace-Window-Posframe mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='ace-window-posframe-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "ace-window-posframe" '("ace-window-posframe-" "aw-")) (provide 'ace-window-autoloads)) "ace-link" ((ace-link-autoloads ace-link) (autoload 'ace-link "ace-link" "Call the ace link function for the current `major-mode'" t) (autoload 'ace-link-info "ace-link" "Open a visible link in an `Info-mode' buffer." t) (autoload 'ace-link-help "ace-link" "Open a visible link in a `help-mode' buffer." t) (autoload 'ace-link-man "ace-link" "Open a visible link in a `man' buffer." t) (autoload 'ace-link-woman "ace-link" "Open a visible link in a `woman-mode' buffer." t) (autoload 'ace-link-eww "ace-link" "Open a visible link in an `eww-mode' buffer.
If EXTERNAL is single prefix, browse the URL using
`browse-url-secondary-browser-function'.

If EXTERNAL is double prefix, browse in new buffer.

(fn &optional EXTERNAL)" t) (autoload 'ace-link-w3m "ace-link" "Open a visible link in an `w3m-mode' buffer." t) (autoload 'ace-link-compilation "ace-link" "Open a visible link in a `compilation-mode' buffer." t) (autoload 'ace-link-gnus "ace-link" "Open a visible link in a `gnus-article-mode' buffer." t) (autoload 'ace-link-mu4e "ace-link" "Open a visible link in an `mu4e-view-mode' buffer." t) (autoload 'ace-link-notmuch-plain "ace-link" "Open a visible link in a `notmuch-show' buffer.
Only consider the 'text/plain' portion of the buffer." t) (autoload 'ace-link-notmuch-html "ace-link" "Open a visible link in a `notmuch-show' buffer.
Only consider the 'text/html' portion of the buffer." t) (autoload 'ace-link-notmuch "ace-link" "Open a visible link in `notmuch-show' buffer.
Consider both the links in 'text/plain' and 'text/html'." t) (autoload 'ace-link-org "ace-link" "Open a visible link in an `org-mode' buffer." t) (autoload 'ace-link-org-agenda "ace-link" "Open a visible link in an `org-mode-agenda' buffer." t) (autoload 'ace-link-xref "ace-link" "Open a visible link in an `xref--xref-buffer-mode' buffer." t) (autoload 'ace-link-custom "ace-link" "Open a visible link in an `Custom-mode' buffer." t) (autoload 'ace-link-addr "ace-link" "Open a visible link in a goto-address buffer." t) (autoload 'ace-link-sldb "ace-link" "Interact with a frame or local variable in a sldb buffer." t) (autoload 'ace-link-slime-xref "ace-link" "Open a visible link in an `slime-xref-mode' buffer." t) (autoload 'ace-link-slime-inspector "ace-link" "Interact with a value, an action or a range button in a
`slime-inspector-mode' buffer." t) (autoload 'ace-link-indium-inspector "ace-link" "Interact with a value, an action or a range button in a
`indium-inspector-mode' buffer." t) (autoload 'ace-link-indium-debugger-frames "ace-link" "Interact with a value, an action or a range button in a
`indium-debugger-frames-mode' buffer." t) (autoload 'ace-link-cider-inspector "ace-link" "Open a visible link in a `cider-inspector-mode' buffer." t) (autoload 'ace-link-setup-default "ace-link" "Bind KEY to appropriate functions in appropriate keymaps.

(fn &optional KEY)") (register-definition-prefixes "ace-link" '("ace-link-")) (provide 'ace-link-autoloads)) "ace-jump-mode" ((ace-jump-mode ace-jump-mode-autoloads) (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Pop up a postion from `ace-jump-mode-mark-ring', and jump back to that position" t) (autoload 'ace-jump-char-mode "ace-jump-mode" "AceJump char mode

(fn QUERY-CHAR)" t) (autoload 'ace-jump-word-mode "ace-jump-mode" "AceJump word mode.
You can set `ace-jump-word-mode-use-query-char' to nil to prevent
asking for a head char, that will mark all the word in current
buffer.

(fn HEAD-CHAR)" t) (autoload 'ace-jump-line-mode "ace-jump-mode" "AceJump line mode.
Marked each no empty line and move there" t) (autoload 'ace-jump-mode "ace-jump-mode" "AceJump mode is a minor mode for you to quick jump to a
position in the curret view.
   There is three submode now:
     `ace-jump-char-mode'
     `ace-jump-word-mode'
     `ace-jump-line-mode'

You can specify the sequence about which mode should enter
by customize `ace-jump-mode-submode-list'.

If you do not want to query char for word mode, you can change
`ace-jump-word-mode-use-query-char' to nil.

If you don't like the default move keys, you can change it by
setting `ace-jump-mode-move-keys'.

You can constrol whether use the case sensitive via
`ace-jump-mode-case-fold'.

(fn &optional PREFIX)" t) (register-definition-prefixes "ace-jump-mode" '("ace-jump-" "aj-")) (provide 'ace-jump-mode-autoloads)) "golden-ratio" ((golden-ratio golden-ratio-autoloads) (autoload 'golden-ratio "golden-ratio" "Resizes current window to the golden-ratio's size specs.

(fn &optional ARG)" t) (defvar golden-ratio-mode nil "Non-nil if Golden-Ratio mode is enabled.
See the `golden-ratio-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `golden-ratio-mode'.") (custom-autoload 'golden-ratio-mode "golden-ratio" nil) (autoload 'golden-ratio-mode "golden-ratio" "Enable automatic window resizing with golden ratio.

This is a global minor mode.  If called interactively, toggle the
`Golden-Ratio mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='golden-ratio-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "golden-ratio" '("golden-ratio-")) (provide 'golden-ratio-autoloads)) "pulsar" ((pulsar pulsar-pkg pulsar-autoloads) (autoload 'pulsar-pulse-line "pulsar" "Temporarily highlight the current line.
When `pulsar-pulse' is non-nil (the default) make the highlight
pulse before fading away.  The pulse effect is controlled by
`pulsar-delay' and `pulsar-iterations'.

Also see `pulsar-highlight-line' for a highlight without the
pulse effect." t) (autoload 'pulsar-pulse-region "pulsar" "Temporarily highlight the active region if any." t) (autoload 'pulsar-highlight-line "pulsar" "Temporarily highlight the current line.
Unlike `pulsar-pulse-line', never pulse the current line.  Keep
the highlight in place until another command is invoked.

Use `pulsar-highlight-face' (it is the same as `pulsar-face' by
default)." t) (autoload 'pulsar-define-pulse-with-face "pulsar" "Produce function to `pulsar--pulse' with FACE.
If FACE starts with the `pulsar-' prefix, remove it and keep only
the remaining text.  The assumption is that something like
`pulsar-red' will be convered to `red', thus deriving a function
named `pulsar-pulse-line-red'.  Any other FACE is taken as-is.

(fn FACE)" nil t) (function-put 'pulsar-define-pulse-with-face 'lisp-indent-function 'function) (autoload 'pulsar-highlight-dwim "pulsar" "Temporarily highlight the current line or active region.
The region may also be a rectangle.

For lines, do the same as `pulsar-highlight-line'." t) (put 'pulsar-global-mode 'globalized-minor-mode t) (defvar pulsar-global-mode nil "Non-nil if Pulsar-Global mode is enabled.
See the `pulsar-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pulsar-global-mode'.") (custom-autoload 'pulsar-global-mode "pulsar" nil) (autoload 'pulsar-global-mode "pulsar" "Toggle Pulsar mode in all buffers.
With prefix ARG, enable Pulsar-Global mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Pulsar mode is enabled in all buffers where `pulsar--on' would do it.

See `pulsar-mode' for more information on Pulsar mode.

(fn &optional ARG)" t) (register-definition-prefixes "pulsar" '("pulsar-")) (provide 'pulsar-autoloads)) "blackout" ((blackout blackout-autoloads) (autoload 'blackout "blackout" "Do not display MODE in the mode line.
If REPLACEMENT is given, then display it instead. REPLACEMENT may
be a string or more generally any mode line construct (see
`mode-line-format').

(fn MODE &optional REPLACEMENT)") (autoload 'use-package-normalize/:blackout "blackout" "Normalize the arguments to `:blackout'.
The return value is an alist whose cars are mode names and whose
cdrs are mode line constructs. For documentation on NAME,
KEYWORD, and ARGS, refer to `use-package'.

(fn NAME KEYWORD ARGS)") (autoload 'use-package-handler/:blackout "blackout" "Handle `:blackout' keyword.
For documentation on NAME, KEYWORD, ARG, REST, and STATE, refer
to `use-package'.

(fn NAME KEYWORD ARG REST STATE)") (with-eval-after-load 'use-package-core (when (and (boundp 'use-package-keywords) (listp use-package-keywords)) (add-to-list 'use-package-keywords :blackout 'append))) (register-definition-prefixes "blackout" '("blackout-")) (provide 'blackout-autoloads)) "boxquote" ((boxquote-autoloads boxquote) (autoload 'boxquote-title "boxquote" "Set the title of the current boxquote to TITLE.

If TITLE is an empty string the title is removed. Note that
the title will be formatted using `boxquote-title-format'.

(fn TITLE)" t) (autoload 'boxquote-region "boxquote" "Draw a box around the left hand side of a region bounding START and END.

(fn START END)" t) (autoload 'boxquote-buffer "boxquote" "Apply `boxquote-region' to a whole buffer." t) (autoload 'boxquote-insert-file "boxquote" "Insert the contents of a file, boxed with `boxquote-region'.

If `boxquote-title-files' is non-nil the boxquote will be given a
title that is the result of applying `boxquote-file-title-function'
to FILENAME.

(fn FILENAME)" t) (autoload 'boxquote-insert-buffer "boxquote" "Insert the contents of a buffer, boxes with `boxquote-region'.

If `boxquote-title-buffers' is non-nil the boxquote will be given a
title that is the result of applying `boxquote-buffer-title-function'
to BUFFER.

(fn BUFFER)" t) (autoload 'boxquote-kill-ring-save "boxquote" "Like `kill-ring-save' but remembers a title if possible.

The title is acquired by calling `boxquote-kill-ring-save-title'.
The title will be used by `boxquote-yank'." t) (autoload 'boxquote-yank "boxquote" "Do a `yank' and box it in with `boxquote-region'.

If the yanked entry was placed on the kill ring with
`boxquote-kill-ring-save' the resulting boxquote will be titled with
whatever `boxquote-kill-ring-save-title' returned at the time." t) (autoload 'boxquote-defun "boxquote" "Apply `boxquote-region' the current defun." t) (autoload 'boxquote-paragraph "boxquote" "Apply `boxquote-region' to the current paragraph." t) (autoload 'boxquote-boxquote "boxquote" "Apply `boxquote-region' to the current boxquote." t) (autoload 'boxquote-describe-function "boxquote" "Call `describe-function' and boxquote the output into the current buffer.

FUNCTION is the function to describe.

(fn FUNCTION)" t) (autoload 'boxquote-describe-variable "boxquote" "Call `describe-variable' and boxquote the output into the current buffer.

VARIABLE is the variable to describe.

(fn VARIABLE)" t) (autoload 'boxquote-describe-key "boxquote" "Call `describe-key' on KEY and boxquote the output into the current buffer.

If the call to this command is prefixed with \\[universal-argument] you will also be
prompted for a buffer. The key definition used will be taken from
that buffer.

(fn KEY)" t) (autoload 'boxquote-shell-command "boxquote" "Call `shell-command' with COMMAND and boxquote the output.

(fn COMMAND)" t) (autoload 'boxquote-where-is "boxquote" "Call `where-is' with DEFINITION and boxquote the result.

(fn DEFINITION)" t) (autoload 'boxquote-text "boxquote" "Insert TEXT, boxquoted.

(fn TEXT)" t) (autoload 'boxquote-narrow-to-boxquote "boxquote" "Narrow the buffer to the current boxquote." t) (autoload 'boxquote-narrow-to-boxquote-content "boxquote" "Narrow the buffer to the content of the current boxquote." t) (autoload 'boxquote-kill "boxquote" "Kill the boxquote and its contents." t) (autoload 'boxquote-fill-paragraph "boxquote" "Perform a `fill-paragraph' inside a boxquote.

(fn ARG)" t) (autoload 'boxquote-unbox-region "boxquote" "Remove a box created with `boxquote-region'.

(fn START END)" t) (autoload 'boxquote-unbox "boxquote" "Remove the boxquote that contains `point'." t) (register-definition-prefixes "boxquote" '("boxquote-")) (provide 'boxquote-autoloads)) "dpaste" ((dpaste dpaste-autoloads) (autoload 'dpaste-region "dpaste" "Post the current region or buffer to dpaste.com and yank the
url to the kill-ring.

(fn BEGIN END TITLE &optional ARG)" t) (autoload 'dpaste-buffer "dpaste" "Post the current buffer to dpaste.com and yank the url to the
kill-ring.

(fn TITLE &optional ARG)" t) (autoload 'dpaste-region-or-buffer "dpaste" "Post the current region or buffer to dpaste.com and yank the
url to the kill-ring.

(fn TITLE &optional ARG)" t) (register-definition-prefixes "dpaste" '("dpaste-")) (provide 'dpaste-autoloads)) "darkroom" ((darkroom-pkg darkroom-autoloads darkroom) (autoload 'darkroom-mode "darkroom" "Remove visual distractions and focus on writing. When this

mode is active, everything but the buffer's text is elided from
view. The buffer margins are set so that text is centered on
screen. Text size is increased (display engine allowing) by
`darkroom-text-scale-increase'.

This is a minor mode.  If called interactively, toggle the
`Darkroom mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `darkroom-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'darkroom-tentative-mode "darkroom" "Enters `darkroom-mode' when all other windows are deleted.

This is a minor mode.  If called interactively, toggle the
`DarkRoom-Tentative mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `darkroom-tentative-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "darkroom" '("darkroom-")) (provide 'darkroom-autoloads)) "bookmark+" ((bookmark+-1 bookmark+-lit bookmark+ bookmark+-mac bookmark+-key bookmark+-bmu bookmark+-chg bookmark+-doc bookmark+-autoloads) (autoload 'bmkp-version-number "bookmark+") (autoload 'bmkp-version "bookmark+") (autoload 'bookmark-bmenu-buffer "bookmark+") (autoload 'bookmark-plus "bookmark+") (autoload 'bmkp-bmenu-buffer "bookmark+") (register-definition-prefixes "bookmark+" '("bookmark-")) (autoload 'bmkp-autofile-access-invokes-bookmark-flag "bookmark+") (autoload 'bmkp-autofile-filecache "bookmark+") (autoload 'bmkp-automatic-bookmark-min-distance "bookmark+") (autoload 'bmkp-automatic-bookmark-mode "bookmark+") (autoload 'bmkp-automatic-bookmark-mode-delay "bookmark+") (autoload 'bmkp-automatic-bookmark-mode-lighter "bookmark+") (autoload 'bmkp-automatic-bookmark-set-function "bookmark+") (autoload 'bmkp-autoname-bookmark-function "bookmark+") (autoload 'bmkp-autoname-format "bookmark+") (autoload 'bmkp-autotemp-bookmark-predicates "bookmark+") (autoload 'bmkp-bookmark-name-length-max "bookmark+") (autoload 'bmkp-count-multi-mods-as-one-flag "bookmark+") (autoload 'bmkp-crosshairs-flag "bookmark+") (autoload 'bmkp-default-bookmark-name "bookmark+") (autoload 'bmkp-default-handlers-for-file-types "bookmark+") (autoload 'bmkp-desktop-default-directory "bookmark+") (autoload 'bmkp-desktop-jump-save-before-flag "bookmark+") (autoload 'bmkp-desktop-no-save-vars "bookmark+") (autoload 'bmkp-annotation-modes-inherit-from "bookmark+") (autoload 'bmkp-handle-region-function "bookmark+") (autoload 'bmkp-info-sort-ignores-directories-flag "bookmark+") (autoload 'bmkp-incremental-filter-delay "bookmark+") (autoload 'bmkp-menu-popup-max-length "bookmark+") (autoload 'bmkp-new-bookmark-default-names "bookmark+") (autoload 'bmkp-other-window-pop-to-flag "bookmark+") (autoload 'bmkp-prompt-for-tags-flag "bookmark+") (autoload 'bmkp-properties-to-keep "bookmark+") (autoload 'bmkp-region-search-size "bookmark+") (autoload 'bmkp-save-new-location-flag "bookmark+") (autoload 'bmkp-sequence-jump-display-function "bookmark+") (autoload 'bmkp-show-end-of-region-flag "bookmark+") (autoload 'bmkp-sort-comparer "bookmark+") (autoload 'bmkp-su-or-sudo-regexp "bookmark+") (autoload 'bmkp-tags-for-completion "bookmark+") (autoload 'bmkp-temporary-bookmarking-mode-hook "bookmark+") (autoload 'bmkp-this-file/buffer-cycle-sort-comparer "bookmark+") (autoload 'bmkp-guess-default-handler-for-file-flag "bookmark+") (autoload 'bmkp-read-bookmark-file-hook "bookmark+") (autoload 'bmkp-temporary-bookmarking-mode-lighter "bookmark+") (autoload 'bmkp-use-region "bookmark+") (autoload 'bmkp-w3m-allow-multiple-buffers-flag "bookmark+") (autoload 'bmkp-write-bookmark-file-hook "bookmark+") (defvar Info-bookmark-use-only-node-not-file-flag t "Non-nil means an Info bookmark uses only the node name.
The recorded Info file name is ignored.  This means use only manuals
corresponding to the current Emacs session, regardless of the Emacs
version or platform used to record the bookmark.

A nil value means use the manuals whose absolute file names are
recorded in the bookmarks.  (But if the file doesn't exist or is
unreadable, then act as if the value is non-nil.)

A non-nil value means you can use the same bookmark with different
Emacs installations, including on different platforms.  A nil value
means that you can use a bookmark to consult the Info manual for a
different Emacs version from that of the current session.") (custom-autoload 'Info-bookmark-use-only-node-not-file-flag "bookmark+-1" t) (autoload 'bookmark-default-annotation-text "bookmark+") (autoload 'bookmark-insert-annotation "bookmark+") (autoload 'bookmark-edit-annotation-mode "bookmark+") (autoload 'bookmark-send-edited-annotation "bookmark+") (autoload 'bookmark-edit-annotation "bookmark+") (autoload 'bookmark-set "bookmark+") (autoload 'bookmark-yank-word "bookmark+") (autoload 'bookmark-jump "bookmark+") (autoload 'bookmark-jump-other-window "bookmark+") (autoload 'bookmark-jump-other-frame "bookmark+") (autoload 'bookmark-relocate "bookmark+") (autoload 'bookmark-insert-current-bookmark "bookmark+") (autoload 'bookmark-insert-location "bookmark+") (autoload 'bookmark-rename "bookmark+") (autoload 'bookmark-insert "bookmark+") (autoload 'bookmark-delete "bookmark+") (autoload 'bookmark-save "bookmark+") (autoload 'bookmark-load "bookmark+") (autoload 'bookmark-show-annotation "bookmark+") (autoload 'bookmark-show-all-annotations "bookmark+") (autoload 'bmkp-annotate-bookmark "bookmark+") (autoload 'bmkp-annotate-bookmark-this-file/buffer "bookmark+") (autoload 'bmkp-annotate-all-bookmarks-this-file/buffer "bookmark+") (autoload 'bmkp-show-this-annotation-read-only "bookmark+") (autoload 'bmkp-edit-this-annotation "bookmark+") (autoload 'bmkp-copy-bookmark "bookmark+") (autoload 'bmkp-clone-bookmark "bookmark+") (autoload 'bmkp-edit-bookmark-name-and-location "bookmark+") (autoload 'bmkp-edit-bookmark-records-send "bookmark+") (autoload 'bmkp-edit-bookmark-record "bookmark+") (autoload 'bmkp-edit-bookmark-record-send "bookmark+") (autoload 'bmkp-edit-bookmark-record-file/buffer "bookmark+") (autoload 'bmkp-edit-tags "bookmark+") (autoload 'bmkp-edit-tags-send "bookmark+") (autoload 'bmkp-bookmark-set-confirm-overwrite "bookmark+") (autoload 'bmkp-send-bug-report "bookmark+") (autoload 'bmkp-toggle-bookmark-set-refreshes "bookmark+") (autoload 'bmkp-toggle-saving-menu-list-state "bookmark+") (autoload 'bmkp-save-menu-list-state "bookmark+") (autoload 'bmkp-toggle-saving-bookmark-file "bookmark+") (autoload 'bmkp-jump-to-list "bookmark+") (autoload 'bmkp-make-function-bookmark "bookmark+") (autoload 'bmkp-set-dired-bookmark-for-files "bookmark+") (autoload 'bmkp-revert-bookmark-file "bookmark+") (autoload 'bmkp-switch-bookmark-file "bookmark+") (autoload 'bmkp-switch-to-last-bookmark-file "bookmark+") (autoload 'bmkp-switch-bookmark-file-create "bookmark+") (autoload 'bmkp-switch-to-bookmark-file-this-file/buffer "bookmark+") (autoload 'bmkp-empty-file "bookmark+") (autoload 'bmkp-save-bookmarks-this-file/buffer "bookmark+") (autoload 'bmkp-crosshairs-highlight "bookmark+") (autoload 'bmkp-choose-navlist-from-bookmark-list "bookmark+") (autoload 'bmkp-choose-navlist-of-type "bookmark+") (autoload 'bmkp-this-file/buffer-bmenu-list "bookmark+") (autoload 'bmkp-this-file-bmenu-list "bookmark+") (autoload 'bmkp-this-buffer-bmenu-list "bookmark+") (autoload 'bmkp-navlist-bmenu-list "bookmark+") (autoload 'bmkp-unomit-all "bookmark+") (autoload 'bmkp-list-all-tags "bookmark+") (autoload 'bmkp-remove-all-tags "bookmark+") (autoload 'bmkp-add-tags "bookmark+") (autoload 'bmkp-set-tag-value-for-navlist "bookmark+") (autoload 'bmkp-set-tag-value "bookmark+") (autoload 'bmkp-remove-tags "bookmark+") (autoload 'bmkp-remove-tags-from-all "bookmark+") (autoload 'bmkp-rename-tag "bookmark+") (autoload 'bmkp-copy-tags "bookmark+") (autoload 'bmkp-paste-add-tags "bookmark+") (autoload 'bmkp-paste-replace-tags "bookmark+") (autoload 'bmkp-url-target-set "bookmark+") (autoload 'bmkp-file-target-set "bookmark+") (autoload 'bmkp-bookmark-a-file "bookmark+") (autoload 'bmkp-autofile-set "bookmark+") (autoload 'bmkp-tag-a-file "bookmark+") (autoload 'bmkp-autofile-add-tags "bookmark+") (autoload 'bmkp-untag-a-file "bookmark+") (autoload 'bmkp-autofile-remove-tags "bookmark+") (autoload 'bmkp-purge-notags-autofiles "bookmark+") (autoload 'bmkp-describe-bookmark "bookmark+") (autoload 'bmkp-describe-bookmark-internals "bookmark+") (autoload 'bmkp-list-defuns-in-commands-file "bookmark+") (autoload 'bmkp-set-bookmark-file-bookmark "bookmark+") (autoload 'bmkp-bookmark-file-jump "bookmark+") (autoload 'bmkp-bookmark-file-load-jump "bookmark+") (autoload 'bmkp-bookmark-file-switch-jump "bookmark+") (autoload 'bmkp-set-snippet-bookmark "bookmark+") (autoload 'bmkp-snippet-to-kill-ring "bookmark+") (autoload 'bmkp-set-desktop-bookmark "bookmark+") (autoload 'bmkp-desktop-change-dir "bookmark+") (autoload 'bmkp-desktop-read "bookmark+") (autoload 'bmkp-desktop-delete "bookmark+") (autoload 'bmkp-retrieve-icicle-search-hits "bookmark+") (autoload 'bmkp-retrieve-more-icicle-search-hits "bookmark+") (autoload 'bmkp-set-icicle-search-hits-bookmark "bookmark+") (autoload 'bmkp-wrap-bookmark-with-last-kbd-macro "bookmark+") (autoload 'bmkp-set-sequence-bookmark "bookmark+") (autoload 'bmkp-set-variable-list-bookmark "bookmark+") (autoload 'Info-bookmark-jump "bookmark+") (autoload 'bmkp-dired-subdirs "bookmark+") (autoload 'bmkp-jump-to-type "bookmark+") (autoload 'bmkp-jump-to-type-other-window "bookmark+") (autoload 'bmkp-autonamed-jump "bookmark+") (autoload 'bmkp-autonamed-jump-other-window "bookmark+") (autoload 'bmkp-autonamed-this-buffer-jump "bookmark+") (autoload 'bmkp-autonamed-this-buffer-jump-other-window "bookmark+") (autoload 'bmkp-bookmark-list-jump "bookmark+") (autoload 'bmkp-desktop-jump "bookmark+") (autoload 'bmkp-dired-jump "bookmark+") (autoload 'bmkp-dired-jump-other-window "bookmark+") (autoload 'bmkp-dired-this-dir-jump "bookmark+") (autoload 'bmkp-dired-this-dir-jump-other-window "bookmark+") (autoload 'bmkp-file-jump "bookmark+") (autoload 'bmkp-file-jump-other-window "bookmark+") (autoload 'bmkp-file-this-dir-jump "bookmark+") (autoload 'bmkp-file-this-dir-jump-other-window "bookmark+") (autoload 'bmkp-gnus-jump "bookmark+") (autoload 'bmkp-gnus-jump-other-window "bookmark+") (autoload 'bmkp-image-jump "bookmark+") (autoload 'bmkp-image-jump-other-window "bookmark+") (autoload 'bmkp-info-jump "bookmark+") (autoload 'bmkp-info-jump-other-window "bookmark+") (autoload 'bmkp-local-file-jump "bookmark+") (autoload 'bmkp-local-file-jump-other-window "bookmark+") (autoload 'bmkp-local-non-dir-file-jump "bookmark+") (autoload 'bmkp-local-non-dir-file-jump-other-window "bookmark+") (autoload 'bmkp-man-jump "bookmark+") (autoload 'bmkp-man-jump-other-window "bookmark+") (autoload 'bmkp-non-dir-file-jump "bookmark+") (autoload 'bmkp-non-dir-file-jump-other-window "bookmark+") (autoload 'bmkp-non-file-jump "bookmark+") (autoload 'bmkp-non-file-jump-other-window "bookmark+") (autoload 'bmkp-region-jump "bookmark+") (autoload 'bmkp-region-jump-other-window "bookmark+") (autoload 'bmkp-region-jump-narrow-indirect-other-window "bookmark+") (autoload 'bmkp-remote-file-jump "bookmark+") (autoload 'bmkp-remote-file-jump-other-window "bookmark+") (autoload 'bmkp-remote-non-dir-file-jump "bookmark+") (autoload 'bmkp-remote-non-dir-file-jump-other-window "bookmark+") (autoload 'bmkp-specific-buffers-jump "bookmark+") (autoload 'bmkp-specific-buffers-jump-other-window "bookmark+") (autoload 'bmkp-specific-files-jump "bookmark+") (autoload 'bmkp-specific-files-jump-other-window "bookmark+") (autoload 'bmkp-temporary-jump "bookmark+") (autoload 'bmkp-temporary-jump-other-window "bookmark+") (autoload 'bmkp-this-buffer-jump "bookmark+") (autoload 'bmkp-this-buffer-jump-other-window "bookmark+") (autoload 'bmkp-variable-list-jump "bookmark+") (autoload 'bmkp-url-jump "bookmark+") (autoload 'bmkp-url-jump-other-window "bookmark+") (autoload 'bmkp-w32-browser-jump "bookmark+") (autoload 'bmkp-w3m-jump "bookmark+") (autoload 'bmkp-w3m-jump-other-window "bookmark+") (autoload 'bmkp-all-tags-jump "bookmark+") (autoload 'bmkp-all-tags-jump-other-window "bookmark+") (autoload 'bmkp-all-tags-regexp-jump "bookmark+") (autoload 'bmkp-all-tags-regexp-jump-other-window "bookmark+") (autoload 'bmkp-some-tags-jump "bookmark+") (autoload 'bmkp-some-tags-jump-other-window "bookmark+") (autoload 'bmkp-some-tags-regexp-jump "bookmark+") (autoload 'bmkp-some-tags-regexp-jump-other-window "bookmark+") (autoload 'bmkp-file-all-tags-jump "bookmark+") (autoload 'bmkp-file-all-tags-jump-other-window "bookmark+") (autoload 'bmkp-file-all-tags-regexp-jump "bookmark+") (autoload 'bmkp-file-all-tags-regexp-jump-other-window "bookmark+") (autoload 'bmkp-file-some-tags-jump "bookmark+") (autoload 'bmkp-file-some-tags-jump-other-window "bookmark+") (autoload 'bmkp-file-some-tags-regexp-jump "bookmark+") (autoload 'bmkp-file-some-tags-regexp-jump-other-window "bookmark+") (autoload 'bmkp-file-this-dir-all-tags-jump "bookmark+") (autoload 'bmkp-file-this-dir-all-tags-jump-other-window "bookmark+") (autoload 'bmkp-file-this-dir-all-tags-regexp-jump "bookmark+") (autoload 'bmkp-file-this-dir-all-tags-regexp-jump-other-window "bookmark+") (autoload 'bmkp-file-this-dir-some-tags-jump "bookmark+") (autoload 'bmkp-file-this-dir-some-tags-jump-other-window "bookmark+") (autoload 'bmkp-file-this-dir-some-tags-regexp-jump "bookmark+") (autoload 'bmkp-file-this-dir-some-tags-regexp-jump-other-window "bookmark+") (autoload 'bmkp-autofile-jump "bookmark+") (autoload 'bmkp-autofile-jump-other-window "bookmark+") (autoload 'bmkp-autofile-all-tags-jump "bookmark+") (autoload 'bmkp-autofile-all-tags-jump-other-window "bookmark+") (autoload 'bmkp-autofile-all-tags-regexp-jump "bookmark+") (autoload 'bmkp-autofile-all-tags-regexp-jump-other-window "bookmark+") (autoload 'bmkp-autofile-some-tags-jump "bookmark+") (autoload 'bmkp-autofile-some-tags-jump-other-window "bookmark+") (autoload 'bmkp-autofile-some-tags-regexp-jump "bookmark+") (autoload 'bmkp-autofile-some-tags-regexp-jump-other-window "bookmark+") (autoload 'bmkp-find-file-all-tags-regexp-other-window "bookmark+") (autoload 'bmkp-find-file-some-tags "bookmark+") (autoload 'bmkp-find-file-some-tags-other-window "bookmark+") (autoload 'bmkp-find-file-some-tags-regexp "bookmark+") (autoload 'bmkp-find-file-some-tags-regexp-other-window "bookmark+") (autoload 'bmkp-jump-in-navlist "bookmark+") (autoload 'bmkp-jump-in-navlist-other-window "bookmark+") (autoload 'bmkp-cycle "bookmark+") (autoload 'bmkp-cycle-other-window "bookmark+") (autoload 'bmkp-cycle-this-file/buffer "bookmark+") (autoload 'bmkp-cycle-this-file/buffer-other-window "bookmark+") (autoload 'bmkp-cycle-this-file "bookmark+") (autoload 'bmkp-cycle-this-file-other-window "bookmark+") (autoload 'bmkp-cycle-this-buffer "bookmark+") (autoload 'bmkp-cycle-this-buffer-other-window "bookmark+") (autoload 'bmkp-next-bookmark "bookmark+") (autoload 'bmkp-previous-bookmark "bookmark+") (autoload 'bmkp-next-bookmark-other-window "bookmark+") (autoload 'bmkp-previous-bookmark-other-window "bookmark+") (autoload 'bmkp-next-bookmark-repeat "bookmark+") (autoload 'bmkp-previous-bookmark-repeat "bookmark+") (autoload 'bmkp-next-bookmark-other-window-repeat "bookmark+") (autoload 'bmkp-previous-bookmark-other-window-repeat "bookmark+") (autoload 'bmkp-next-bookmark-this-file/buffer "bookmark+") (autoload 'bmkp-previous-bookmark-this-file/buffer "bookmark+") (autoload 'bmkp-next-bookmark-this-file/buffer-repeat "bookmark+") (autoload 'bmkp-previous-bookmark-this-file/buffer-repeat "bookmark+") (autoload 'bmkp-next-bookmark-this-file "bookmark+") (autoload 'bmkp-previous-bookmark-this-file "bookmark+") (autoload 'bmkp-next-bookmark-this-file-repeat "bookmark+") (autoload 'bmkp-previous-bookmark-this-file-repeat "bookmark+") (autoload 'bmkp-next-bookmark-this-buffer "bookmark+") (autoload 'bmkp-previous-bookmark-this-buffer "bookmark+") (autoload 'bmkp-next-bookmark-this-buffer-repeat "bookmark+") (autoload 'bmkp-previous-bookmark-this-buffer-repeat "bookmark+") (autoload 'bmkp-next-bookmark-w32 "bookmark+") (autoload 'bmkp-previous-bookmark-w32 "bookmark+") (autoload 'bmkp-next-bookmark-w32-repeat "bookmark+") (autoload 'bmkp-previous-bookmark-w32-repeat "bookmark+") (autoload 'bmkp-toggle-autonamed-bookmark-set/delete "bookmark+") (autoload 'bmkp-set-autonamed-bookmark "bookmark+") (autoload 'bmkp-set-autonamed-bookmark-at-line "bookmark+") (autoload 'bmkp-set-autonamed-regexp-buffer "bookmark+") (autoload 'bmkp-set-autonamed-regexp-region "bookmark+") (autoload 'bmkp-delete-all-autonamed-for-this-buffer "bookmark+") (autoload 'bmkp-toggle-autotemp-on-set "bookmark+") (autoload 'bmkp-toggle-temporary-bookmark "bookmark+") (autoload 'bmkp-make-bookmark-temporary "bookmark+") (autoload 'bmkp-make-bookmark-savable "bookmark+") (autoload 'bmkp-delete-all-temporary-bookmarks "bookmark+") (autoload 'bmkp-delete-bookmarks "bookmark+") (register-definition-prefixes "bookmark+-1" '("bmk" "bookmark-")) (autoload 'bmkp-bmenu-omitted-bookmarks "bookmark+") (autoload 'bmkp-bmenu-commands-file "bookmark+") (autoload 'bmkp-bmenu-state-file "bookmark+") (autoload 'bmkp-bmenu-image-bookmark-icon-file "bookmark+") (autoload 'bmkp-bmenu-show-file-not-buffer-flag "bookmark+") (autoload 'bookmark-bmenu-mark "bookmark+") (autoload 'bookmark-bmenu-unmark "bookmark+") (autoload 'bmkp-bmenu-flag-for-deletion "bookmark+") (autoload 'bookmark-bmenu-delete "bookmark+") (autoload 'bmkp-bmenu-flag-for-deletion-backwards "bookmark+") (autoload 'bookmark-bmenu-delete-backwards "bookmark+") (autoload 'list-bookmarks "bookmark+") (autoload 'bookmark-bmenu-list "bookmark+") (autoload 'bookmark-bmenu-1-window "bookmark+") (autoload 'bookmark-bmenu-2-window "bookmark+") (autoload 'bookmark-bmenu-this-window "bookmark+") (autoload 'bookmark-bmenu-other-window "bookmark+") (autoload 'bookmark-bmenu-other-window "bookmark+") (autoload 'bookmark-bmenu-switch-other-window "bookmark+") (autoload 'bookmark-bmenu-other-window-with-mouse "bookmark+") (autoload 'bookmark-bmenu-show-annotation "bookmark+") (autoload 'bookmark-bmenu-execute-deletions "bookmark+") (autoload 'bookmark-bmenu-rename "bookmark+") (autoload 'bmkp-bmenu-show-only-autonamed-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-non-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-dired-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-function-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-gnus-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-icicles-search-hits-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-non-invokable-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-image-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-info-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-desktop-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-man-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-region-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-tagged-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-untagged-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-url-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-variable-list-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-snippet-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-w3m-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-temporary-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-bookmark-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-bookmark-list-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-all "bookmark+") (autoload 'bmkp-bmenu-show-only-autofile-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-orphaned-local-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-specific-buffer-bookmarks "bookmark+") (autoload 'bmkp-bmenu-show-only-specific-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-refresh-menu-list "bookmark+") (autoload 'bmkp-bmenu-filter-bookmark-name-incrementally "bookmark+") (autoload 'bmkp-bmenu-filter-file-name-incrementally "bookmark+") (autoload 'bmkp-bmenu-filter-annotation-incrementally "bookmark+") (autoload 'bmkp-bmenu-filter-tags-incrementally "bookmark+") (autoload 'bmkp-bmenu-toggle-show-only-unmarked "bookmark+") (autoload 'bmkp-bmenu-toggle-show-only-marked "bookmark+") (autoload 'bmkp-bmenu-mark-all "bookmark+") (autoload 'bmkp-bmenu-unmark-all "bookmark+") (autoload 'bmkp-bmenu-regexp-mark "bookmark+") (autoload 'bmkp-bmenu-mark-autofile-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-autonamed-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-bookmark-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-bookmark-list-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-desktop-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-dired-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-function-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-gnus-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-icicles-search-hits-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-image-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-info-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-man-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-non-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-non-invokable-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-orphaned-local-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-region-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-snippet-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-specific-buffer-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-specific-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-temporary-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-variable-list-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-url-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-w3m-bookmarks "bookmark+") (autoload 'bmkp-bmenu-mark-bookmarks-satisfying "bookmark+") (autoload 'bmkp-bmenu-toggle-marks "bookmark+") (autoload 'bmkp-bmenu-toggle-marked-temporary/savable "bookmark+") (autoload 'bmkp-bmenu-toggle-temporary "bookmark+") (autoload 'bmkp-bmenu-dired-marked "bookmark+") (autoload 'bmkp-bmenu-delete-marked "bookmark+") (autoload 'bmkp-bmenu-move-marked-to-bookmark-file "bookmark+") (autoload 'bmkp-bmenu-copy-marked-to-bookmark-file "bookmark+") (autoload 'bmkp-bmenu-create-bookmark-file-from-marked "bookmark+") (autoload 'bmkp-bmenu-set-bookmark-file-bookmark-from-marked "bookmark+") (autoload 'bmkp-bmenu-load-marked-bookmark-file-bookmarks "bookmark+") (autoload 'bmkp-bmenu-load-marking "bookmark+") (autoload 'bmkp-bmenu-load-marking-unmark-first "bookmark+") (autoload 'bmkp-bmenu-make-sequence-from-marked "bookmark+") (autoload 'bmkp-bmenu-omit "bookmark+") (autoload 'bmkp-bmenu-omit/unomit-marked "bookmark+") (autoload 'bmkp-bmenu-omit-marked "bookmark+") (autoload 'bmkp-bmenu-unomit-marked "bookmark+") (autoload 'bmkp-bmenu-show-only-omitted-bookmarks "bookmark+") (autoload 'bmkp-bmenu-search-marked-bookmarks-regexp "bookmark+") (autoload 'bmkp-bmenu-query-replace-marked-bookmarks-regexp "bookmark+") (autoload 'bmkp-bmenu-remove-all-tags "bookmark+") (autoload 'bmkp-bmenu-add-tags "bookmark+") (autoload 'bmkp-bmenu-set-tag-value "bookmark+") (autoload 'bmkp-bmenu-set-tag-value-for-marked "bookmark+") (autoload 'bmkp-bmenu-remove-tags "bookmark+") (autoload 'bmkp-bmenu-add-tags-to-marked "bookmark+") (autoload 'bmkp-bmenu-remove-tags-from-marked "bookmark+") (autoload 'bmkp-bmenu-list-tags-of-marked "bookmark+") (autoload 'bmkp-bmenu-mark-bookmarks-tagged-regexp "bookmark+") (autoload 'bmkp-bmenu-mark-bookmarks-tagged-all "bookmark+") (autoload 'bmkp-bmenu-mark-bookmarks-tagged-none "bookmark+") (autoload 'bmkp-bmenu-mark-bookmarks-tagged-some "bookmark+") (autoload 'bmkp-bmenu-mark-bookmarks-tagged-not-all "bookmark+") (autoload 'bmkp-bmenu-unmark-bookmarks-tagged-regexp "bookmark+") (autoload 'bmkp-bmenu-unmark-bookmarks-tagged-all "bookmark+") (autoload 'bmkp-bmenu-unmark-bookmarks-tagged-none "bookmark+") (autoload 'bmkp-bmenu-unmark-bookmarks-tagged-some "bookmark+") (autoload 'bmkp-bmenu-unmark-bookmarks-tagged-not-all "bookmark+") (autoload 'bmkp-bmenu-copy-tags "bookmark+") (autoload 'bmkp-bmenu-paste-add-tags "bookmark+") (autoload 'bmkp-bmenu-paste-replace-tags "bookmark+") (autoload 'bmkp-bmenu-paste-add-tags-to-marked "bookmark+") (autoload 'bmkp-bmenu-paste-replace-tags-for-marked "bookmark+") (autoload 'bmkp-bmenu-create/edit-annotations-for-marked "bookmark+") (autoload 'bmkp-bmenu-show-or-edit-annotation "bookmark+") (autoload 'bmkp-bmenu-jump-to-marked "bookmark+") (autoload 'bmkp-bmenu-w32-open "bookmark+") (autoload 'bmkp-bmenu-w32-open-with-mouse "bookmark+") (autoload 'bmkp-bmenu-w32-jump-to-marked "bookmark+") (autoload 'bmkp-bmenu-mode-status-help "bookmark+") (autoload 'bmkp-bmenu-define-jump-marked-command "bookmark+") (autoload 'bmkp-bmenu-define-command "bookmark+") (autoload 'bmkp-bmenu-define-full-snapshot-command "bookmark+") (autoload 'bmkp-define-tags-sort-command "bookmark+") (autoload 'bmkp-bmenu-relocate-marked "bookmark+") (autoload 'bmkp-copy-bookmark "bookmark+") (autoload 'bmkp-bmenu-clone-bookmark "bookmark+") (autoload 'bmkp-bmenu-edit-bookmark-name-and-location "bookmark+") (autoload 'bmkp-bmenu-edit-tags "bookmark+") (autoload 'bmkp-bmenu-edit-bookmark-record "bookmark+") (autoload 'bmkp-bmenu-edit-marked "bookmark+") (autoload 'bmkp-bmenu-quit "bookmark+") (autoload 'bmkp-bmenu-change-sort-order-repeat "bookmark+") (autoload 'bmkp-bmenu-change-sort-order "bookmark+") (autoload 'bmkp-reverse-sort-order "bookmark+") (autoload 'bmkp-reverse-multi-sort-order "bookmark+") (autoload 'bmkp-bmenu-show-this-annotation+move-down "bookmark+") (autoload 'bmkp-bmenu-show-this-annotation+move-up "bookmark+") (autoload 'bmkp-bmenu-describe-this+move-down "bookmark+") (autoload 'bmkp-bmenu-describe-this+move-up "bookmark+") (autoload 'bmkp-bmenu-describe-this-bookmark "bookmark+") (autoload 'bmkp-bmenu-describe-marked "bookmark+") (autoload 'bmkp-bmenu-mouse-3-menu "bookmark+") (register-definition-prefixes "bookmark+-bmu" '("bmkp-" "bookmark-bmenu-")) (register-definition-prefixes "bookmark+-key" '("bmkp-")) (autoload 'bmkp-auto-light-relocate-when-jump-flag "bookmark+") (autoload 'bmkp-auto-light-when-jump "bookmark+") (autoload 'bmkp-auto-light-when-set "bookmark+") (autoload 'bmkp-light-priorities "bookmark+") (autoload 'bmkp-light-style-autonamed "bookmark+") (autoload 'bmkp-light-style-non-autonamed "bookmark+") (autoload 'bmkp-light-style-autonamed-region "bookmark+") (autoload 'bmkp-light-style-non-autonamed-region "bookmark+") (autoload 'bmkp-light-threshold "bookmark+") (autoload 'bmkp-tooltip-content-function "bookmark+") (autoload 'bmkp-bmenu-show-only-lighted-bookmarks "bookmark+") (autoload 'bmkp-bmenu-light "bookmark+") (autoload 'bmkp-bmenu-light-marked "bookmark+") (autoload 'bmkp-bmenu-unlight "bookmark+") (autoload 'bmkp-bmenu-unlight-marked "bookmark+") (autoload 'bmkp-bmenu-set-lighting "bookmark+") (autoload 'bmkp-bmenu-set-lighting-for-marked "bookmark+") (autoload 'bmkp-bookmarks-lighted-at-point "bookmark+") (autoload 'bmkp-toggle-auto-light-when-jump "bookmark+") (autoload 'bmkp-lighted-jump "bookmark+") (autoload 'bmkp-lighted-jump-other-window "bookmark+") (autoload 'bmkp-lighted-here-jump-to-list "bookmark+") (autoload 'bmkp-unlight-bookmark "bookmark+") (autoload 'bmkp-unlight-bookmark-on-this-line "bookmark+") (autoload 'bmkp-unlight-bookmark-this-buffer "bookmark+") (autoload 'bmkp-unlight-bookmarks "bookmark+") (autoload 'bmkp-unlight-autonamed-this-buffer "bookmark+") (autoload 'bmkp-unlight-non-autonamed-this-buffer "bookmark+") (autoload 'bmkp-unlight-this-buffer "bookmark+") (autoload 'bmkp-toggle-auto-light-when-set "bookmark+") (autoload 'bmkp-set-lighting-for-bookmark "bookmark+") (autoload 'bmkp-set-lighting-for-buffer "bookmark+") (autoload 'bmkp-set-lighting-for-this-buffer "bookmark+") (autoload 'bmkp-light-bookmark "bookmark+") (autoload 'bmkp-light-bookmark-this-buffer "bookmark+") (autoload 'bmkp-light-bookmarks "bookmark+") (autoload 'bmkp-light-navlist-bookmarks "bookmark+") (autoload 'bmkp-light-this-buffer "bookmark+") (autoload 'bmkp-light-bookmarks-in-region "bookmark+") (autoload 'bmkp-light-autonamed-this-buffer "bookmark+") (autoload 'bmkp-light-non-autonamed-this-buffer "bookmark+") (autoload 'bmkp-cycle-lighted-this-buffer "bookmark+") (autoload 'bmkp-cycle-lighted-this-buffer-other-window "bookmark+") (autoload 'bmkp-next-lighted-this-buffer "bookmark+") (autoload 'bmkp-previous-lighted-this-buffer "bookmark+") (autoload 'bmkp-next-lighted-this-buffer-repeat "bookmark+") (autoload 'bmkp-previous-lighted-this-buffer-repeat "bookmark+") (autoload 'bmkp-describe-bookmark-lighted-here "bookmark+") (autoload 'bmkp-describe-bookmark-lighted-on-this-line "bookmark+") (register-definition-prefixes "bookmark+-lit" '("bmkp-")) (autoload 'bmkp-with-help-window "bookmark+") (autoload 'bmkp-with-output-to-plain-temp-buffer "bookmark+") (autoload 'bmkp-make-plain-predicate "bookmark+") (autoload 'bmkp-define-cycle-command "bookmark+") (autoload 'bmkp-define-next+prev-cycle-commands "bookmark+") (autoload 'bmkp-define-show-only-command "bookmark+") (autoload 'bmkp-define-sort-command "bookmark+") (autoload 'bmkp-define-file-sort-predicate "bookmark+") (autoload 'bmkp-define-history-variables "bookmark+") (autoload 'bmkp-menu-bar-make-toggle "bookmark+") (autoload 'bmkp-with-bookmark-dir "bookmark+") (register-definition-prefixes "bookmark+-mac" '("bmkp-")) (provide 'bookmark+-autoloads)) "ht" ((ht ht-autoloads) (register-definition-prefixes "ht" 'nil) (provide 'ht-autoloads)) "emojify" ((emojify-autoloads emojify) (autoload 'emojify-set-emoji-styles "emojify" "Set the type of emojis that should be displayed.

STYLES is the styles emoji styles that should be used, see `emojify-emoji-styles'

(fn STYLES)") (autoload 'emojify-mode "emojify" "Emojify mode

This is a minor mode.  If called interactively, toggle the
`Emojify mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `emojify-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-emojify-mode 'globalized-minor-mode t) (defvar global-emojify-mode nil "Non-nil if Global Emojify mode is enabled.
See the `global-emojify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-emojify-mode'.") (custom-autoload 'global-emojify-mode "emojify" nil) (autoload 'global-emojify-mode "emojify" "Toggle Emojify mode in all buffers.
With prefix ARG, enable Global Emojify mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Emojify mode is enabled in all buffers where `emojify-mode' would do it.

See `emojify-mode' for more information on Emojify mode.

(fn &optional ARG)" t) (autoload 'emojify-mode-line-mode "emojify" "Emojify mode line

This is a minor mode.  If called interactively, toggle the
`Emojify-Mode-Line mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `emojify-mode-line-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-emojify-mode-line-mode 'globalized-minor-mode t) (defvar global-emojify-mode-line-mode nil "Non-nil if Global Emojify-Mode-Line mode is enabled.
See the `global-emojify-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-emojify-mode-line-mode'.") (custom-autoload 'global-emojify-mode-line-mode "emojify" nil) (autoload 'global-emojify-mode-line-mode "emojify" "Toggle Emojify-Mode-Line mode in all buffers.
With prefix ARG, enable Global Emojify-Mode-Line mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Emojify-Mode-Line mode is enabled in all buffers where `emojify-mode-line-mode' would do it.

See `emojify-mode-line-mode' for more information on Emojify-Mode-Line mode.

(fn &optional ARG)" t) (autoload 'emojify-apropos-emoji "emojify" "Show Emojis that match PATTERN.

(fn PATTERN)" t) (autoload 'emojify-insert-emoji "emojify" "Interactively prompt for Emojis and insert them in the current buffer.

This respects the `emojify-emoji-styles' variable." t) (register-definition-prefixes "emojify" '("emojify-")) (provide 'emojify-autoloads)) "multiple-cursors" ((multiple-cursors-core multiple-cursors-autoloads mc-separate-operations mc-cycle-cursors multiple-cursors-pkg mc-hide-unmatched-lines-mode mc-mark-pop rectangular-region-mode mc-mark-more mc-edit-lines multiple-cursors) (register-definition-prefixes "mc-cycle-cursors" '("mc/")) (autoload 'mc/edit-lines "mc-edit-lines" "Add one cursor to each line of the active region.
Starts from mark and moves in straight down or up towards the
line point is on.

What is done with lines which are not long enough is governed by
`mc/edit-lines-empty-lines'.  The prefix argument ARG can be used
to override this.  If ARG is a symbol (when called from Lisp),
that symbol is used instead of `mc/edit-lines-empty-lines'.
Otherwise, if ARG negative, short lines will be ignored.  Any
other non-nil value will cause short lines to be padded.

(fn &optional ARG)" t) (autoload 'mc/edit-ends-of-lines "mc-edit-lines" "Add one cursor to the end of each line in the active region." t) (autoload 'mc/edit-beginnings-of-lines "mc-edit-lines" "Add one cursor to the beginning of each line in the active region." t) (register-definition-prefixes "mc-edit-lines" '("mc/edit-lines-empty-lines")) (autoload 'mc-hide-unmatched-lines-mode "mc-hide-unmatched-lines-mode" "Minor mode when enabled hides all lines where no cursors (and

also hum/lines-to-expand below and above) To make use of this
mode press \"C-'\" while multiple-cursor-mode is active. You can
still edit lines while you are in mc-hide-unmatched-lines
mode. To leave this mode press <return> or \"C-g\"

This is a minor mode.  If called interactively, toggle the
`Mc-Hide-Unmatched-Lines mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `mc-hide-unmatched-lines-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "mc-hide-unmatched-lines-mode" '("hum/")) (autoload 'mc/mark-next-like-this "mc-mark-more" "Find and mark the next part of the buffer matching the currently active region
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t) (autoload 'mc/mark-next-like-this-word "mc-mark-more" "Find and mark the next part of the buffer matching the currently active region
If no region is active, mark the word at the point and find the next match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t) (autoload 'mc/mark-next-word-like-this "mc-mark-more" "Find and mark the next word of the buffer matching the currently active region
The matching region must be a whole word to be a match
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t) (autoload 'mc/mark-next-symbol-like-this "mc-mark-more" "Find and mark the next symbol of the buffer matching the currently active region
The matching region must be a whole symbol to be a match
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t) (autoload 'mc/mark-previous-like-this "mc-mark-more" "Find and mark the previous part of the buffer matching the
currently active region.

If no region is active ,add a cursor on the previous line.

With negative ARG, delete the last one instead.

With zero ARG, skip the last one and mark next.

(fn ARG)" t) (autoload 'mc/mark-previous-like-this-word "mc-mark-more" "Find and mark the previous part of the buffer matching the
currently active region.

If no region is active, mark the word at the point and find the
previous match.

With negative ARG, delete the last one instead.

With zero ARG, skip the last one and mark previous.

(fn ARG)" t) (autoload 'mc/mark-previous-word-like-this "mc-mark-more" "Find and mark the previous part of the buffer matching the
currently active region.

The matching region must be a whole word to be a match.

If no region is active, add a cursor on the previous line.

With negative ARG, delete the last one instead.

With zero ARG, skip the last one and mark next.

(fn ARG)" t) (autoload 'mc/mark-previous-symbol-like-this "mc-mark-more" "Find and mark the previous part of the buffer matching
the currently active region.

The matching region must be a whole symbol to be a match.

If no region is active add a cursor on the previous line.

With negative ARG, delete the last one instead.

With zero ARG, skip the last one and mark next.

(fn ARG)" t) (autoload 'mc/mark-next-lines "mc-mark-more" "

(fn ARG)" t) (autoload 'mc/mark-previous-lines "mc-mark-more" "

(fn ARG)" t) (autoload 'mc/unmark-next-like-this "mc-mark-more" "Deselect next part of the buffer matching the currently active region." t) (autoload 'mc/unmark-previous-like-this "mc-mark-more" "Deselect prev part of the buffer matching the currently active region." t) (autoload 'mc/skip-to-next-like-this "mc-mark-more" "Skip the current one and select the next part of the buffer
matching the currently active region." t) (autoload 'mc/skip-to-previous-like-this "mc-mark-more" "Skip the current one and select the prev part of the buffer
matching the currently active region." t) (autoload 'mc/mark-all-like-this "mc-mark-more" "Find and mark all the parts of the buffer matching the currently active region" t) (autoload 'mc/mark-all-words-like-this "mc-mark-more" nil t) (autoload 'mc/mark-all-symbols-like-this "mc-mark-more" nil t) (autoload 'mc/mark-all-in-region "mc-mark-more" "Find and mark all the parts in the region matching the given search

(fn BEG END &optional SEARCH)" t) (autoload 'mc/mark-all-in-region-regexp "mc-mark-more" "Find and mark all the parts in the region matching the given regexp.

(fn BEG END)" t) (autoload 'mc/mark-more-like-this-extended "mc-mark-more" "Like mark-more-like-this, but then lets you adjust with arrow keys.
The adjustments work like this:

   <up>    Mark previous like this and set direction to \\='up
   <down>  Mark next like this and set direction to \\='down

If direction is \\='up:

   <left>  Skip past the cursor furthest up
   <right> Remove the cursor furthest up

If direction is \\='down:

   <left>  Remove the cursor furthest down
   <right> Skip past the cursor furthest down

The bindings for these commands can be changed.
See `mc/mark-more-like-this-extended-keymap'." t) (autoload 'mc/mark-all-like-this-dwim "mc-mark-more" "Tries to guess what you want to mark all of.
Can be pressed multiple times to increase selection.

With prefix, it behaves the same as original `mc/mark-all-like-this'

(fn ARG)" t) (autoload 'mc/mark-all-dwim "mc-mark-more" "Tries even harder to guess what you want to mark all of.

If the region is active and spans multiple lines, it will behave
as if `mc/mark-all-in-region'. With the prefix ARG, it will call
`mc/edit-lines' instead.

If the region is inactive or on a single line, it will behave like
`mc/mark-all-like-this-dwim'.

(fn ARG)" t) (autoload 'mc/mark-all-like-this-in-defun "mc-mark-more" "Mark all like this in defun." t) (autoload 'mc/mark-all-words-like-this-in-defun "mc-mark-more" "Mark all words like this in defun." t) (autoload 'mc/mark-all-symbols-like-this-in-defun "mc-mark-more" "Mark all symbols like this in defun." t) (autoload 'mc/toggle-cursor-on-click "mc-mark-more" "Add a cursor where you click, or remove a fake cursor that is
already there.

(fn EVENT)" t) (defalias 'mc/add-cursor-on-click 'mc/toggle-cursor-on-click) (autoload 'mc/mark-sgml-tag-pair "mc-mark-more" "Mark the tag we're in and its pair for renaming." t) (register-definition-prefixes "mc-mark-more" '("mc--" "mc/")) (autoload 'mc/mark-pop "mc-mark-pop" "Add a cursor at the current point, pop off mark ring and jump
to the popped mark." t) (autoload 'mc/insert-numbers "mc-separate-operations" "Insert increasing numbers for each cursor, starting at
`mc/insert-numbers-default' or ARG.

(fn ARG)" t) (autoload 'mc/insert-letters "mc-separate-operations" "Insert increasing letters for each cursor, starting at 0 or ARG.
     Where letter[0]=a letter[2]=c letter[26]=aa

(fn ARG)" t) (autoload 'mc/reverse-regions "mc-separate-operations" nil t) (autoload 'mc/sort-regions "mc-separate-operations" nil t) (autoload 'mc/vertical-align "mc-separate-operations" "Aligns all cursors vertically with a given CHARACTER to the one with the
highest column number (the rightest).
Might not behave as intended if more than one cursors are on the same line.

(fn CHARACTER)" t) (autoload 'mc/vertical-align-with-space "mc-separate-operations" "Aligns all cursors with whitespace like `mc/vertical-align' does" t) (register-definition-prefixes "mc-separate-operations" '("mc--" "mc/insert-numbers-default")) (autoload 'activate-cursor-for-undo "multiple-cursors-core" "Called when undoing to temporarily activate the fake cursor
which action is being undone.

(fn ID)") (autoload 'multiple-cursors-mode "multiple-cursors-core" "Mode while multiple cursors are active.

This is a minor mode.  If called interactively, toggle the
`Multiple-Cursors mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `multiple-cursors-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "multiple-cursors-core" '("deactivate-cursor-after-undo" "mc--" "mc/" "unsupported-cmd")) (autoload 'set-rectangular-region-anchor "rectangular-region-mode" "Anchors the rectangular region at point.

Think of this one as `set-mark' except you're marking a
rectangular region. It is an exceedingly quick way of adding
multiple cursors to multiple lines." t) (autoload 'rectangular-region-mode "rectangular-region-mode" "A mode for creating a rectangular region to edit

This is a minor mode.  If called interactively, toggle the
`Rectangular-Region mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `rectangular-region-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "rectangular-region-mode" '("rectangular-region-mode" "rrm/")) (provide 'multiple-cursors-autoloads)) "jinx" ((jinx jinx-autoloads) (put 'jinx-languages 'safe-local-variable #'stringp) (put 'jinx-local-words 'safe-local-variable #'stringp) (put 'jinx-mode 'safe-local-variable #'not) (autoload 'jinx-languages "jinx" "Set languages locally or globally to LANGS.
LANGS should be one or more language codes as a string, separated
by whitespace.  When called interactively, the language codes are
read via `completing-read-multiple'.  If the prefix argument
GLOBAL is non-nil, the languages are changed globally for all
buffers.  See also the variable `jinx-languages'.

(fn LANGS &optional GLOBAL)" t) (autoload 'jinx-correct-all "jinx" "Correct all misspelled words in the buffer.
With prefix argument ONLY-CHECK, only check the buffer and highlight all
misspellings, but do not open the correction UI.

(fn &optional ONLY-CHECK)" t) (autoload 'jinx-correct-nearest "jinx" "Correct nearest misspelled word." t) (autoload 'jinx-correct-word "jinx" "Correct word between START and END, by default the word before point.
Suggest corrections even if the word is not misspelled.
Optionally insert INITIAL input in the minibuffer.

(fn &optional START END INITIAL)" t) (autoload 'jinx-correct "jinx" "Correct word depending on prefix ARG.
This command dispatches to the following commands:
  - `jinx-correct-nearest': If prefix ARG is nil, correct nearest
    misspelled word.
  - `jinx-correct-all': If a region is marked, or if prefix ARG
    is 4, corresponding to \\[universal-argument] pressed once,
    correct all misspelled words.
  - `jinx-correct-word': If prefix ARG is 16, corresponding to
    \\[universal-argument] pressed twice, correct word before point.
  - If prefix ARG is 64, corresponding to \\[universal-argument] pressed
    three times, check the whole buffer, but do not open the correction
    UI.

(fn &optional ARG)" t) (autoload 'jinx-mode "jinx" "Enchanted Spell Checker.

This is a minor mode.  If called interactively, toggle the `Jinx
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `jinx-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-jinx-mode 'globalized-minor-mode t) (defvar global-jinx-mode nil "Non-nil if Global Jinx mode is enabled.
See the `global-jinx-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-jinx-mode'.") (custom-autoload 'global-jinx-mode "jinx" nil) (autoload 'global-jinx-mode "jinx" "Toggle Jinx mode in all buffers.
With prefix ARG, enable Global Jinx mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Jinx mode is enabled in all buffers where `jinx--on' would do it.

See `jinx-mode' for more information on Jinx mode.

(fn &optional ARG)" t) (register-definition-prefixes "jinx" '("global-jinx-modes" "jinx-")) (provide 'jinx-autoloads)) "ripgrep" ((ripgrep-autoloads ripgrep) (autoload 'ripgrep-regexp "ripgrep" "Run a ripgrep search with `REGEXP' rooted at `DIRECTORY'.
`ARGS' provides Ripgrep command line arguments.

(fn REGEXP DIRECTORY &optional ARGS)" t) (register-definition-prefixes "ripgrep" '("ripgrep")) (provide 'ripgrep-autoloads)) "sudo-edit" ((sudo-edit sudo-edit-autoloads) (autoload 'sudo-edit-set-header "sudo-edit" "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook' and `dired-file-hook'.") (defvar sudo-edit-indicator-mode nil "Non-nil if Sudo-Edit-Indicator mode is enabled.
See the `sudo-edit-indicator-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `sudo-edit-indicator-mode'.") (custom-autoload 'sudo-edit-indicator-mode "sudo-edit" nil) (autoload 'sudo-edit-indicator-mode "sudo-edit" "Indicates editing as root by displaying a message in the header line.

This is a global minor mode.  If called interactively, toggle the
`Sudo-Edit-Indicator mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='sudo-edit-indicator-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'sudo-edit "sudo-edit" "Edit currently visited file as another user, by default `sudo-edit-user'.

With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file.

(fn &optional ARG)" t) (autoload 'sudo-edit-find-file "sudo-edit" "Edit FILENAME as another user, by default `sudo-edit-user'.

(fn FILENAME)" t) (register-definition-prefixes "sudo-edit" '("sudo-edit-")) (provide 'sudo-edit-autoloads)) "prescient" ((prescient-autoloads prescient) (autoload 'prescient-filter "prescient" "Use QUERY to filter list of CANDIDATES.

CANDIDATES is a completion table, such as a list of strings
or a function as defined in the Info node
`(elisp)Programmed Completion'.

QUERY is a string containing the sub-queries, which are gotten
using `prescient-split-query'. Each sub-query is used to produce
a regular expression according to the filter methods listed in
`prescient-filter-method'. A candidate must match every regular
expression made from the sub-queries to be included in the list
of returned candidates.

PRED is the predicate used with the completion table, as
described in the above Info node.

This function does not modify CANDIDATES; it always make a new
copy of the list.

(fn QUERY CANDIDATES &optional PRED)") (autoload 'prescient-completion-sort "prescient" "Sort the filtered CANDIDATES.

This function will always sort candidates using the function
`prescient-sort'. When CANDIDATES has been filtered using the
`prescient' completion style, it can optionally also sort them
using the function `prescient-sort-full-matches-first'.

This function checks for the properties `prescient-regexps' and
`prescient-ignore-case' on any candidate in CANDIDATES (though
they are stored on the first candidate returned by
`prescient-filter'). These properties are used for implementing
the user option `prescient-sort-full-matches-first'.

(fn CANDIDATES)") (autoload 'prescient-all-completions "prescient" "`all-completions' using prescient.el.

STRING is the input. TABLE is a completion table. PRED is a
predicate that further restricts the matching candidates. POINT
would be the current point, but it is not used by this function.
See the function `all-completions' for more information.

This function returns a list of completions whose final `cdr' is
the length of the prefix string used for completion (which might
be all or just part of STRING).

When `completion-lazy-hilit' is bound and non-nil, then this
function sets `completion-lazy-hilit-fn'. Otherwise, if
`prescient-completion-highlight-matches' is non-nil, this
function propertizes all of the returned completions using the
face `prescient-primary-highlight' and the face
`prescient-secondary-highlight'.

(fn STRING TABLE &optional PRED POINT)") (autoload 'prescient-try-completion "prescient" "`try-completion' using Prescient.

STRING is the input.  TABLE is a completion table.  PRED is a
predicate.  POINT is the current point.  See the function
`try-completion' for more information.

If there are no matches, this function returns nil. If the only
match equals STRING, this function returns t. Otherwise, this
function returns a cons cell of the completed string and its
length. If there is more than one match, that completed string is
actually just the input, in which case nothing happens.

(fn STRING TABLE &optional PRED POINT)") (add-to-list 'completion-styles-alist '(prescient prescient-try-completion prescient-all-completions "Filtering using prescient.el.
For sorting, see the function `prescient-completion-sort'.")) (autoload 'prescient-create-and-bind-toggle-command "prescient" "Create and bind a command to toggle the use of a filter method.

The created command toggles the FILTER-TYPE method on
or off buffer locally, and doesn't affect the default
behavior (determined by `prescient-filter-method').

The created command is bound to KBD-STRING in
`prescient-toggle-map'. This map is itself bound to `M-s'
in the completion buffer when `selectrum-prescient-mode' or
`vertico-prescient-mode' are enabled.

FILTER-TYPE is an unquoted symbol that can be used in
`prescient-filter-method'. KBD-STRING is a string that can be
passed to `kbd'.

(fn FILTER-TYPE KBD-STRING)" nil t) (register-definition-prefixes "prescient" '("prescient-" "selectrum-")) (provide 'prescient-autoloads)) "consult" ((consult-info consult-flymake consult-xref consult-compile consult-autoloads consult consult-kmacro consult-org consult-imenu consult-register) (autoload 'consult-completion-in-region "consult" "Use minibuffer completion as the UI for `completion-at-point'.

The function is called with 4 arguments: START END COLLECTION
PREDICATE.  The arguments and expected return value are as
specified for `completion-in-region'.  Use this function as a
value for `completion-in-region-function'.

(fn START END COLLECTION &optional PREDICATE)") (autoload 'consult-outline "consult" "Jump to an outline heading, obtained by matching against `outline-regexp'.

This command supports narrowing to a heading level and candidate
preview.  The initial narrowing LEVEL can be given as prefix
argument.  The symbol at point is added to the future history.

(fn &optional LEVEL)" t) (autoload 'consult-mark "consult" "Jump to a marker in MARKERS list (defaults to buffer-local `mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history.

(fn &optional MARKERS)" t) (autoload 'consult-global-mark "consult" "Jump to a marker in MARKERS list (defaults to `global-mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history.

(fn &optional MARKERS)" t) (autoload 'consult-line "consult" "Search for a matching line.

Depending on the setting `consult-point-placement' the command
jumps to the beginning or the end of the first match on the line
or the line beginning.  The default candidate is the non-empty
line next to point.  This command obeys narrowing.  Optional
INITIAL input can be provided.  The search starting point is
changed if the START prefix argument is set.  The symbol at point
and the last `isearch-string' is added to the future history.

(fn &optional INITIAL START)" t) (autoload 'consult-line-multi "consult" "Search for a matching line in multiple buffers.

By default search across all project buffers.  If the prefix
argument QUERY is non-nil, all buffers are searched.  Optional
INITIAL input can be provided.  The symbol at point and the last
`isearch-string' is added to the future history.  In order to
search a subset of buffers, QUERY can be set to a plist according
to `consult--buffer-query'.

(fn QUERY &optional INITIAL)" t) (autoload 'consult-keep-lines "consult" "Select a subset of the lines in the current buffer with live preview.

The selected lines are kept and the other lines are deleted.  When called
interactively, the lines selected are those that match the minibuffer input.  In
order to match the inverse of the input, prefix the input with `! '.  When
called from Elisp, the filtering is performed by a FILTER function.  This
command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

(fn FILTER &optional INITIAL)" t) (autoload 'consult-focus-lines "consult" "Hide or show lines using overlays.

The selected lines are shown and the other lines hidden.  When called
interactively, the lines selected are those that match the minibuffer input.  In
order to match the inverse of the input, prefix the input with `! '.  With
optional prefix argument SHOW reveal the hidden lines.  Alternatively the
command can be restarted to reveal the lines.  When called from Elisp, the
filtering is performed by a FILTER function.  This command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

(fn FILTER &optional SHOW INITIAL)" t) (autoload 'consult-goto-line "consult" "Read line number and jump to the line with preview.

Enter either a line number to jump to the first column of the
given line or line:column in order to jump to a specific column.
Jump directly if a line number is given as prefix ARG.  The
command respects narrowing and the settings
`consult-goto-line-numbers' and `consult-line-numbers-widen'.

(fn &optional ARG)" t) (autoload 'consult-recent-file "consult" "Find recent file using `completing-read'." t) (autoload 'consult-mode-command "consult" "Run a command from any of the given MODES.

If no MODES are specified, use currently active major and minor modes.

(fn &rest MODES)" t) (autoload 'consult-yank-from-kill-ring "consult" "Select STRING from the kill ring and insert it.
With prefix ARG, put point at beginning, and mark at end, like `yank' does.

This command behaves like `yank-from-kill-ring', which also offers a
`completing-read' interface to the `kill-ring'.  Additionally the
Consult version supports preview of the selected string.

(fn STRING &optional ARG)" t) (autoload 'consult-yank-pop "consult" "If there is a recent yank act like `yank-pop'.

Otherwise select string from the kill ring and insert it.
See `yank-pop' for the meaning of ARG.

This command behaves like `yank-pop', which also offers a
`completing-read' interface to the `kill-ring'.  Additionally the
Consult version supports preview of the selected string.

(fn &optional ARG)" t) (autoload 'consult-yank-replace "consult" "Select STRING from the kill ring.

If there was no recent yank, insert the string.
Otherwise replace the just-yanked string with the selected string.

(fn STRING)" t) (autoload 'consult-bookmark "consult" "If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing.  See the
variable `consult-bookmark-narrow' for the narrowing configuration.

(fn NAME)" t) (autoload 'consult-complex-command "consult" "Select and evaluate command from the command history.

This command can act as a drop-in replacement for `repeat-complex-command'." t) (autoload 'consult-history "consult" "Insert string from HISTORY of current buffer.
In order to select from a specific HISTORY, pass the history
variable as argument.  INDEX is the name of the index variable to
update, if any.  BOL is the function which jumps to the beginning
of the prompt.  See also `cape-history' from the Cape package.

(fn &optional HISTORY INDEX BOL)" t) (autoload 'consult-isearch-history "consult" "Read a search string with completion from the Isearch history.

This replaces the current search string if Isearch is active, and
starts a new Isearch session otherwise." t) (autoload 'consult-minor-mode-menu "consult" "Enable or disable minor mode.

This is an alternative to `minor-mode-menu-from-indicator'." t) (autoload 'consult-theme "consult" "Disable current themes and enable THEME from `consult-themes'.

The command supports previewing the currently selected theme.

(fn THEME)" t) (autoload 'consult-buffer "consult" "Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files as
virtual buffers.  Buffers are previewed.  Narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding
keys.  In order to determine the project-specific files and buffers, the
`consult-project-function' is used.  The virtual buffer SOURCES
default to `consult-buffer-sources'.  See `consult--multi' for the
configuration of the virtual buffer sources.

(fn &optional SOURCES)" t) (autoload 'consult-project-buffer "consult" "Enhanced `project-switch-to-buffer' command with support for virtual buffers.
The command may prompt you for a project directory if it is invoked from
outside a project.  See `consult-buffer' for more details." t) (autoload 'consult-buffer-other-window "consult" "Variant of `consult-buffer', switching to a buffer in another window." t) (autoload 'consult-buffer-other-frame "consult" "Variant of `consult-buffer', switching to a buffer in another frame." t) (autoload 'consult-buffer-other-tab "consult" "Variant of `consult-buffer', switching to a buffer in another tab." t) (autoload 'consult-grep "consult" "Search with `grep' for files in DIR where the content matches a regexp.

The initial input is given by the INITIAL argument.  DIR can be nil, a
directory string or a list of file/directory paths.  If `consult-grep'
is called interactively with a prefix argument, the user can specify the
directories or files to search in.  Multiple directories or files must
be separated by comma in the minibuffer, since they are read via
`completing-read-multiple'.  By default the project directory is used if
`consult-project-function' is defined and returns non-nil.  Otherwise
the `default-directory' is searched.  If the command is invoked with a
double prefix argument (twice `C-u') the user is asked for a project, if
not yet inside a project, or the current project is searched.

The input string is split, the first part of the string (grep input) is
passed to the asynchronous grep process and the second part of the
string is passed to the completion-style filtering.

The input string is split at a punctuation character, which is given as
the first character of the input string.  The format is similar to
Perl-style regular expressions, e.g., /regexp/.  Furthermore command
line options can be passed to grep, specified behind --.  The overall
prompt input has the form `#async-input -- grep-opts#filter-string'.

Note that the grep input string is transformed from Emacs regular
expressions to Posix regular expressions.  Always enter Emacs regular
expressions at the prompt.  `consult-grep' behaves like builtin Emacs
search commands, e.g., Isearch, which take Emacs regular expressions.
Furthermore the asynchronous input split into words, each word must
match separately and in any order.  See `consult--regexp-compiler' for
the inner workings.  In order to disable transformations of the grep
input, adjust `consult--regexp-compiler' accordingly.

Here we give a few example inputs:

#alpha beta         : Search for alpha and beta in any order.
#alpha.*beta        : Search for alpha before beta.
#\\(alpha\\|beta\\) : Search for alpha or beta (Note Emacs syntax!)
#word -- -C3        : Search for word, include 3 lines as context
#first#second       : Search for first, quick filter for second.

The symbol at point is added to the future history.

(fn &optional DIR INITIAL)" t) (autoload 'consult-git-grep "consult" "Search with `git grep' for files in DIR with INITIAL input.
See `consult-grep' for details.

(fn &optional DIR INITIAL)" t) (autoload 'consult-ripgrep "consult" "Search with `rg' for files in DIR with INITIAL input.
See `consult-grep' for details.

(fn &optional DIR INITIAL)" t) (autoload 'consult-find "consult" "Search for files with `find' in DIR.
The file names must match the input regexp.  INITIAL is the
initial minibuffer input.  See `consult-grep' for details
regarding the asynchronous search and the arguments.

(fn &optional DIR INITIAL)" t) (autoload 'consult-fd "consult" "Search for files with `fd' in DIR.
The file names must match the input regexp.  INITIAL is the
initial minibuffer input.  See `consult-grep' for details
regarding the asynchronous search and the arguments.

(fn &optional DIR INITIAL)" t) (autoload 'consult-locate "consult" "Search with `locate' for files which match input given INITIAL input.

The input is treated literally such that locate can take advantage of
the locate database index.  Regular expressions would often force a slow
linear search through the entire database.  The locate process is started
asynchronously, similar to `consult-grep'.  See `consult-grep' for more
details regarding the asynchronous search.

(fn &optional INITIAL)" t) (autoload 'consult-man "consult" "Search for man page given INITIAL input.

The input string is not preprocessed and passed literally to the
underlying man commands.  The man process is started asynchronously,
similar to `consult-grep'.  See `consult-grep' for more details regarding
the asynchronous search.

(fn &optional INITIAL)" t) (register-definition-prefixes "consult" '("consult-")) (autoload 'consult-compile-error "consult-compile" "Jump to a compilation error in the current buffer.

This command collects entries from compilation buffers and grep
buffers related to the current buffer.  The command supports
preview of the currently selected error." t) (register-definition-prefixes "consult-compile" '("consult-compile--")) (autoload 'consult-flymake "consult-flymake" "Jump to Flymake diagnostic.
When PROJECT is non-nil then prompt with diagnostics from all
buffers in the current project instead of just the current buffer.

(fn &optional PROJECT)" t) (register-definition-prefixes "consult-flymake" '("consult-flymake--")) (autoload 'consult-imenu "consult-imenu" "Select item from flattened `imenu' using `completing-read' with preview.

The command supports preview and narrowing.  See the variable
`consult-imenu-config', which configures the narrowing.
The symbol at point is added to the future history.

See also `consult-imenu-multi'." t) (autoload 'consult-imenu-multi "consult-imenu" "Select item from the imenus of all buffers from the same project.

In order to determine the buffers belonging to the same project, the
`consult-project-function' is used.  Only the buffers with the
same major mode as the current buffer are used.  See also
`consult-imenu' for more details.  In order to search a subset of buffers,
QUERY can be set to a plist according to `consult--buffer-query'.

(fn &optional QUERY)" t) (register-definition-prefixes "consult-imenu" '("consult-imenu-")) (autoload 'consult-info "consult-info" "Full text search through info MANUALS.

(fn &rest MANUALS)" t) (register-definition-prefixes "consult-info" '("consult-info--")) (autoload 'consult-kmacro "consult-kmacro" "Run a chosen keyboard macro.

With prefix ARG, run the macro that many times.
Macros containing mouse clicks are omitted.

(fn ARG)" t) (register-definition-prefixes "consult-kmacro" '("consult-kmacro--")) (autoload 'consult-org-heading "consult-org" "Jump to an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, all entries of the current
buffer are offered.

(fn &optional MATCH SCOPE)" t) (autoload 'consult-org-agenda "consult-org" "Jump to an Org agenda heading.

By default, all agenda entries are offered.  MATCH is as in
`org-map-entries' and can used to refine this.

(fn &optional MATCH)" t) (register-definition-prefixes "consult-org" '("consult-org--")) (autoload 'consult-register-window "consult-register" "Enhanced drop-in replacement for `register-preview'.

BUFFER is the window buffer.
SHOW-EMPTY must be t if the window should be shown for an empty register list.

(fn BUFFER &optional SHOW-EMPTY)") (autoload 'consult-register-format "consult-register" "Enhanced preview of register REG.
This function can be used as `register-preview-function'.
If COMPLETION is non-nil format the register for completion.

(fn REG &optional COMPLETION)") (autoload 'consult-register "consult-register" "Load register and either jump to location or insert the stored text.

This command is useful to search the register contents.  For quick access
to registers it is still recommended to use the register functions
`consult-register-load' and `consult-register-store' or the built-in
built-in register access functions.  The command supports narrowing, see
`consult-register--narrow'.  Marker positions are previewed.  See
`jump-to-register' and `insert-register' for the meaning of prefix ARG.

(fn &optional ARG)" t) (autoload 'consult-register-load "consult-register" "Do what I mean with a REG.

For a window configuration, restore it.  For a number or text, insert it.
For a location, jump to it.  See `jump-to-register' and `insert-register'
for the meaning of prefix ARG.

(fn REG &optional ARG)" t) (autoload 'consult-register-store "consult-register" "Store register dependent on current context, showing an action menu.

With an active region, store/append/prepend the contents, optionally
deleting the region when a prefix ARG is given.  With a numeric prefix
ARG, store or add the number.  Otherwise store point, frameset, window or
kmacro.

(fn ARG)" t) (register-definition-prefixes "consult-register" '("consult-register-")) (autoload 'consult-xref "consult-xref" "Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments.

(fn FETCHER &optional ALIST)") (register-definition-prefixes "consult-xref" '("consult-xref--")) (provide 'consult-autoloads)) "marginalia" ((marginalia-autoloads marginalia) (defvar marginalia-mode nil "Non-nil if Marginalia mode is enabled.
See the `marginalia-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `marginalia-mode'.") (custom-autoload 'marginalia-mode "marginalia" nil) (autoload 'marginalia-mode "marginalia" "Annotate completion candidates with richer information.

This is a global minor mode.  If called interactively, toggle the
`Marginalia mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='marginalia-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'marginalia-cycle "marginalia" "Cycle between annotators in `marginalia-annotator-registry'." t) (function-put 'marginalia-cycle 'completion-predicate #'(lambda (&rest _) (> (minibuffer-depth) 1))) (register-definition-prefixes "marginalia" '("marginalia-")) (provide 'marginalia-autoloads)) "all-the-icons" ((all-the-icons-autoloads all-the-icons-faces all-the-icons) (autoload 'all-the-icons-icon-for-dir "all-the-icons" "Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

Note: You want chevron, please use `all-the-icons-icon-for-dir-with-chevron'.

(fn DIR &rest ARG-OVERRIDES)") (autoload 'all-the-icons-icon-for-file "all-the-icons" "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn FILE &rest ARG-OVERRIDES)") (autoload 'all-the-icons-icon-for-mode "all-the-icons" "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn MODE &rest ARG-OVERRIDES)") (autoload 'all-the-icons-icon-for-url "all-the-icons" "Get the formatted icon for URL.
If an icon for URL isn't found in `all-the-icons-url-alist', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn URL &rest ARG-OVERRIDES)") (autoload 'all-the-icons-install-fonts "all-the-icons" "Helper function to download and install the latests fonts based on OS.
When PFX is non-nil, ignore the prompt and just install

(fn &optional PFX)" t) (autoload 'all-the-icons-insert "all-the-icons" "Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When FAMILY is non-nil, limit the candidates to the icon set matching it.

(fn &optional ARG FAMILY)" t) (register-definition-prefixes "all-the-icons" '("all-the-icons-")) (provide 'all-the-icons-autoloads)) "all-the-icons-completion" ((all-the-icons-completion-autoloads all-the-icons-completion) (autoload 'all-the-icons-completion-marginalia-setup "all-the-icons-completion" "Hook to `marginalia-mode-hook' to bind `all-the-icons-completion-mode' to it.") (defvar all-the-icons-completion-mode nil "Non-nil if All-The-Icons-Completion mode is enabled.
See the `all-the-icons-completion-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `all-the-icons-completion-mode'.") (custom-autoload 'all-the-icons-completion-mode "all-the-icons-completion" nil) (autoload 'all-the-icons-completion-mode "all-the-icons-completion" "Add icons to completion candidates.

This is a global minor mode.  If called interactively, toggle the
`All-The-Icons-Completion mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='all-the-icons-completion-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "all-the-icons-completion" '("all-the-icons-completion-completion-metadata-get")) (provide 'all-the-icons-completion-autoloads)) "all-the-icons-dired" ((all-the-icons-dired all-the-icons-dired-autoloads) (autoload 'all-the-icons-dired-mode "all-the-icons-dired" "Display all-the-icons icon for each file in a Dired buffer.

This is a minor mode.  If called interactively, toggle the
`All-The-Icons-Dired mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `all-the-icons-dired-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "all-the-icons-dired" '("all-the-icons-dired-")) (provide 'all-the-icons-dired-autoloads)) "corfu" ((corfu-info corfu-indexed corfu-history corfu-popupinfo corfu-echo corfu corfu-quick corfu-autoloads) (autoload 'corfu-mode "corfu" "COmpletion in Region FUnction.

This is a minor mode.  If called interactively, toggle the `Corfu
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `corfu-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-corfu-mode 'globalized-minor-mode t) (defvar global-corfu-mode nil "Non-nil if Global Corfu mode is enabled.
See the `global-corfu-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-corfu-mode'.") (custom-autoload 'global-corfu-mode "corfu" nil) (autoload 'global-corfu-mode "corfu" "Toggle Corfu mode in all buffers.
With prefix ARG, enable Global Corfu mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Corfu mode is enabled in all buffers where `corfu--on' would do it.

See `corfu-mode' for more information on Corfu mode.

`global-corfu-modes' is used to control which modes this minor mode is used in.

(fn &optional ARG)" t) (defvar global-corfu-modes t "Which major modes `corfu-mode' is switched on in.
This variable can be either t (all major modes), nil (no major modes),
or a list of modes and (not modes) to switch use this minor mode or
not.  For instance

  (c-mode (not message-mode mail-mode) text-mode)

means \"use this mode in all modes derived from `c-mode', don't use in
modes derived from `message-mode' or `mail-mode', but do use in other
modes derived from `text-mode'\".  An element with value t means \"use\"
and nil means \"don't use\".  There's an implicit nil at the end of the
list.") (custom-autoload 'global-corfu-modes "corfu" t) (register-definition-prefixes "corfu" '("corfu-" "global-corfu-minibuffer")) (defvar corfu-echo-mode nil "Non-nil if Corfu-Echo mode is enabled.
See the `corfu-echo-mode' command
for a description of this minor mode.") (custom-autoload 'corfu-echo-mode "corfu-echo" nil) (autoload 'corfu-echo-mode "corfu-echo" "Show candidate documentation in echo area.

This is a global minor mode.  If called interactively, toggle the
`Corfu-Echo mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='corfu-echo-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "corfu-echo" '("corfu-echo-")) (defvar corfu-history-mode nil "Non-nil if Corfu-History mode is enabled.
See the `corfu-history-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `corfu-history-mode'.") (custom-autoload 'corfu-history-mode "corfu-history" nil) (autoload 'corfu-history-mode "corfu-history" "Update Corfu history and sort completions by history.

This is a global minor mode.  If called interactively, toggle the
`Corfu-History mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='corfu-history-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "corfu-history" '("corfu-history")) (defvar corfu-indexed-mode nil "Non-nil if Corfu-Indexed mode is enabled.
See the `corfu-indexed-mode' command
for a description of this minor mode.") (custom-autoload 'corfu-indexed-mode "corfu-indexed" nil) (autoload 'corfu-indexed-mode "corfu-indexed" "Prefix candidates with indices.

This is a global minor mode.  If called interactively, toggle the
`Corfu-Indexed mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='corfu-indexed-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "corfu-indexed" '("corfu-indexed-")) (autoload 'corfu-info-documentation "corfu-info" "Show documentation of current candidate.
If called with a prefix ARG, the buffer is persistent.

(fn &optional ARG)" t) (autoload 'corfu-info-location "corfu-info" "Show location of current candidate.
If called with a prefix ARG, the buffer is persistent.

(fn &optional ARG)" t) (register-definition-prefixes "corfu-info" '("corfu-info--")) (defvar corfu-popupinfo-mode nil "Non-nil if Corfu-Popupinfo mode is enabled.
See the `corfu-popupinfo-mode' command
for a description of this minor mode.") (custom-autoload 'corfu-popupinfo-mode "corfu-popupinfo" nil) (autoload 'corfu-popupinfo-mode "corfu-popupinfo" "Corfu info popup minor mode.

This is a global minor mode.  If called interactively, toggle the
`Corfu-Popupinfo mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='corfu-popupinfo-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "corfu-popupinfo" '("corfu-popupinfo-")) (autoload 'corfu-quick-jump "corfu-quick" "Jump to candidate using quick keys." t) (autoload 'corfu-quick-insert "corfu-quick" "Insert candidate using quick keys." t) (autoload 'corfu-quick-complete "corfu-quick" "Complete candidate using quick keys." t) (register-definition-prefixes "corfu-quick" '("corfu-quick")) (provide 'corfu-autoloads)) "orderless" ((orderless-kwd orderless orderless-autoloads) (autoload 'orderless-all-completions "orderless" "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.  The
matching portions of each candidate are highlighted.
This function is part of the `orderless' completion style.

(fn STRING TABLE PRED POINT)") (autoload 'orderless-try-completion "orderless" "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED.  If there is only one
match, it completes to that match.  If there are no matches, it
returns nil.  In any other case it \"completes\" STRING to
itself, without moving POINT.
This function is part of the `orderless' completion style.

(fn STRING TABLE PRED POINT)") (add-to-list 'completion-styles-alist '(orderless orderless-try-completion orderless-all-completions "Completion of multiple components, in any order.")) (autoload 'orderless-ivy-re-builder "orderless" "Convert STR into regexps for use with ivy.
This function is for integration of orderless with ivy, use it as
a value in `ivy-re-builders-alist'.

(fn STR)") (register-definition-prefixes "orderless" '("orderless-")) (autoload 'orderless-kwd-dispatch "orderless-kwd" "Match COMPONENT against the keywords in `orderless-kwd-alist'.

(fn COMPONENT INDEX TOTAL)") (register-definition-prefixes "orderless-kwd" '("orderless-kwd-")) (provide 'orderless-autoloads)) "cape" ((cape cape-char cape-autoloads cape-keyword) (autoload 'cape-history "cape" "Complete from Eshell, Comint or minibuffer history.
See also `consult-history' for a more flexible variant based on
`completing-read'.  If INTERACTIVE is nil the function acts like a Capf.

(fn &optional INTERACTIVE)" t) (autoload 'cape-file "cape" "Complete file name at point.
See the user option `cape-file-directory-must-exist'.
If INTERACTIVE is nil the function acts like a Capf.

(fn &optional INTERACTIVE)" t) (autoload 'cape-elisp-symbol "cape" "Complete Elisp symbol at point.
If INTERACTIVE is nil the function acts like a Capf.

(fn &optional INTERACTIVE)" t) (autoload 'cape-elisp-block "cape" "Complete Elisp in Org or Markdown code block.
This Capf is particularly useful for literate Emacs configurations.
If INTERACTIVE is nil the function acts like a Capf.

(fn &optional INTERACTIVE)" t) (autoload 'cape-dabbrev "cape" "Complete with Dabbrev at point.

If INTERACTIVE is nil the function acts like a Capf.  In case you
observe a performance issue with auto-completion and `cape-dabbrev'
it is strongly recommended to disable scanning in other buffers.
See the user options `cape-dabbrev-min-length' and
`cape-dabbrev-check-other-buffers'.

(fn &optional INTERACTIVE)" t) (autoload 'cape-dict "cape" "Complete word from dictionary at point.
This completion function works best if the dictionary is sorted
by frequency.  See the custom option `cape-dict-file'.  If
INTERACTIVE is nil the function acts like a Capf.

(fn &optional INTERACTIVE)" t) (autoload 'cape-abbrev "cape" "Complete abbreviation at point.
If INTERACTIVE is nil the function acts like a Capf.

(fn &optional INTERACTIVE)" t) (autoload 'cape-line "cape" "Complete current line from other lines.
The buffers returned by `cape-line-buffer-function' are scanned for lines.
If INTERACTIVE is nil the function acts like a Capf.

(fn &optional INTERACTIVE)" t) (autoload 'cape-company-to-capf "cape" "Convert Company BACKEND function to Capf.
VALID is a function taking the old and new input string.  It should
return nil if the cached candidates became invalid.  The default value
for VALID is `string-prefix-p' such that the candidates are only fetched
again if the input prefix changed.

(fn BACKEND &optional VALID)") (autoload 'cape-interactive "cape" "Complete interactively with the given CAPFS.

(fn &rest CAPFS)") (autoload 'cape-capf-interactive "cape" "Create interactive completion function from CAPF.

(fn CAPF)") (autoload 'cape-wrap-super "cape" "Call CAPFS and return merged completion result.
The CAPFS list can contain the keyword `:with' to mark the Capfs
afterwards as auxiliary One of the non-auxiliary Capfs before
`:with' must return non-nil for the super Capf to set in and
return a non-nil result.  Such behavior is useful when listing
multiple super Capfs in the `completion-at-point-functions':

  (setq completion-at-point-functions
        (list (cape-capf-super \\='eglot-completion-at-point
                               :with \\='tempel-complete)
              (cape-capf-super \\='cape-dabbrev
                               :with \\='tempel-complete)))

(fn &rest CAPFS)") (autoload 'cape-wrap-debug "cape" "Call CAPF and return a completion table which prints trace messages.
If CAPF is an anonymous lambda, pass the Capf NAME explicitly for
meaningful debugging output.

(fn CAPF &optional NAME)") (autoload 'cape-wrap-buster "cape" "Call CAPF and return a completion table with cache busting.
This function can be used as an advice around an existing Capf.
The cache is busted when the input changes.  The argument VALID
can be a function taking the old and new input string.  It should
return nil if the new input requires that the completion table is
refreshed.  The default value for VALID is `equal', such that the
completion table is refreshed on every input change.

(fn CAPF &optional VALID)") (autoload 'cape-wrap-passthrough "cape" "Call CAPF and make sure that no completion style filtering takes place.

(fn CAPF)") (autoload 'cape-wrap-properties "cape" "Call CAPF and add additional completion PROPERTIES.
Completion properties include for example :exclusive, :annotation-function and
the various :company-* extensions.  Furthermore a boolean :sort flag and a
completion :category symbol can be specified.

(fn CAPF &rest PROPERTIES)") (autoload 'cape-wrap-nonexclusive "cape" "Call CAPF and ensure that it is marked as non-exclusive.
This function can be used as an advice around an existing Capf.

(fn CAPF)") (autoload 'cape-wrap-predicate "cape" "Call CAPF and add an additional candidate PREDICATE.
The PREDICATE is passed the candidate symbol or string.

(fn CAPF PREDICATE)") (autoload 'cape-wrap-silent "cape" "Call CAPF and silence it (no messages, no errors).
This function can be used as an advice around an existing Capf.

(fn CAPF)") (autoload 'cape-wrap-case-fold "cape" "Call CAPF and return a case-insensitive completion table.
If NOFOLD is non-nil return a case sensitive table instead.  This
function can be used as an advice around an existing Capf.

(fn CAPF &optional NOFOLD)") (autoload 'cape-wrap-noninterruptible "cape" "Call CAPF and return a non-interruptible completion table.
This function can be used as an advice around an existing Capf.

(fn CAPF)") (autoload 'cape-wrap-prefix-length "cape" "Call CAPF and ensure that prefix length is greater or equal than LENGTH.
If the prefix is long enough, enforce auto completion.

(fn CAPF LENGTH)") (autoload 'cape-wrap-inside-faces "cape" "Call CAPF only if inside FACES.
This function can be used as an advice around an existing Capf.

(fn CAPF &rest FACES)") (autoload 'cape-wrap-inside-code "cape" "Call CAPF only if inside code, not inside a comment or string.
This function can be used as an advice around an existing Capf.

(fn CAPF)") (autoload 'cape-wrap-inside-comment "cape" "Call CAPF only if inside comment.
This function can be used as an advice around an existing Capf.

(fn CAPF)") (autoload 'cape-wrap-inside-string "cape" "Call CAPF only if inside string.
This function can be used as an advice around an existing Capf.

(fn CAPF)") (autoload 'cape-wrap-purify "cape" "Call CAPF and ensure that it does not illegally modify the buffer.
This function can be used as an advice around an existing
Capf.  It has been introduced mainly to fix the broken
`pcomplete-completions-at-point' function in Emacs versions < 29.

(fn CAPF)") (autoload 'cape-wrap-accept-all "cape" "Call CAPF and return a completion table which accepts every input.
This function can be used as an advice around an existing Capf.

(fn CAPF)") (autoload 'cape-capf-accept-all "cape") (autoload 'cape-capf-buster "cape") (autoload 'cape-capf-case-fold "cape") (autoload 'cape-capf-debug "cape") (autoload 'cape-capf-inside-code "cape") (autoload 'cape-capf-inside-comment "cape") (autoload 'cape-capf-inside-faces "cape") (autoload 'cape-capf-inside-string "cape") (autoload 'cape-capf-nonexclusive "cape") (autoload 'cape-capf-noninterruptible "cape") (autoload 'cape-capf-passthrough "cape") (autoload 'cape-capf-predicate "cape") (autoload 'cape-capf-prefix-length "cape") (autoload 'cape-capf-properties "cape") (autoload 'cape-capf-purify "cape") (autoload 'cape-capf-silent "cape") (autoload 'cape-capf-super "cape") (autoload 'cape-prefix-map "cape" nil t 'keymap) (register-definition-prefixes "cape" '("cape-")) (autoload 'cape-tex "cape-char" nil t) (autoload 'cape-sgml "cape-char" nil t) (autoload 'cape-rfc1345 "cape-char" nil t) (when (> emacs-major-version 28) (autoload 'cape-emoji "cape-char" nil t)) (register-definition-prefixes "cape-char" '("cape-char--")) (autoload 'cape-keyword "cape-keyword" "Complete programming language keyword at point.
See the variable `cape-keyword-list'.
If INTERACTIVE is nil the function acts like a capf.

(fn &optional INTERACTIVE)" t) (register-definition-prefixes "cape-keyword" '("cape-")) (provide 'cape-autoloads)) "which-key" ((which-key which-key-autoloads) (defvar which-key-mode nil "Non-nil if Which-Key mode is enabled.
See the `which-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-mode'.") (custom-autoload 'which-key-mode "which-key" nil) (autoload 'which-key-mode "which-key" "Toggle `which-key-mode'.

This is a global minor mode.  If called interactively, toggle the
`Which-Key mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='which-key-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'which-key-setup-side-window-right "which-key" "Set up side-window on right." t) (autoload 'which-key-setup-side-window-right-bottom "which-key" "Set up side-window on right if space allows.
Otherwise, use bottom." t) (autoload 'which-key-setup-side-window-bottom "which-key" "Set up side-window that opens on bottom." t) (autoload 'which-key-setup-minibuffer "which-key" "Set up minibuffer display.
Do not use this setup if you use the paging commands.  Instead use
`which-key-setup-side-window-bottom', which is nearly identical
but more functional." t) (autoload 'which-key-add-keymap-based-replacements "which-key" "Replace the description of KEY using REPLACEMENT in KEYMAP.
KEY should take a format suitable for use in `kbd'.  REPLACEMENT
should be a cons cell of the form (STRING . COMMAND) for each
REPLACEMENT, where STRING is the replacement string and COMMAND
is a symbol corresponding to the intended command to be
replaced.  COMMAND can be nil if the binding corresponds to a key
prefix.  An example is

(which-key-add-keymap-based-replacements global-map
  \"C-x w\" \\='(\"Save as\" . write-file)).

For backwards compatibility, REPLACEMENT can also be a string,
but the above format is preferred, and the option to use a string
for REPLACEMENT will eventually be removed.

(fn KEYMAP KEY REPLACEMENT &rest MORE)") (function-put 'which-key-add-keymap-based-replacements 'lisp-indent-function 'defun) (autoload 'which-key-add-key-based-replacements "which-key" "Replace the description of KEY-SEQUENCE with REPLACEMENT.
KEY-SEQUENCE is a string suitable for use in `kbd'.
REPLACEMENT may either be a string, as in

(which-key-add-key-based-replacements \"C-x 1\" \"maximize\")

a cons of two strings as in

(which-key-add-key-based-replacements \"C-x 8\"
                                        \\='(\"unicode\" . \"Unicode keys\"))

or a function that takes a (KEY . BINDING) cons and returns a
replacement.

In the second case, the second string is used to provide a longer
name for the keys under a prefix.

MORE allows you to specifcy additional KEY REPLACEMENT pairs.  All
replacements are added to `which-key-replacement-alist'.

(fn KEY-SEQUENCE REPLACEMENT &rest MORE)") (autoload 'which-key-add-major-mode-key-based-replacements "which-key" "Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply.

(fn MODE KEY-SEQUENCE REPLACEMENT &rest MORE)") (function-put 'which-key-add-major-mode-key-based-replacements 'lisp-indent-function 'defun) (autoload 'which-key-reload-key-sequence "which-key" "Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'.  If nil, KEY-SEQ defaults to
`which-key--current-key-list'.  Any prefix arguments that were
used are reapplied to the new key sequence.

(fn &optional KEY-SEQ)") (autoload 'which-key-show-standard-help "which-key" "Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'.

(fn &optional _)" t) (autoload 'which-key-show-next-page-no-cycle "which-key" "Show next page of keys or `which-key-show-standard-help'." t) (autoload 'which-key-show-previous-page-no-cycle "which-key" "Show previous page of keys if one exists." t) (autoload 'which-key-show-next-page-cycle "which-key" "Show the next page of keys, cycling from end to beginning.

(fn &optional _)" t) (autoload 'which-key-show-previous-page-cycle "which-key" "Show the previous page of keys, cycling from beginning to end.

(fn &optional _)" t) (autoload 'which-key-show-top-level "which-key" "Show top-level bindings.

(fn &optional _)" t) (autoload 'which-key-show-major-mode "which-key" "Show top-level bindings in the map of the current major mode.
This function will also detect evil bindings made using
`evil-define-key' in this map.  These bindings will depend on the
current evil state.

(fn &optional ALL)" t) (autoload 'which-key-show-full-major-mode "which-key" "Show all bindings in the map of the current major mode.
This function will also detect evil bindings made using
`evil-define-key' in this map.  These bindings will depend on the
current evil state." t) (autoload 'which-key-dump-bindings "which-key" "Dump bindings from PREFIX into buffer named BUFFER-NAME.
PREFIX should be a string suitable for `kbd'.

(fn PREFIX BUFFER-NAME)" t) (autoload 'which-key-undo-key "which-key" "Undo last keypress and force which-key update.

(fn &optional _)" t) (autoload 'which-key-C-h-dispatch "which-key" "Dispatch \\`C-h' commands by looking up key in `which-key-C-h-map'.
This command is always accessible (from any prefix) if
`which-key-use-C-h-commands' is non nil." t) (autoload 'which-key-show-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key.
KEYMAP is selected interactively from all available keymaps.

If NO-PAGING is non-nil, which-key will not intercept subsequent
keypresses for the paging functionality.

(fn KEYMAP &optional NO-PAGING)" t) (autoload 'which-key-show-full-keymap "which-key" "Show all bindings in KEYMAP using which-key.
KEYMAP is selected interactively from all available keymaps.

(fn KEYMAP)" t) (autoload 'which-key-show-minor-mode-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key.
KEYMAP is selected interactively by mode in
`minor-mode-map-alist'.

(fn &optional ALL)" t) (autoload 'which-key-show-full-minor-mode-keymap "which-key" "Show all bindings in KEYMAP using which-key.
KEYMAP is selected interactively by mode in
`minor-mode-map-alist'." t) (register-definition-prefixes "which-key" '("evil-state" "which-key-")) (provide 'which-key-autoloads)) "yasnippet" ((yasnippet yasnippet-autoloads) (autoload 'yas-minor-mode "yasnippet" "Toggle YASnippet mode.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{yas-minor-mode-map}

This is a minor mode.  If called interactively, toggle the `yas
minor mode' mode.  If the prefix argument is positive, enable the
mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `yas-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'yas-global-mode 'globalized-minor-mode t) (defvar yas-global-mode nil "Non-nil if Yas-Global mode is enabled.
See the `yas-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.") (custom-autoload 'yas-global-mode "yasnippet" nil) (autoload 'yas-global-mode "yasnippet" "Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Yas minor mode is enabled in all buffers where `yas-minor-mode-on' would do it.

See `yas-minor-mode' for more information on Yas minor mode.

(fn &optional ARG)" t) (autoload 'snippet-mode "yasnippet" "A mode for editing yasnippets" t nil) (register-definition-prefixes "yasnippet" '("help-snippet-def" "snippet-mode-map" "yas")) (provide 'yasnippet-autoloads)) "yasnippet-snippets" ((yasnippet-snippets-autoloads yasnippet-snippets) (autoload 'yasnippet-snippets-initialize "yasnippet-snippets" "Load the `yasnippet-snippets' snippets directory.") (eval-after-load 'yasnippet '(yasnippet-snippets-initialize)) (register-definition-prefixes "yasnippet-snippets" '("yasnippet-snippets-")) (provide 'yasnippet-snippets-autoloads)) "vertico" ((vertico-quick vertico vertico-mouse vertico-flat vertico-grid vertico-multiform vertico-unobtrusive vertico-buffer vertico-indexed vertico-repeat vertico-autoloads vertico-directory vertico-suspend vertico-reverse) (defvar vertico-mode nil "Non-nil if Vertico mode is enabled.
See the `vertico-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-mode'.") (custom-autoload 'vertico-mode "vertico" nil) (autoload 'vertico-mode "vertico" "VERTical Interactive COmpletion.

This is a global minor mode.  If called interactively, toggle the
`Vertico mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico" '("vertico-")) (defvar vertico-buffer-mode nil "Non-nil if Vertico-Buffer mode is enabled.
See the `vertico-buffer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-buffer-mode'.") (custom-autoload 'vertico-buffer-mode "vertico-buffer" nil) (autoload 'vertico-buffer-mode "vertico-buffer" "Display Vertico like a regular buffer in a large window.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Buffer mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-buffer-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-buffer" '("vertico-buffer-")) (autoload 'vertico-directory-enter "vertico-directory" "Enter directory or exit completion with current candidate.
Exit with current input if prefix ARG is given.

(fn &optional ARG)" t) (autoload 'vertico-directory-up "vertico-directory" "Delete N names before point.

(fn &optional N)" t) (autoload 'vertico-directory-delete-char "vertico-directory" "Delete N directories or chars before point.

(fn &optional N)" t) (autoload 'vertico-directory-delete-word "vertico-directory" "Delete N directories or words before point.

(fn &optional N)" t) (autoload 'vertico-directory-tidy "vertico-directory" "Tidy shadowed file name, see `rfn-eshadow-overlay'.") (defvar vertico-flat-mode nil "Non-nil if Vertico-Flat mode is enabled.
See the `vertico-flat-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-flat-mode'.") (custom-autoload 'vertico-flat-mode "vertico-flat" nil) (autoload 'vertico-flat-mode "vertico-flat" "Flat, horizontal display for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Flat mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-flat-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-flat" '("vertico-flat-")) (defvar vertico-grid-mode nil "Non-nil if Vertico-Grid mode is enabled.
See the `vertico-grid-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-grid-mode'.") (custom-autoload 'vertico-grid-mode "vertico-grid" nil) (autoload 'vertico-grid-mode "vertico-grid" "Grid display for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Grid mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-grid-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-grid" '("vertico-grid-")) (defvar vertico-indexed-mode nil "Non-nil if Vertico-Indexed mode is enabled.
See the `vertico-indexed-mode' command
for a description of this minor mode.") (custom-autoload 'vertico-indexed-mode "vertico-indexed" nil) (autoload 'vertico-indexed-mode "vertico-indexed" "Prefix candidates with indices.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Indexed mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-indexed-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-indexed" '("vertico-indexed-")) (defvar vertico-mouse-mode nil "Non-nil if Vertico-Mouse mode is enabled.
See the `vertico-mouse-mode' command
for a description of this minor mode.") (custom-autoload 'vertico-mouse-mode "vertico-mouse" nil) (autoload 'vertico-mouse-mode "vertico-mouse" "Mouse support for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Mouse mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-mouse-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-mouse" '("vertico-mouse-")) (defvar vertico-multiform-mode nil "Non-nil if Vertico-Multiform mode is enabled.
See the `vertico-multiform-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-multiform-mode'.") (custom-autoload 'vertico-multiform-mode "vertico-multiform" nil) (autoload 'vertico-multiform-mode "vertico-multiform" "Configure Vertico in various forms per command.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Multiform mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-multiform-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-multiform" '("vertico-multiform-")) (autoload 'vertico-quick-jump "vertico-quick" "Jump to candidate using quick keys." t) (autoload 'vertico-quick-exit "vertico-quick" "Exit with candidate using quick keys." t) (autoload 'vertico-quick-insert "vertico-quick" "Insert candidate using quick keys." t) (register-definition-prefixes "vertico-quick" '("vertico-quick")) (autoload 'vertico-repeat-save "vertico-repeat" "Save Vertico session for `vertico-repeat'.
This function must be registered as `minibuffer-setup-hook'.") (autoload 'vertico-repeat-next "vertico-repeat" "Repeat Nth next Vertico completion session.
This command must be called from an existing Vertico session
after `vertico-repeat-previous'.

(fn N)" t) (autoload 'vertico-repeat-previous "vertico-repeat" "Repeat Nth previous Vertico completion session.
If called from an existing Vertico session, restore the input and
selected candidate for the current command.

(fn N)" t) (autoload 'vertico-repeat-select "vertico-repeat" "Select a Vertico session from the session history and repeat it.
If called from an existing Vertico session, you can select among
previous sessions for the current command." t) (autoload 'vertico-repeat "vertico-repeat" "Repeat last Vertico session.
If prefix ARG is non-nil, offer completion menu to select from session history.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-repeat" '("vertico-repeat-")) (defvar vertico-reverse-mode nil "Non-nil if Vertico-Reverse mode is enabled.
See the `vertico-reverse-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-reverse-mode'.") (custom-autoload 'vertico-reverse-mode "vertico-reverse" nil) (autoload 'vertico-reverse-mode "vertico-reverse" "Reverse the Vertico display.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Reverse mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-reverse-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-reverse" '("vertico-reverse-map")) (autoload 'vertico-suspend "vertico-suspend" "Suspend the current completion session.
If the command is invoked from within the Vertico minibuffer, the
current session is suspended.  If the command is invoked from
outside the minibuffer, the active minibuffer is either selected
or the latest completion session is restored." t) (register-definition-prefixes "vertico-suspend" '("vertico-suspend--")) (defvar vertico-unobtrusive-mode nil "Non-nil if Vertico-Unobtrusive mode is enabled.
See the `vertico-unobtrusive-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-unobtrusive-mode'.") (custom-autoload 'vertico-unobtrusive-mode "vertico-unobtrusive" nil) (autoload 'vertico-unobtrusive-mode "vertico-unobtrusive" "Unobtrusive display for Vertico.

This is a global minor mode.  If called interactively, toggle the
`Vertico-Unobtrusive mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='vertico-unobtrusive-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "vertico-unobtrusive" '("vertico-unobtrusive--restore")) (provide 'vertico-autoloads)) "org-contrib" ((ox-bibtex org-depend ol-mew org-checklist ol-wl ol-bookmark ob-hledger org-screen ox-extra ol-git-link ob-mathomatic org-panel ob-oz ob-io ob-fomus ob-vala org-wikinodes org-license org-mairix ob-csharp org-contribdir org-learn ob-stata ox-freemind ob-abc org-collector ol-elisp-symbol org-bibtex-extras org-registry org-expiry org-annotate-file ob-ebnf orgtbl-sqlinsert ob-eukleides org-eldoc org-secretary ob-shen ox-groff org-invoice ob-vbnet org-sudoku org-effectiveness ol-vm ob-J org-toc org-screenshot org-contrib ox-confluence ox-s5 org-interactive-query ob-picolisp org-choose org-contrib-autoloads org-mac-iCal ox-deck ob-ledger ox-taskjuggler ob-tcl ob-mscgen) (register-definition-prefixes "ob-J" '("obj-" "org-babel-")) (register-definition-prefixes "ob-abc" '("org-babel-")) (register-definition-prefixes "ob-csharp" '("org-babel-")) (register-definition-prefixes "ob-ebnf" '("org-babel-")) (register-definition-prefixes "ob-eukleides" '("org-")) (register-definition-prefixes "ob-fomus" '("org-babel-")) (register-definition-prefixes "ob-hledger" '("org-babel-")) (register-definition-prefixes "ob-io" '("org-babel-")) (register-definition-prefixes "ob-ledger" '("org-babel-")) (register-definition-prefixes "ob-mathomatic" '("org-babel-")) (register-definition-prefixes "ob-mscgen" '("org-babel-")) (register-definition-prefixes "ob-oz" '("org-babel-" "oz-send-string-expression")) (register-definition-prefixes "ob-picolisp" '("org-babel-")) (register-definition-prefixes "ob-shen" '("org-babel-")) (register-definition-prefixes "ob-stata" '("org-babel-")) (register-definition-prefixes "ob-tcl" '("org-babel-")) (register-definition-prefixes "ob-vala" '("org-babel-")) (register-definition-prefixes "ob-vbnet" '("org-babel-")) (register-definition-prefixes "ol-bookmark" '("org-bookmark-")) (register-definition-prefixes "ol-elisp-symbol" '("org-elisp-symbol-")) (register-definition-prefixes "ol-git-link" '("org-git")) (register-definition-prefixes "ol-mew" '("org-mew-")) (register-definition-prefixes "ol-vm" '("org-vm-")) (register-definition-prefixes "ol-wl" '("org-wl-")) (autoload 'org-annotate-file "org-annotate-file" "Visit `org-annotate-file-storage-file` and add a new annotation section.
The annotation is opened at the new section which will be referencing
the point in the current file." t) (autoload 'org-annotate-file-show-section "org-annotate-file" "Add or show annotation entry in STORAGE-FILE and return the buffer.
The annotation will link to ANNOTATED-BUFFER if specified,
  otherwise the current buffer is used.

(fn STORAGE-FILE &optional ANNOTATED-BUFFER)") (register-definition-prefixes "org-annotate-file" '("org-annotate-file-")) (register-definition-prefixes "org-bibtex-extras" '("obe-")) (register-definition-prefixes "org-checklist" '("org-")) (register-definition-prefixes "org-choose" '("org-choose-")) (register-definition-prefixes "org-collector" '("and-rest" "org-")) (register-definition-prefixes "org-depend" '("org-depend-")) (register-definition-prefixes "org-effectiveness" '("org-effectiveness-")) (autoload 'org-eldoc-load "org-eldoc" "Set up org-eldoc documentation function." t) (register-definition-prefixes "org-eldoc" '("org-eldoc-")) (register-definition-prefixes "org-expiry" '("org-")) (register-definition-prefixes "org-interactive-query" '("org-agenda-query-")) (register-definition-prefixes "org-invoice" '("org-")) (register-definition-prefixes "org-learn" '("calculate-new-optimal-factor" "determine-next-interval" "get-optimal-factor" "initial-" "inter-repetition-interval" "modify-" "org-" "set-optimal-factor")) (register-definition-prefixes "org-license" '("org-license-")) (register-definition-prefixes "org-mac-iCal" '("omi-" "org-mac-iCal")) (register-definition-prefixes "org-mairix" '("org-")) (register-definition-prefixes "org-panel" '("orgpan-")) (autoload 'org-registry-show "org-registry" "Show Org files where there are links pointing to the current
buffer.

(fn &optional VISIT)" t) (autoload 'org-registry-visit "org-registry" "If an Org file contains a link to the current location, visit
this file." t) (autoload 'org-registry-initialize "org-registry" "Initialize `org-registry-alist'.
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-registry-file' and make it the new value for
`org-registry-alist'.

(fn &optional FROM-SCRATCH)" t) (autoload 'org-registry-insinuate "org-registry" "Call `org-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit." t) (autoload 'org-registry-update "org-registry" "Update the registry for the current Org file." t) (register-definition-prefixes "org-registry" '("org-registry-")) (register-definition-prefixes "org-screen" '("org-screen")) (autoload 'org-screenshot-take "org-screenshot" "Take a screenshot and insert link to it at point, if image
display is already on (see \\[org-toggle-inline-images])
screenshot will be displayed as an image

Screen area for the screenshot is selected with the mouse, left
click on a window screenshots that window, while left click and
drag selects a region. Pressing any key cancels the screen shot

With `C-u' universal argument waits one second after target is
selected before taking the screenshot. With double `C-u' wait two
seconds.

With triple `C-u' wait 3 seconds, and also rings the bell when
screenshot is done, any more `C-u' after that increases delay by
2 seconds

(fn &optional DELAY)" t) (autoload 'org-screenshot-rotate-prev "org-screenshot" "Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

(fn DIR)" t) (autoload 'org-screenshot-rotate-next "org-screenshot" "Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

(fn DIR)" t) (autoload 'org-screenshot-show-unused "org-screenshot" "Open A Dired buffer with unused screenshots marked" t) (register-definition-prefixes "org-screenshot" '("org-screenshot-")) (register-definition-prefixes "org-secretary" '("join" "org-sec-")) (register-definition-prefixes "org-sudoku" '("org-sudoku-")) (autoload 'org-toc-show "org-toc" "Show the table of contents of the current Org-mode buffer.

(fn &optional DEPTH POSITION)" t) (register-definition-prefixes "org-toc" '("org-")) (register-definition-prefixes "org-wikinodes" '("org-wikinodes-")) (register-definition-prefixes "orgtbl-sqlinsert" '("orgtbl-")) (register-definition-prefixes "ox-bibtex" '("org-")) (register-definition-prefixes "ox-confluence" '("org-confluence-")) (register-definition-prefixes "ox-deck" '("org-deck-")) (register-definition-prefixes "ox-extra" '("org-" "ox-extras")) (autoload 'org-freemind-export-to-freemind "ox-freemind" "Export current buffer to a Freemind Mindmap file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t) (register-definition-prefixes "ox-freemind" '("org-freemind-")) (register-definition-prefixes "ox-groff" '("org-groff-")) (register-definition-prefixes "ox-s5" '("org-s5-")) (autoload 'org-taskjuggler-export "ox-taskjuggler" "Export current buffer to a TaskJuggler file.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t) (autoload 'org-taskjuggler-export-and-process "ox-taskjuggler" "Export current buffer to a TaskJuggler file and process it.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return a list of reports.

(fn &optional SUBTREEP VISIBLE-ONLY)" t) (autoload 'org-taskjuggler-export-process-and-open "ox-taskjuggler" "Export current buffer to a TaskJuggler file, process and open it.

Export and process the file using
`org-taskjuggler-export-and-process' and open the generated
reports with a browser.

If you are targeting TaskJuggler 2.4 (see
`org-taskjuggler-target-version') the processing and display of
the reports is done using the TaskJuggler GUI.

(fn &optional SUBTREEP VISIBLE-ONLY)" t) (register-definition-prefixes "ox-taskjuggler" '("org-taskjuggler-")) (provide 'org-contrib-autoloads)) "async" ((async-autoloads async-bytecomp dired-async async-package smtpmail-async async) (autoload 'async-start-process "async" "Start the executable PROGRAM asynchronously named NAME.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory.

(fn NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS)") (autoload 'async-start "async" "Execute START-FUNC (often a lambda) in a subordinate Emacs process.
When done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message \"This is a test\")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message \"Async process done, result should be 222: %s\"
                  result)))

If you call `async-send' from a child process, the message will
be also passed to the FINISH-FUNC.  You can test RESULT to see if
it is a message by using `async-message-p'.  If nil, it means
this is the final result.  Example of the FINISH-FUNC:

    (lambda (result)
      (if (async-message-p result)
          (message \"Received a message from child process: %s\" result)
        (message \"Async process done, result: %s\" result)))

If FINISH-FUNC is nil or missing, a future is returned that can
be inspected using `async-get', blocking until the value is
ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message \"This is a test\")
                     (sleep-for 3)
                     222))))

        (message \"I'm going to do some work here\") ;; ....

        (message \"Waiting on async process, result should be 222: %s\"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any
return value from the child process, pass the `ignore' symbol as
the second argument (if you don't, and never call `async-get', it
will leave *emacs* process buffers hanging around):

    (async-start
     (lambda ()
       (delete-file \"a remote file on a slow link\" nil))
     \\='ignore)

Special case:
If the output of START-FUNC is a string with properties
e.g. (buffer-string) RESULT will be transformed in a list where the
car is the string itself (without props) and the cdr the rest of
properties, this allows using in FINISH-FUNC the string without
properties and then apply the properties in cdr to this string (if
needed).
Properties handling special objects like markers are returned as
list to allow restoring them later.
See <https://github.com/jwiegley/emacs-async/issues/145> for more infos.

Note: Even when FINISH-FUNC is present, a future is still
returned except that it yields no value (since the value is
passed to FINISH-FUNC).  Call `async-get' on such a future always
returns nil.  It can still be useful, however, as an argument to
`async-ready' or `async-wait'.

(fn START-FUNC &optional FINISH-FUNC)") (register-definition-prefixes "async" '("async-")) (autoload 'async-byte-recompile-directory "async-bytecomp" "Compile all *.el files in DIRECTORY asynchronously.
All *.elc files are systematically deleted before proceeding.

(fn DIRECTORY &optional QUIET)") (defvar async-bytecomp-package-mode nil "Non-nil if Async-Bytecomp-Package mode is enabled.
See the `async-bytecomp-package-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `async-bytecomp-package-mode'.") (custom-autoload 'async-bytecomp-package-mode "async-bytecomp" nil) (autoload 'async-bytecomp-package-mode "async-bytecomp" "Byte compile asynchronously packages installed with package.el.

Async compilation of packages can be controlled by
`async-bytecomp-allowed-packages'.

This is a global minor mode.  If called interactively, toggle the
`Async-Bytecomp-Package mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='async-bytecomp-package-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'async-byte-compile-file "async-bytecomp" "Byte compile Lisp code FILE asynchronously.

Same as `byte-compile-file' but asynchronous.

(fn FILE)" t) (register-definition-prefixes "async-bytecomp" '("async-")) (register-definition-prefixes "async-package" '("async-p")) (defvar dired-async-mode nil "Non-nil if Dired-Async mode is enabled.
See the `dired-async-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dired-async-mode'.") (custom-autoload 'dired-async-mode "dired-async" nil) (autoload 'dired-async-mode "dired-async" "Do dired actions asynchronously.

This is a global minor mode.  If called interactively, toggle the
`Dired-Async mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='dired-async-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'dired-async-do-copy "dired-async" "Run ‘dired-do-copy’ asynchronously.

(fn &optional ARG)" t) (autoload 'dired-async-do-symlink "dired-async" "Run ‘dired-do-symlink’ asynchronously.

(fn &optional ARG)" t) (autoload 'dired-async-do-hardlink "dired-async" "Run ‘dired-do-hardlink’ asynchronously.

(fn &optional ARG)" t) (autoload 'dired-async-do-rename "dired-async" "Run ‘dired-do-rename’ asynchronously.

(fn &optional ARG)" t) (register-definition-prefixes "dired-async" '("dired-async-")) (register-definition-prefixes "smtpmail-async" '("async-smtpmail-")) (provide 'async-autoloads)) "ob-async" ((ob-async ob-async-autoloads) (defalias 'org-babel-execute-src-block:async 'ob-async-org-babel-execute-src-block) (autoload 'ob-async-org-babel-execute-src-block "ob-async" "Like org-babel-execute-src-block, but run asynchronously.

Original docstring for org-babel-execute-src-block:

Execute the current source code block.  Insert the results of
execution into the buffer.  Source code execution and the
collection and formatting of results can be controlled through a
variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block.

(fn &optional ORIG-FUN ARG INFO PARAMS)" t) (register-definition-prefixes "ob-async" '("ob-async-")) (provide 'ob-async-autoloads)) "ts" ((ts ts-autoloads) (register-definition-prefixes "ts" '("ts-" "ts<" "ts=" "ts>")) (provide 'ts-autoloads)) "org-super-agenda" ((org-super-agenda-autoloads org-super-agenda) (defvar org-super-agenda-mode nil "Non-nil if Org-Super-Agenda mode is enabled.
See the `org-super-agenda-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-super-agenda-mode'.") (custom-autoload 'org-super-agenda-mode "org-super-agenda" nil) (autoload 'org-super-agenda-mode "org-super-agenda" "Group items in Org agenda views according to `org-super-agenda-groups'.
With prefix argument ARG, turn on if positive, otherwise off.

(fn &optional ARG)" t) (register-definition-prefixes "org-super-agenda" '("org-super-agenda-")) (provide 'org-super-agenda-autoloads)) "ox-gfm" ((ox-gfm-autoloads ox-gfm) (autoload 'org-gfm-export-as-markdown "ox-gfm" "Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org GFM Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t) (autoload 'org-gfm-convert-region-to-md "ox-gfm" "Convert the region to Github Flavored Markdown.
This can be used in any buffer, this function assume that the
current region has org-mode syntax.  For example, you can write
an itemized list in org-mode syntax in a Markdown buffer and use
this command to convert it." t) (autoload 'org-gfm-export-to-markdown "ox-gfm" "Export current buffer to a Github Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t) (autoload 'org-gfm-publish-to-gfm "ox-gfm" "Publish an org file to Markdown.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name.

(fn PLIST FILENAME PUB-DIR)") (register-definition-prefixes "ox-gfm" '("gfm-table-" "org-gfm-" "width-cookies")) (provide 'ox-gfm-autoloads)) "transient" ((transient-autoloads transient) (autoload 'transient-insert-suffix "transient" "Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
Remove a conflicting binding unless optional KEEP-OTHER is
  non-nil.
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX &optional KEEP-OTHER)") (function-put 'transient-insert-suffix 'lisp-indent-function 'defun) (autoload 'transient-append-suffix "transient" "Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
Remove a conflicting binding unless optional KEEP-OTHER is
  non-nil.
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX &optional KEEP-OTHER)") (function-put 'transient-append-suffix 'lisp-indent-function 'defun) (autoload 'transient-replace-suffix "transient" "Replace the suffix at LOC in PREFIX with SUFFIX.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)") (function-put 'transient-replace-suffix 'lisp-indent-function 'defun) (autoload 'transient-remove-suffix "transient" "Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC)") (function-put 'transient-remove-suffix 'lisp-indent-function 'defun) (register-definition-prefixes "transient" '("find-function-advised-original" "transient")) (provide 'transient-autoloads)) "gptel" ((gptel-org gptel-gemini gptel-openai gptel-curl gptel-ollama gptel gptel-privategpt gptel-context gptel-kagi gptel-autoloads gptel-rewrite gptel-anthropic gptel-transient) (autoload 'gptel-mode "gptel" "Minor mode for interacting with LLMs.

This is a minor mode.  If called interactively, toggle the `GPTel
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `gptel-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'gptel-send "gptel" "Submit this prompt to the current LLM backend.

By default, the contents of the buffer up to the cursor position
are sent.  If the region is active, its contents are sent
instead.

The response from the LLM is inserted below the cursor position
at the time of sending.  To change this behavior or model
parameters, use prefix arg ARG activate a transient menu with
more options instead.

This command is asynchronous, you can continue to use Emacs while
waiting for the response.

(fn &optional ARG)" t) (autoload 'gptel "gptel" "Switch to or start a chat session with NAME.

Ask for API-KEY if `gptel-api-key' is unset.

If region is active, use it as the INITIAL prompt.  Returns the
buffer created or switched to.

INTERACTIVEP is t when gptel is called interactively.

(fn NAME &optional _ INITIAL INTERACTIVEP)" t) (register-definition-prefixes "gptel" '("gptel-")) (autoload 'gptel-make-anthropic "gptel-anthropic" "Register an Anthropic API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.anthropic.com\" by default.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/messages\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY is a variable whose value is the API key, or function that
returns the key.

(fn NAME &key CURL-ARGS STREAM KEY (HEADER (lambda nil (when-let (key (gptel--get-api-key)) \\=`((\"x-api-key\" \\=\\, key) (\"anthropic-version\" . \"2023-06-01\"))))) (MODELS \\='(\"claude-3-5-sonnet-20240620\" \"claude-3-sonnet-20240229\" \"claude-3-haiku-20240307\" \"claude-3-opus-20240229\")) (HOST \"api.anthropic.com\") (PROTOCOL \"https\") (ENDPOINT \"/v1/messages\"))") (function-put 'gptel-make-anthropic 'lisp-indent-function 1) (autoload 'gptel-add "gptel-context" "Add/remove regions or buffers from gptel's context." t) (autoload 'gptel-add-file "gptel-context" "Add files to gptel's context." t) (autoload 'gptel-context--wrap "gptel-context" "

(fn MESSAGE)") (autoload 'gptel-context--collect "gptel-context" "Get the list of all active context overlays.") (register-definition-prefixes "gptel-context" '("gptel-context-")) (autoload 'gptel-curl-get-response "gptel-curl" "Retrieve response to prompt in INFO.

INFO is a plist with the following keys:
- :data (the data being sent)
- :buffer (the gptel buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point.

(fn INFO &optional CALLBACK)") (register-definition-prefixes "gptel-curl" '("gptel-")) (autoload 'gptel-make-gemini "gptel-gemini" "Register a Gemini backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, defaults to
\"generativelanguage.googleapis.com\".

MODELS is a list of available model names.

STREAM is a boolean to enable streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, \"https\" by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1beta/models\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

(fn NAME &key CURL-ARGS HEADER KEY (STREAM nil) (HOST \"generativelanguage.googleapis.com\") (PROTOCOL \"https\") (MODELS \\='(\"gemini-pro\" \"gemini-1.5-pro-latest\")) (ENDPOINT \"/v1beta/models\"))") (function-put 'gptel-make-gemini 'lisp-indent-function 1) (autoload 'gptel-make-kagi "gptel-kagi" "Register a Kagi FastGPT backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is the Kagi host (with port), defaults to \"kagi.com\".

MODELS is a list of available Kagi models: only fastgpt is supported.

STREAM is a boolean to toggle streaming responses, defaults to
false.  Kagi does not support a streaming API yet.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v0/fastgpt\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

Example:
-------

(gptel-make-kagi \"Kagi\" :key my-kagi-key)

(fn NAME &key CURL-ARGS STREAM KEY (HOST \"kagi.com\") (HEADER (lambda nil \\=`((\"Authorization\" \\=\\, (concat \"Bot \" (gptel--get-api-key)))))) (MODELS \\='(\"fastgpt\" \"summarize:cecil\" \"summarize:agnes\" \"summarize:daphne\" \"summarize:muriel\")) (PROTOCOL \"https\") (ENDPOINT \"/api/v0/\"))") (function-put 'gptel-make-kagi 'lisp-indent-function 1) (autoload 'gptel-make-ollama "gptel-ollama" "Register an Ollama backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is where Ollama runs (with port), defaults to localhost:11434

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, http by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/generate\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.  This is typically not required
for local models like Ollama.

Example:
-------

(gptel-make-ollama
  \"Ollama\"
  :host \"localhost:11434\"
  :models \\='(\"mistral:latest\")
  :stream t)

(fn NAME &key CURL-ARGS HEADER KEY MODELS STREAM (HOST \"localhost:11434\") (PROTOCOL \"http\") (ENDPOINT \"/api/chat\"))") (function-put 'gptel-make-ollama 'lisp-indent-function 1) (register-definition-prefixes "gptel-ollama" '("gptel--ollama-token-count")) (autoload 'gptel-make-openai "gptel-openai" "Register an OpenAI API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, typically \"api.openai.com\".

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/chat/completions\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

(fn NAME &key CURL-ARGS MODELS STREAM KEY (HEADER (lambda nil (when-let (key (gptel--get-api-key)) \\=`((\"Authorization\" \\=\\, (concat \"Bearer \" key)))))) (HOST \"api.openai.com\") (PROTOCOL \"https\") (ENDPOINT \"/v1/chat/completions\"))") (function-put 'gptel-make-openai 'lisp-indent-function 1) (autoload 'gptel-make-azure "gptel-openai" "Register an Azure backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is the API host.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT is the API endpoint for completions.

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

Example:
-------

(gptel-make-azure
 \"Azure-1\"
 :protocol \"https\"
 :host \"RESOURCE_NAME.openai.azure.com\"
 :endpoint
 \"/openai/deployments/DEPLOYMENT_NAME/completions?api-version=2023-05-15\"
 :stream t
 :models \\='(\"gpt-3.5-turbo\" \"gpt-4\"))

(fn NAME &key CURL-ARGS HOST (PROTOCOL \"https\") (HEADER (lambda nil \\=`((\"api-key\" \\=\\, (gptel--get-api-key))))) (KEY \\='gptel-api-key) MODELS STREAM ENDPOINT)") (function-put 'gptel-make-azure 'lisp-indent-function 1) (defalias 'gptel-make-gpt4all 'gptel-make-openai "Register a GPT4All backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is where GPT4All runs (with port), typically localhost:8491

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v1/completions\"

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key. This is typically not required for
local models like GPT4All.

Example:
-------

(gptel-make-gpt4all
 \"GPT4All\"
 :protocol \"http\"
 :host \"localhost:4891\"
 :models \\='(\"mistral-7b-openorca.Q4_0.gguf\"))") (register-definition-prefixes "gptel-openai" '("gptel--")) (register-definition-prefixes "gptel-org" '("gptel-")) (autoload 'gptel-make-privategpt "gptel-privategpt" "Register an Privategpt API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.privategpt.com\" by default.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/messages\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY is a variable whose value is the API key, or function that
returns the key.

CONTEXT and SOURCES: if true (the default), use available context
and provide sources used by the model to generate the response.

(fn NAME &key CURL-ARGS STREAM KEY (HEADER (lambda nil (when-let (key (gptel--get-api-key)) \\=`((\"Authorization\" \\=\\, (concat \"Bearer \" key)))))) (HOST \"localhost:8001\") (PROTOCOL \"http\") (MODELS \\='(\"private-gpt\")) (ENDPOINT \"/v1/chat/completions\") (CONTEXT t) (SOURCES t))") (function-put 'gptel-make-privategpt 'lisp-indent-function 1) (register-definition-prefixes "gptel-privategpt" '("gptel--privategpt-parse-sources")) (autoload 'gptel-rewrite-menu "gptel-rewrite" nil t) (register-definition-prefixes "gptel-rewrite" '("gptel-")) (autoload 'gptel-menu "gptel-transient" nil t) (autoload 'gptel-system-prompt "gptel-transient" nil t) (register-definition-prefixes "gptel-transient" '("gptel-")) (provide 'gptel-autoloads)) "plz" ((plz-autoloads plz-pkg plz) (register-definition-prefixes "plz" '("plz-")) (provide 'plz-autoloads)) "llm" ((llm llm-ollama llm-integration-test llm-gemini llm-claude llm-llamacpp llm-test llm-provider-utils-test llm-autoloads llm-gpt4all plz-event-source llm-provider-utils llm-openai llm-pkg llm-prompt-test plz-media-type llm-tester llm-fake llm-vertex llm-prompt llm-request-plz) (register-definition-prefixes "llm" '("llm-")) (register-definition-prefixes "llm-claude" '("llm-claude--tool-call")) (register-definition-prefixes "llm-gemini" '("llm-gemini--chat-url")) (register-definition-prefixes "llm-integration-test" '("llm-")) (register-definition-prefixes "llm-llamacpp" '("llm-llamacpp-")) (register-definition-prefixes "llm-ollama" '("llm-ollama-")) (register-definition-prefixes "llm-openai" '("llm-openai-")) (register-definition-prefixes "llm-prompt" '("llm-")) (register-definition-prefixes "llm-prompt-test" '("approx-equal")) (register-definition-prefixes "llm-provider-utils" '("llm-provider-utils-")) (register-definition-prefixes "llm-request-plz" '("llm-request-")) (register-definition-prefixes "llm-tester" '("llm-tester-")) (register-definition-prefixes "llm-vertex" '("llm-vertex-")) (register-definition-prefixes "plz-event-source" '("plz-event-source")) (register-definition-prefixes "plz-media-type" '("plz-media-type")) (provide 'llm-autoloads)) "spinner" ((spinner spinner-autoloads spinner-pkg) (autoload 'spinner-create "spinner" "Create a spinner of the given TYPE.
The possible TYPEs are described in `spinner--type-to-frames'.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If BUFFER-LOCAL is non-nil, the spinner will be automatically
deactivated if the buffer is killed.  If BUFFER-LOCAL is a
buffer, use that instead of current buffer.

When started, in order to function properly, the spinner runs a
timer which periodically calls `force-mode-line-update' in the
current buffer.  If BUFFER-LOCAL was set at creation time, then
`force-mode-line-update' is called in that buffer instead.  When
the spinner is stopped, the timer is deactivated.

DELAY, if given, is the number of seconds to wait after starting
the spinner before actually displaying it. It is safe to cancel
the spinner before this time, in which case it won't display at
all.

(fn &optional TYPE BUFFER-LOCAL FPS DELAY)") (autoload 'spinner-start "spinner" "Start a mode-line spinner of given TYPE-OR-OBJECT.
If TYPE-OR-OBJECT is an object created with `make-spinner',
simply activate it.  This method is designed for minor modes, so
they can use the spinner as part of their lighter by doing:
    \\='(:eval (spinner-print THE-SPINNER))
To stop this spinner, call `spinner-stop' on it.

If TYPE-OR-OBJECT is anything else, a buffer-local spinner is
created with this type, and it is displayed in the
`mode-line-process' of the buffer it was created it.  Both
TYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).
To stop this spinner, call `spinner-stop' in the same buffer.

Either way, the return value is a function which can be called
anywhere to stop this spinner.  You can also call `spinner-stop'
in the same buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

DELAY, if given, is the number of seconds to wait until actually
displaying the spinner. It is safe to cancel the spinner before
this time, in which case it won't display at all.

(fn &optional TYPE-OR-OBJECT FPS DELAY)") (register-definition-prefixes "spinner" '("spinner-")) (provide 'spinner-autoloads)) "ellama" ((ellama ellama-autoloads) (autoload 'ellama-load-session "ellama" "Load ellama session from file." t) (autoload 'ellama-session-remove "ellama" "Remove ellama session." t) (autoload 'ellama-session-switch "ellama" "Change current active session." t) (autoload 'ellama-session-rename "ellama" "Rename current ellama session." t) (autoload 'ellama-context-add-file "ellama" "Add file to context." t) (autoload 'ellama-context-add-file-quote "ellama" "Add file quote to context interactively." t) (autoload 'ellama-context-add-buffer "ellama" "Add BUF to context.

(fn BUF)" t) (autoload 'ellama-context-add-selection "ellama" "Add file to context." t) (autoload 'ellama-context-add-info-node "ellama" "Add info NODE to context.

(fn NODE)" t) (autoload 'ellama-context-add-info-node-quote "ellama" "Add info node quote to context interactively." t) (autoload 'ellama-context-add-webpage-quote-eww "ellama" "Add webpage quote to context interactively from `eww'." t) (autoload 'ellama-solve-reasoning-problem "ellama" "Solve reasoning PROBLEM with absctraction of thought.
Problem will be solved with the chain of questions to LLM.

(fn PROBLEM)" t) (autoload 'ellama-solve-domain-specific-problem "ellama" "Solve domain-specific PROBLEM with `ellama-chain'.

(fn PROBLEM)" t) (autoload 'ellama-chat "ellama" "Send PROMPT to ellama chat with conversation history.

If CREATE-SESSION set, creates new session even if there is an active session.
ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation.

:session SESSION -- SESSION is a ellama conversation session.

:session-id ID -- ID is a ellama session unique identifier.

:on-done ON-DONE -- ON-DONE a function that's called with
the full response text when the request completes (with BUFFER current).

(fn PROMPT &optional CREATE-SESSION &rest ARGS)" t) (autoload 'ellama-ask-about "ellama" "Ask ellama about selected region or current buffer." t) (autoload 'ellama-ask-selection "ellama" "Send selected region or current buffer to ellama chat." t) (autoload 'ellama-complete "ellama" "Complete text in current buffer." t) (autoload 'ellama-generate-commit-message "ellama" "Generate commit message based on diff." t) (autoload 'ellama-ask-line "ellama" "Send current line to ellama chat." t) (autoload 'ellama-translate "ellama" "Ask ellama to translate selected region or word at point." t) (autoload 'ellama-translate-buffer "ellama" "Ask ellama to translate current buffer." t) (autoload 'ellama-define-word "ellama" "Find definition of current word." t) (autoload 'ellama-summarize "ellama" "Summarize selected region or current buffer." t) (autoload 'ellama-summarize-killring "ellama" "Summarize text from the kill ring." t) (autoload 'ellama-code-review "ellama" "Review code in selected region or current buffer." t) (autoload 'ellama-change "ellama" "Change selected text or text in current buffer according to provided CHANGE.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template.

(fn CHANGE &optional EDIT-TEMPLATE)" t) (autoload 'ellama-improve-grammar "ellama" "Enhance the grammar and spelling in the currently selected region or buffer.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template.

(fn &optional EDIT-TEMPLATE)" t) (autoload 'ellama-improve-wording "ellama" "Enhance the wording in the currently selected region or buffer.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template.

(fn &optional EDIT-TEMPLATE)" t) (autoload 'ellama-improve-conciseness "ellama" "Make the text of the currently selected region or buffer concise and simple.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template.

(fn &optional EDIT-TEMPLATE)" t) (autoload 'ellama-code-edit "ellama" "Change selected code or code in current buffer according to provided CHANGE.

(fn CHANGE)" t) (autoload 'ellama-code-improve "ellama" "Change selected code or code in current buffer according to provided CHANGE." t) (autoload 'ellama-code-complete "ellama" "Complete selected code or code in current buffer." t) (autoload 'ellama-code-add "ellama" "Add new code according to DESCRIPTION.
Code will be generated with provided context from selected region or current
buffer.

(fn DESCRIPTION)" t) (autoload 'ellama-make-format "ellama" "Render selected text or text in current buffer as NEEDED-FORMAT.

(fn NEEDED-FORMAT)" t) (autoload 'ellama-make-list "ellama" "Create markdown list from active region or current buffer." t) (autoload 'ellama-make-table "ellama" "Create markdown table from active region or current buffer." t) (autoload 'ellama-provider-select "ellama" "Select ellama provider." t) (autoload 'ellama-chat-translation-enable "ellama" "Enable chat translation." t) (autoload 'ellama-chat-translation-disable "ellama" "Enable chat translation." t) (register-definition-prefixes "ellama" '("ellama-")) (provide 'ellama-autoloads)) "go-translate" ((gt-engine-google-rpc gt-faces go-translate gt-engine-echo gt-httpx gt-engine-deepl gt-text-utility gt-engine-bing go-translate-autoloads gt-core gt-engine-stardict gt-engine-chatgpt gt-extension gt-engine-libre gt-engine-google gt-engine-youdao) (autoload 'gt-do-translate "go-translate" "Translate using `gt-default-translator'.

Define your default translator like this:

  (setq gt-default-translator
    (gt-translator :engines (gt-bing-engine)))

  (setq gt-default-translator
    (gt-translator :taker (gt-taker :langs '(en fr) :text 'sentence :prompt t)
                   :engines (list (gt-google-engine) (gt-deepl-engine))
                   :render (gt-buffer-render)))

Or define several different translators and put them in `gt-preset-translators',
and switch with `gt-do-setup' at any time.

This is just a simple wrapper of `gt-start' method. Create other translate
commands in the same way using your creativity.

If ARG is not nil, translate with translator select by `gt-preset-translators'.

(fn &optional ARG)" t) (register-definition-prefixes "go-translate" '("gt-")) (autoload 'gt-do-speak "gt-core" "Speak content around point.

If the text under point or with selection has `gt-task' or `gt-tts-url'
property, try to speak it with current translation engine.

Otherwise try to TTS with local program or using selected engine.

When TTS with specific engine, you can specify the language with `lang.' prefix." t) (register-definition-prefixes "gt-core" '("gt-")) (register-definition-prefixes "gt-engine-bing" '("gt-bing-")) (register-definition-prefixes "gt-engine-chatgpt" '("gt-chatgpt-")) (register-definition-prefixes "gt-engine-deepl" '("gt-deepl-")) (register-definition-prefixes "gt-engine-echo" '("gt-")) (register-definition-prefixes "gt-engine-google" '("gt-google-")) (register-definition-prefixes "gt-engine-google-rpc" '("gt-google-rpc-")) (register-definition-prefixes "gt-engine-libre" '("gt-libre-")) (register-definition-prefixes "gt-engine-stardict" '("gt-stardict-")) (register-definition-prefixes "gt-engine-youdao" '("gt-youdao-")) (register-definition-prefixes "gt-extension" '("gt-")) (register-definition-prefixes "gt-faces" '("gt-p")) (register-definition-prefixes "gt-httpx" '("gt-")) (autoload 'gt-qrcode "gt-text-utility" "Genrate QR Code for STR.

(fn STR)" t) (register-definition-prefixes "gt-text-utility" '("gt-text-")) (provide 'go-translate-autoloads)) "connection" ((connection-autoloads connection) (register-definition-prefixes "connection" '("connection-")) (provide 'connection-autoloads)) "link" ((link-autoloads link) (register-definition-prefixes "link" '("link-")) (provide 'link-autoloads)) "dictionary" ((dictionary-autoloads dictionary) (autoload 'dictionary-mode "dictionary" "This is a mode for searching a dictionary server implementing
 the protocol defined in RFC 2229.

 This is a quick reference to this mode describing the default key bindings:

 * q close the dictionary buffer
 * h display this help information
 * s ask for a new word to search
 * d search the word at point
 * n or Tab place point to the next link
 * p or S-Tab place point to the prev link

 * m ask for a pattern and list all matching words.
 * D select the default dictionary
 * M select the default search strategy

 * Return or Button2 visit that link
 * M-Return or M-Button2 search the word beneath link in all dictionaries
 ") (autoload 'dictionary "dictionary" "Create a new dictonary buffer and install dictionary-mode" t) (autoload 'dictionary-search "dictionary" "Search the `word' in `dictionary' if given or in all if nil.  
It presents the word at point as default input and allows editing it.

(fn WORD &optional DICTIONARY)" t) (autoload 'dictionary-lookup-definition "dictionary" "Unconditionally lookup the word at point." t) (autoload 'dictionary-match-words "dictionary" "Search `pattern' in current default dictionary using default strategy.

(fn &optional PATTERN &rest IGNORED)" t) (autoload 'dictionary-mouse-popup-matching-words "dictionary" "Display entries matching the word at the cursor

(fn EVENT)" t) (autoload 'dictionary-popup-matching-words "dictionary" "Display entries matching the word at the point

(fn &optional WORD)" t) (autoload 'dictionary-tooltip-mode "dictionary" "Display tooltips for the current word

(fn &optional ARG)" t) (autoload 'dictionary-tooltip-mode "dictionary" "Display tooltips for the current word

(fn &optional ARG)" t) (autoload 'global-dictionary-tooltip-mode "dictionary" "Enable/disable dictionary-tooltip-mode for all buffers

(fn &optional ARG)" t) (register-definition-prefixes "dictionary" '("dictionary-" "global-dictionary-tooltip-mode")) (provide 'dictionary-autoloads)) "goldendict" ((goldendict-autoloads goldendict) (autoload 'goldendict-dwim "goldendict" "Query current symbol/word at point or region selected with Goldendict.
If you invoke command with `RAISE-MAIN-WINDOW' prefix \\<universal-argument>, it will raise Goldendict main window.

(fn &optional RAISE-MAIN-WINDOW)" t) (register-definition-prefixes "goldendict" '("goldendict-")) (provide 'goldendict-autoloads)) "devdocs-browser" ((devdocs-browser devdocs-browser-autoloads) (autoload 'devdocs-browser-list-docs "devdocs-browser" "Get doc metadata lists, reload cache if REFRESH-CACHE.

(fn &optional REFRESH-CACHE)") (autoload 'devdocs-browser-update-metadata "devdocs-browser" "Update doc metadata list.
To upgrade docs content, see `devdocs-browser-upgrade-doc'." t) (defalias 'devdocs-browser-update-docs 'devdocs-browser-update-metadata) (autoload 'devdocs-browser-install-doc "devdocs-browser" "Install doc by SLUG-OR-NAME.
When called interactively, user can choose from the list.
When called interactively with prefix, or FORCE is t, reinstall existing doc.

(fn SLUG-OR-NAME &optional FORCE)" t) (autoload 'devdocs-browser-uninstall-doc "devdocs-browser" "Uninstall doc by SLUG.
When called interactively, user can choose from the list.

(fn SLUG)" t) (autoload 'devdocs-browser-upgrade-doc "devdocs-browser" "Upgrade doc by SLUG, return t if upgrade success.
Also download new version of offline data if
there's offline data for current version.
When called interactively, user can choose from list.
You may need to call `devdocs-browser-update-docs' first.

(fn SLUG)" t) (autoload 'devdocs-browser-upgrade-all-docs "devdocs-browser" "Upgrade all docs." t) (autoload 'devdocs-browser-open-in "devdocs-browser" "Open entry in specified docs SLUG-OR-NAME-LIST.
When called interactively, user can choose from the list.

(fn SLUG-OR-NAME-LIST)" t) (autoload 'devdocs-browser-open "devdocs-browser" "Open entry in active docs.
Active docs are specified by `devdocs-browser-active-docs',
or `devdocs-browser-major-mode-docs-alist',
or the current doc type if called in a devdocs eww buffer.
When all of them are nil, all installed docs are used." t) (register-definition-prefixes "devdocs-browser" '("devdocs-browser-")) (provide 'devdocs-browser-autoloads)) "elfeed" ((elfeed-autoloads elfeed-csv elfeed-curl elfeed-search xml-query elfeed-db elfeed-pkg elfeed-show elfeed-log elfeed elfeed-link elfeed-lib) (autoload 'elfeed-update "elfeed" "Update all the feeds in `elfeed-feeds'." t) (autoload 'elfeed "elfeed" "Enter elfeed." t) (autoload 'elfeed-load-opml "elfeed" "Load feeds from an OPML file into `elfeed-feeds'.
When called interactively, the changes to `elfeed-feeds' are
saved to your customization file.

(fn FILE)" t) (autoload 'elfeed-export-opml "elfeed" "Export the current feed listing to OPML-formatted FILE.

(fn FILE)" t) (register-definition-prefixes "elfeed" '("elfeed-")) (register-definition-prefixes "elfeed-csv" '("elfeed-csv-")) (register-definition-prefixes "elfeed-curl" '("elfeed-curl-")) (register-definition-prefixes "elfeed-db" '("elfeed-" "with-elfeed-db-visit")) (register-definition-prefixes "elfeed-lib" '("elfeed-")) (autoload 'elfeed-link-store-link "elfeed-link" "Store a link to an elfeed search or entry buffer.

When storing a link to an entry, automatically extract all the
entry metadata.  These can be used in the capture templates as
%:elfeed-entry-<prop>.  See `elfeed-entry--create' for the list
of available props.") (autoload 'elfeed-link-open "elfeed-link" "Jump to an elfeed entry or search.

Depending on what FILTER-OR-ID looks like, we jump to either
search buffer or show a concrete entry.

(fn FILTER-OR-ID)") (eval-after-load 'org `(funcall ',(lambda nil (if (version< (org-version) "9.0") (with-no-warnings (org-add-link-type "elfeed" #'elfeed-link-open) (add-hook 'org-store-link-functions #'elfeed-link-store-link)) (with-no-warnings (org-link-set-parameters "elfeed" :follow #'elfeed-link-open :store #'elfeed-link-store-link)))))) (register-definition-prefixes "elfeed-log" '("elfeed-log")) (autoload 'elfeed-search-bookmark-handler "elfeed-search" "Jump to an elfeed-search bookmarked location.

(fn RECORD)") (autoload 'elfeed-search-desktop-restore "elfeed-search" "Restore the state of an elfeed-search buffer on desktop restore.

(fn FILE-NAME BUFFER-NAME SEARCH-FILTER)") (add-to-list 'desktop-buffer-mode-handlers '(elfeed-search-mode . elfeed-search-desktop-restore)) (register-definition-prefixes "elfeed-search" '("elfeed-s")) (autoload 'elfeed-show-bookmark-handler "elfeed-show" "Show the bookmarked entry saved in the `RECORD'.

(fn RECORD)") (register-definition-prefixes "elfeed-show" '("elfeed-")) (register-definition-prefixes "xml-query" '("xml-query")) (provide 'elfeed-autoloads)) "tablist" ((tablist-filter tablist-autoloads tablist) (autoload 'tablist-minor-mode "tablist" "Toggle tablist minor mode.

This is a minor mode.  If called interactively, toggle the
`Tablist minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `tablist-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'tablist-mode "tablist" "

(fn)" t) (register-definition-prefixes "tablist" '("tablist-")) (register-definition-prefixes "tablist-filter" '("tablist-filter-")) (provide 'tablist-autoloads)) "let-alist" ((let-alist let-alist-autoloads let-alist-pkg) (autoload 'let-alist "let-alist" "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site
      .site.contents))

essentially expands to

  (let ((.title (cdr (assq \\='title alist)))
        (.body  (cdr (assq \\='body alist)))
        (.site  (cdr (assq \\='site alist)))
        (.site.contents (cdr (assq \\='contents (cdr (assq \\='site alist))))))
    (if (and .title .body)
        .body
      .site
      .site.contents))

If you nest `let-alist' invocations, the inner one can't access
the variables of the outer one.  You can, however, access alists
inside the original alist by using dots inside the symbol, as
displayed in the example above.

Note that there is no way to differentiate the case where a key
is missing from when it is present, but its value is nil.  Thus,
the following form evaluates to nil:

    (let-alist \\='((some-key . nil))
      .some-key)

(fn ALIST &rest BODY)" nil t) (function-put 'let-alist 'lisp-indent-function 1) (register-definition-prefixes "let-alist" '("let-alist--")) (provide 'let-alist-autoloads)) "pdf-tools" ((pdf-info pdf-tools pdf-sync pdf-isearch pdf-util pdf-macs pdf-tools-autoloads pdf-virtual pdf-occur pdf-loader pdf-annot pdf-outline pdf-dev pdf-misc pdf-view pdf-links pdf-history pdf-cache) (autoload 'pdf-annot-minor-mode "pdf-annot" "Support for PDF Annotations.

\\{pdf-annot-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-Annot minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-annot-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-annot" '("pdf-annot-")) (register-definition-prefixes "pdf-cache" '("boundingbox" "define-pdf-cache-function" "page" "pdf-cache-" "textregions")) (register-definition-prefixes "pdf-dev" '("pdf-dev-")) (autoload 'pdf-history-minor-mode "pdf-history" "Keep a history of previously visited pages.

This is a simple stack-based history.  Turning the page or
following a link pushes the left-behind page on the stack, which
may be navigated with the following keys.

\\{pdf-history-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-History minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-history-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-history" '("pdf-history-")) (register-definition-prefixes "pdf-info" '("pdf-info-")) (autoload 'pdf-isearch-minor-mode "pdf-isearch" "Isearch mode for PDF buffer.

When this mode is enabled \\[isearch-forward], among other keys,
starts an incremental search in this PDF document.  Since this mode
uses external programs to highlight found matches via
image-processing, proceeding to the next match may be slow.

Therefore two isearch behaviours have been defined: Normal isearch and
batch mode.  The later one is a minor mode
(`pdf-isearch-batch-mode'), which when activated inhibits isearch
from stopping at and highlighting every single match, but rather
display them batch-wise.  Here a batch means a number of matches
currently visible in the selected window.

The kind of highlighting is determined by three faces
`pdf-isearch-match' (for the current match), `pdf-isearch-lazy'
(for all other matches) and `pdf-isearch-batch' (when in batch
mode), which see.

Colors may also be influenced by the minor-mode
`pdf-view-dark-minor-mode'.  If this is minor mode enabled, each face's
dark colors, are used (see e.g. `frame-background-mode'), instead
of the light ones.

\\{pdf-isearch-minor-mode-map}
While in `isearch-mode' the following keys are available. Note
that not every isearch command work as expected.

\\{pdf-isearch-active-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-Isearch minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-isearch-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-isearch" '("pdf-isearch-")) (autoload 'pdf-links-minor-mode "pdf-links" "Handle links in PDF documents.\\<pdf-links-minor-mode-map>

If this mode is enabled, most links in the document may be
activated by clicking on them or by pressing \\[pdf-links-action-perform] and selecting
one of the displayed keys, or by using isearch limited to
links via \\[pdf-links-isearch-link].

\\{pdf-links-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-Links minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-links-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-links-action-perform "pdf-links" "Follow LINK, depending on its type.

This may turn to another page, switch to another PDF buffer or
invoke `pdf-links-browse-uri-function'.

Interactively, link is read via `pdf-links-read-link-action'.
This function displays characters around the links in the current
page and starts reading characters (ignoring case).  After a
sufficient number of characters have been read, the corresponding
link's link is invoked.  Additionally, SPC may be used to
scroll the current page.

(fn LINK)" t) (register-definition-prefixes "pdf-links" '("pdf-links-")) (autoload 'pdf-loader-install "pdf-loader" "Prepare Emacs for using PDF Tools.

This function acts as a replacement for `pdf-tools-install' and
makes Emacs load and use PDF Tools as soon as a PDF file is
opened, but not sooner.

The arguments are passed verbatim to `pdf-tools-install', which
see.

(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)") (register-definition-prefixes "pdf-loader" '("pdf-loader--")) (register-definition-prefixes "pdf-macs" '("pdf-view-")) (autoload 'pdf-misc-minor-mode "pdf-misc" "FIXME:  Not documented.

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-misc-size-indication-minor-mode "pdf-misc" "Provide a working size indication in the mode-line.

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc-Size-Indication minor mode' mode.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-size-indication-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-misc-menu-bar-minor-mode "pdf-misc" "Display a PDF Tools menu in the menu-bar.

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc-Menu-Bar minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-menu-bar-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-misc-context-menu-minor-mode "pdf-misc" "Provide a right-click context menu in PDF buffers.

\\{pdf-misc-context-menu-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc-Context-Menu minor mode' mode.  If the prefix argument
is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-context-menu-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-misc" '("pdf-misc-")) (autoload 'pdf-occur "pdf-occur" "List lines matching STRING or PCRE.

Interactively search for a regexp. Unless a prefix arg was given,
in which case this functions performs a string search.

If `pdf-occur-prefer-string-search' is non-nil, the meaning of
the prefix-arg is inverted.

(fn STRING &optional REGEXP-P)" t) (autoload 'pdf-occur-multi-command "pdf-occur" "Perform `pdf-occur' on multiple buffer.

For a programmatic search of multiple documents see
`pdf-occur-search'." t) (defvar pdf-occur-global-minor-mode nil "Non-nil if Pdf-Occur-Global minor mode is enabled.
See the `pdf-occur-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pdf-occur-global-minor-mode'.") (custom-autoload 'pdf-occur-global-minor-mode "pdf-occur" nil) (autoload 'pdf-occur-global-minor-mode "pdf-occur" "Enable integration of Pdf Occur with other modes.

This global minor mode enables (or disables)
`pdf-occur-ibuffer-minor-mode' and `pdf-occur-dired-minor-mode'
in all current and future ibuffer/dired buffer.

This is a global minor mode.  If called interactively, toggle the
`Pdf-Occur-Global minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pdf-occur-global-minor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-occur-ibuffer-minor-mode "pdf-occur" "Hack into ibuffer's do-occur binding.

This mode remaps `ibuffer-do-occur' to
`pdf-occur-ibuffer-do-occur', which will start the PDF Tools
version of `occur', if all marked buffer's are in `pdf-view-mode'
and otherwise fallback to `ibuffer-do-occur'.

This is a minor mode.  If called interactively, toggle the
`Pdf-Occur-Ibuffer minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-occur-ibuffer-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-occur-dired-minor-mode "pdf-occur" "Hack into dired's `dired-do-search' binding.

This mode remaps `dired-do-search' to
`pdf-occur-dired-do-search', which will start the PDF Tools
version of `occur', if all marked buffer's are in `pdf-view-mode'
and otherwise fallback to `dired-do-search'.

This is a minor mode.  If called interactively, toggle the
`Pdf-Occur-Dired minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-occur-dired-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-occur" '("pdf-occur-")) (autoload 'pdf-outline-minor-mode "pdf-outline" "Display an outline of a PDF document.

This provides a PDF's outline on the menu bar via imenu.
Additionally the same outline may be viewed in a designated
buffer.

\\{pdf-outline-minor-mode-map}

This is a minor mode.  If called interactively, toggle the
`Pdf-Outline minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-outline-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-outline "pdf-outline" "Display an PDF outline of BUFFER.

BUFFER defaults to the current buffer.  Select the outline
buffer, unless NO-SELECT-WINDOW-P is non-nil.

(fn &optional BUFFER NO-SELECT-WINDOW-P)" t) (autoload 'pdf-outline-imenu-enable "pdf-outline" "Enable imenu in the current PDF buffer." t) (register-definition-prefixes "pdf-outline" '("pdf-outline")) (autoload 'pdf-sync-minor-mode "pdf-sync" "Correlate a PDF position with the TeX file.

\\<pdf-sync-minor-mode-map>
This works via SyncTeX, which means the TeX sources need to have
been compiled with `--synctex=1'.  In AUCTeX this can be done by
setting `TeX-source-correlate-method' to `synctex' (before AUCTeX
is loaded) and enabling `TeX-source-correlate-mode'.

Then \\[pdf-sync-backward-search-mouse] in the PDF buffer will
open the corresponding TeX location.

If AUCTeX is your preferred tex-mode, this library arranges to
bind `pdf-sync-forward-display-pdf-key' (the default is `C-c C-g')
to `pdf-sync-forward-search' in `TeX-source-correlate-map'.  This
function displays the PDF page corresponding to the current
position in the TeX buffer.  This function only works together
with AUCTeX.

This is a minor mode.  If called interactively, toggle the
`Pdf-Sync minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-sync-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "pdf-sync" '("pdf-sync-")) (defvar pdf-tools-handle-upgrades t "Whether PDF Tools should handle upgrading itself.") (custom-autoload 'pdf-tools-handle-upgrades "pdf-tools" t) (autoload 'pdf-tools-install "pdf-tools" "Install PDF-Tools in all current and future PDF buffers.

If the `pdf-info-epdfinfo-program' is not running or does not
appear to be working, attempt to rebuild it.  If this build
succeeded, continue with the activation of the package.
Otherwise fail silently, i.e. no error is signaled.

Build the program (if necessary) without asking first, if
NO-QUERY-P is non-nil.

Don't attempt to install system packages, if SKIP-DEPENDENCIES-P
is non-nil.

Do not signal an error in case the build failed, if NO-ERROR-P is
non-nil.

Attempt to install system packages (even if it is deemed
unnecessary), if FORCE-DEPENDENCIES-P is non-nil.

Note that SKIP-DEPENDENCIES-P and FORCE-DEPENDENCIES-P are
mutually exclusive.

Note further, that you can influence the installation directory
by setting `pdf-info-epdfinfo-program' to an appropriate
value (e.g. ~/bin/epdfinfo) before calling this function.

See `pdf-view-mode' and `pdf-tools-enabled-modes'.

(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)" t) (autoload 'pdf-tools-enable-minor-modes "pdf-tools" "Enable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'.

(fn &optional MODES)" t) (autoload 'pdf-tools-help "pdf-tools" "Show a Help buffer for `pdf-tools'." t) (register-definition-prefixes "pdf-tools" '("pdf-tools-")) (register-definition-prefixes "pdf-util" '("display-buffer-split-below-and-attach" "pdf-util-")) (autoload 'pdf-view-bookmark-jump-handler "pdf-view" "The bookmark handler-function interface for bookmark BMK.

See also `pdf-view-bookmark-make-record'.

(fn BMK)") (register-definition-prefixes "pdf-view" '("cua-copy-region--pdf-view-advice" "pdf-view-")) (autoload 'pdf-virtual-edit-mode "pdf-virtual" "Major mode when editing a virtual PDF buffer.

(fn)" t) (autoload 'pdf-virtual-view-mode "pdf-virtual" "Major mode in virtual PDF buffers.

(fn)" t) (defvar pdf-virtual-global-minor-mode nil "Non-nil if Pdf-Virtual-Global minor mode is enabled.
See the `pdf-virtual-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pdf-virtual-global-minor-mode'.") (custom-autoload 'pdf-virtual-global-minor-mode "pdf-virtual" nil) (autoload 'pdf-virtual-global-minor-mode "pdf-virtual" "Enable recognition and handling of VPDF files.

This is a global minor mode.  If called interactively, toggle the
`Pdf-Virtual-Global minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pdf-virtual-global-minor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pdf-virtual-buffer-create "pdf-virtual" "

(fn &optional FILENAMES BUFFER-NAME DISPLAY-P)" t) (register-definition-prefixes "pdf-virtual" '("pdf-virtual-")) (provide 'pdf-tools-autoloads)) "eat" ((eat eat-autoloads) (autoload 'eat-term-make "eat" "Make a Eat terminal at POSITION in BUFFER.

(fn BUFFER POSITION)") (autoload 'eat "eat" "Start a new Eat terminal emulator in a buffer.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like \\[universal-argument] 42 \\[eat]),
switch to the session with that number, or create it if it doesn't
already exist.

With double prefix argument ARG, ask for the program to run and run it
in a newly created session.

PROGRAM can be a shell command.

(fn &optional PROGRAM ARG)" t) (autoload 'eat-other-window "eat" "Start a new Eat terminal emulator in a buffer in another window.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG switch to the session with that number, or
create it if it doesn't already exist.

With double prefix argument ARG, ask for the program to run and run it
in a newly created session.

PROGRAM can be a shell command.

(fn &optional PROGRAM ARG)" t) (defvar eat-eshell-mode nil "Non-nil if Eat-Eshell mode is enabled.
See the `eat-eshell-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eat-eshell-mode'.") (custom-autoload 'eat-eshell-mode "eat" nil) (autoload 'eat-eshell-mode "eat" "Toggle Eat terminal emulation in Eshell.

This is a global minor mode.  If called interactively, toggle the
`Eat-Eshell mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='eat-eshell-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (defvar eat-eshell-visual-command-mode nil "Non-nil if Eat-Eshell-Visual-Command mode is enabled.
See the `eat-eshell-visual-command-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eat-eshell-visual-command-mode'.") (custom-autoload 'eat-eshell-visual-command-mode "eat" nil) (autoload 'eat-eshell-visual-command-mode "eat" "Toggle running Eshell visual commands with Eat.

This is a global minor mode.  If called interactively, toggle the
`Eat-Eshell-Visual-Command mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='eat-eshell-visual-command-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'eat-project "eat" "Start Eat in the current project's root directory.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like
\\[universal-argument] 42 \\[eat-project]), switch to the session with
that number, or create it if it doesn't already exist.

(fn &optional ARG)" t) (autoload 'eat-project-other-window "eat" "Start Eat in the current project root directory in another window.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like
\\[universal-argument] 42 \\[eat-project]), switch to the session with
that number, or create it if it doesn't already exist.

(fn &optional ARG)" t) (register-definition-prefixes "eat" '("eat-")) (provide 'eat-autoloads)) "vterm" ((vterm vterm-autoloads) (autoload 'vterm-module-compile "vterm" "Compile vterm-module." t) (autoload 'vterm--bookmark-handler "vterm" "Handler to restore a vterm bookmark BMK.

If a vterm buffer of the same name does not exist, the function will create a
new vterm buffer of the name. It also checks the current directory and sets
it to the bookmarked directory if needed.

(fn BMK)") (autoload 'vterm-next-error-function "vterm" "Advance to the next error message and visit the file where the error was.
This is the value of `next-error-function' in Compilation
buffers.  Prefix arg N says how many error messages to move
forwards (or backwards, if negative).

Optional argument RESET clears all the errors.

(fn N &optional RESET)" t) (autoload 'vterm "vterm" "Create an interactive Vterm buffer.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'.

(fn &optional ARG)" t) (autoload 'vterm-other-window "vterm" "Create an interactive Vterm buffer in another window.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'.

(fn &optional ARG)" t) (register-definition-prefixes "vterm" '("vterm-")) (provide 'vterm-autoloads)) "notmuch" ((notmuch-query notmuch-compat notmuch-lib notmuch-tree notmuch-maildir-fcc notmuch-mua notmuch-wash coolj notmuch-tag notmuch-parser notmuch-autoloads rstdoc notmuch notmuch-show notmuch-print notmuch-draft notmuch-address notmuch-hello notmuch-jump notmuch-message notmuch-crypto notmuch-company make-deps) (register-definition-prefixes "coolj" '("coolj-")) (register-definition-prefixes "make-deps" '("batch-make-deps" "make-deps")) (autoload 'notmuch-search "notmuch" "Display threads matching QUERY in a notmuch-search buffer.

If QUERY is nil, it is read interactively from the minibuffer.
Other optional parameters are used as follows:

  OLDEST-FIRST: A Boolean controlling the sort order of returned threads
  HIDE-EXCLUDED: A boolean controlling whether to omit threads with excluded
                 tags.
  TARGET-THREAD: A thread ID (without the thread: prefix) that will be made
                 current if it appears in the search results.
  TARGET-LINE: The line number to move to if the target thread does not
               appear in the search results.
  NO-DISPLAY: Do not try to foreground the search results buffer. If it is
              already foregrounded i.e. displayed in a window, this has no
              effect, meaning the buffer will remain visible.

When called interactively, this will prompt for a query and use
the configured default sort order.

(fn &optional QUERY OLDEST-FIRST HIDE-EXCLUDED TARGET-THREAD TARGET-LINE NO-DISPLAY)" t) (autoload 'notmuch "notmuch" "Run notmuch and display saved searches, known tags, etc." t) (autoload 'notmuch-cycle-notmuch-buffers "notmuch" "Cycle through any existing notmuch buffers (search, show or hello).

If the current buffer is the only notmuch buffer, bury it.
If no notmuch buffers exist, run `notmuch'." t) (register-definition-prefixes "notmuch" '("notmuch-")) (register-definition-prefixes "notmuch-address" '("notmuch-address-")) (autoload 'notmuch-company-setup "notmuch-company") (autoload 'notmuch-company "notmuch-company" "`company-mode' completion back-end for `notmuch'.

(fn COMMAND &optional ARG &rest IGNORE)" t) (register-definition-prefixes "notmuch-company" '("notmuch-company-last-prefix")) (register-definition-prefixes "notmuch-compat" '("notmuch-")) (register-definition-prefixes "notmuch-crypto" '("notmuch-crypto-")) (register-definition-prefixes "notmuch-draft" '("notmuch-draft-")) (autoload 'notmuch-hello "notmuch-hello" "Run notmuch and display saved searches, known tags, etc.

(fn &optional NO-DISPLAY)" t) (register-definition-prefixes "notmuch-hello" '("notmuch-")) (autoload 'notmuch-jump-search "notmuch-jump" "Jump to a saved search by shortcut key.

This prompts for and performs a saved search using the shortcut
keys configured in the :key property of `notmuch-saved-searches'.
Typically these shortcuts are a single key long, so this is a
fast way to jump to a saved search from anywhere in Notmuch." t) (autoload 'notmuch-jump "notmuch-jump" "Interactively prompt for one of the keys in ACTION-MAP.

Displays a summary of all bindings in ACTION-MAP in the
minibuffer, reads a key from the minibuffer, and performs the
corresponding action.  The prompt can be canceled with C-g or
RET.  PROMPT must be a string to use for the prompt.  PROMPT
should include a space at the end.

ACTION-MAP must be a list of triples of the form
  (KEY LABEL ACTION)
where KEY is a key binding, LABEL is a string label to display in
the buffer, and ACTION is a nullary function to call.  LABEL may
be null, in which case the action will still be bound, but will
not appear in the pop-up buffer.

(fn ACTION-MAP PROMPT)") (register-definition-prefixes "notmuch-jump" '("notmuch-jump-")) (register-definition-prefixes "notmuch-lib" '("notmuch-")) (register-definition-prefixes "notmuch-maildir-fcc" '("notmuch-" "with-temporary-notmuch-message-buffer")) (register-definition-prefixes "notmuch-message" '("notmuch-message-")) (autoload 'notmuch-mua-mail "notmuch-mua" "Invoke the notmuch mail composition window.

The position of point when the function returns differs depending
on the values of TO and SUBJECT.  If both are non-nil, point is
moved to the message's body.  If SUBJECT is nil but TO isn't,
point is moved to the \"Subject:\" header.  Otherwise, point is
moved to the \"To:\" header.

(fn &optional TO SUBJECT OTHER-HEADERS CONTINUE SWITCH-FUNCTION YANK-ACTION SEND-ACTIONS RETURN-ACTION &rest IGNORED)" t) (autoload 'notmuch-mua-send-and-exit "notmuch-mua" "

(fn &optional ARG)" t) (autoload 'notmuch-mua-send "notmuch-mua" "

(fn &optional ARG)" t) (autoload 'notmuch-mua-kill-buffer "notmuch-mua" nil t) (define-mail-user-agent 'notmuch-user-agent 'notmuch-mua-mail 'notmuch-mua-send-and-exit 'notmuch-mua-kill-buffer 'notmuch-mua-send-hook) (register-definition-prefixes "notmuch-mua" '("notmuch-")) (register-definition-prefixes "notmuch-parser" '("notmuch-sexp-")) (register-definition-prefixes "notmuch-print" '("notmuch-print-")) (register-definition-prefixes "notmuch-query" '("notmuch-query-")) (autoload 'notmuch-show "notmuch-show" "Run \"notmuch show\" with the given thread ID and display results.

ELIDE-TOGGLE, if non-nil, inverts the default elide behavior.

The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the
next thread from that buffer can be show when done with this
one).

The optional QUERY-CONTEXT is a notmuch search term. Only
messages from the thread matching this search term are shown if
non-nil.

The optional BUFFER-NAME provides the name of the buffer in
which the message thread is shown. If it is nil (which occurs
when the command is called interactively) the argument to the
function is used.

Returns the buffer containing the messages, or NIL if no messages
matched.

(fn THREAD-ID &optional ELIDE-TOGGLE PARENT-BUFFER QUERY-CONTEXT BUFFER-NAME)" t) (register-definition-prefixes "notmuch-show" '("notmuch-" "with-current-notmuch-show-message")) (register-definition-prefixes "notmuch-tag" '("notmuch-")) (autoload 'notmuch-tree "notmuch-tree" "Display threads matching QUERY in tree view.

The arguments are:
  QUERY: the main query. This can be any query but in many cases will be
      a single thread. If nil this is read interactively from the minibuffer.
  QUERY-CONTEXT: is an additional term for the query. The query used
      is QUERY and QUERY-CONTEXT unless that does not match any messages
      in which case we fall back to just QUERY.
  TARGET: A message ID (with the id: prefix) that will be made
      current if it appears in the tree view results.
  BUFFER-NAME: the name of the buffer to display the tree view. If
      it is nil \"*notmuch-tree\" followed by QUERY is used.
  OPEN-TARGET: If TRUE open the target message in the message pane.
  UNTHREADED: If TRUE only show matching messages in an unthreaded view.

(fn &optional QUERY QUERY-CONTEXT TARGET BUFFER-NAME OPEN-TARGET UNTHREADED PARENT-BUFFER OLDEST-FIRST HIDE-EXCLUDED)" t) (register-definition-prefixes "notmuch-tree" '("notmuch-")) (register-definition-prefixes "notmuch-wash" '("notmuch-wash-")) (register-definition-prefixes "rstdoc" '("rst")) (provide 'notmuch-autoloads)) "mu4e" ((mu4e-lists mu4e-speedbar mu4e-context mu4e-autoloads mu4e-draft mu4e-server mu4e-obsolete mu4e-query-items mu4e-window mu4e-helpers mu4e-actions mu4e-compose mu4e-org mu4e-search mu4e-vars mu4e-config mu4e mu4e-icalendar mu4e-headers mu4e-message mu4e-modeline mu4e-contacts mu4e-main mu4e-contrib mu4e-mark mu4e-update mu4e-view mu4e-bookmarks mu4e-folders mu4e-notification) (autoload 'mu4e "mu4e" "If mu4e is not running yet, start it.
Then, show the main window, unless BACKGROUND (prefix-argument)
is non-nil.

(fn &optional BACKGROUND)" t) (register-definition-prefixes "mu4e" '("mu4e-")) (register-definition-prefixes "mu4e-actions" '("mu4e-")) (register-definition-prefixes "mu4e-bookmarks" '("mu4e-")) (autoload 'mu4e~compose-mail "mu4e-compose" "This is mu4e's implementation of `compose-mail'.
Quoting its docstring:

Start composing a mail message to send. This uses the user's
chosen mail composition package as selected with the variable
`mail-user-agent'. The optional arguments TO and SUBJECT specify
recipients and the initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

CONTINUE, if non-nil, says to continue editing a message already
being composed.  Interactively, CONTINUE is the prefix argument.

YANK-ACTION, if non-nil, is an action to perform, if and when
necessary, to insert the raw text of the message being replied
to. It has the form (FUNCTION . ARGS). The user agent will apply
FUNCTION to ARGS, to insert the raw text of the original message.
(The user agent will also run `mail-citation-hook', *after* the
original text has been inserted in this way.)

SEND-ACTIONS is a list of actions to call when the message is sent.
Each action has the form (FUNCTION . ARGS).

RETURN-ACTION, if non-nil, is an action for returning to the
caller.  It has the form (FUNCTION . ARGS).  The function is
called after the mail has been sent or put aside, and the mail
buffer buried.

(fn &optional TO SUBJECT OTHER-HEADERS CONTINUE SWITCH-ACTION YANK-ACTION SEND-ACTIONS RETURN-ACTION)") (define-mail-user-agent 'mu4e-user-agent 'mu4e~compose-mail 'message-send-and-exit 'message-kill-buffer 'message-send-hook) (register-definition-prefixes "mu4e-compose" '("mu4e")) (register-definition-prefixes "mu4e-config" '("mu4e-")) (register-definition-prefixes "mu4e-contacts" '("mu4e-")) (register-definition-prefixes "mu4e-context" '("mu4e-" "with-mu4e-context-vars")) (register-definition-prefixes "mu4e-contrib" '("eshell/mu4e-attach" "mu4e-")) (register-definition-prefixes "mu4e-draft" '("mu4e")) (register-definition-prefixes "mu4e-folders" '("mu4e")) (register-definition-prefixes "mu4e-headers" '("mu4e")) (register-definition-prefixes "mu4e-helpers" '("mu4e-")) (autoload 'mu4e-icalendar-setup "mu4e-icalendar" "Perform the necessary initialization to use mu4e-icalendar.") (register-definition-prefixes "mu4e-icalendar" '("mu4e-")) (register-definition-prefixes "mu4e-lists" '("mu4e-")) (register-definition-prefixes "mu4e-main" '("mu4e-")) (register-definition-prefixes "mu4e-mark" '("mu4e-")) (register-definition-prefixes "mu4e-message" '("mu4e-")) (register-definition-prefixes "mu4e-modeline" '("mu4e-")) (register-definition-prefixes "mu4e-notification" '("mu4e-")) (register-definition-prefixes "mu4e-org" '("mu4e-")) (register-definition-prefixes "mu4e-query-items" '("mu4e-")) (register-definition-prefixes "mu4e-search" '("mu4e-")) (register-definition-prefixes "mu4e-server" '("mu4e-")) (autoload 'mu4e-speedbar-buttons "mu4e-speedbar" "Create buttons for any mu4e BUFFER.

(fn &optional BUFFER)" t) (register-definition-prefixes "mu4e-speedbar" '("mu4e")) (register-definition-prefixes "mu4e-update" '("mu4e-")) (register-definition-prefixes "mu4e-vars" '("mu4e")) (register-definition-prefixes "mu4e-view" '("gnus-icalendar-event-from-handle" "mu4e")) (register-definition-prefixes "mu4e-window" '("mu4e-")) (provide 'mu4e-autoloads)) "erc" ((erc-truncate erc-speedbar erc-button erc-join erc-log erc-imenu erc-autoloads erc-netsplit erc-ring erc-nicks erc-goodies erc-pcomplete erc-notify erc-fill erc-ezbounce erc-track erc-lang erc-sound erc-sasl erc-replace erc-status-sidebar erc erc-capab erc-stamp erc-services erc-dcc erc-compat erc-page erc-networks erc-desktop-notifications erc-menu erc-list erc-pkg erc-identd erc-xdcc erc-match erc-autoaway erc-spelling erc-backend erc-loaddefs erc-ibuffer erc-common) (dolist (symbol '(erc-sasl erc-spelling erc-imenu erc-nicks)) (custom-add-load symbol symbol)) (autoload 'erc-select-read-args "erc" "Prompt for connection parameters and return them in a plist.
By default, collect `:server', `:port', `:nickname', and
`:password'.  With a non-nil prefix argument, also prompt for
`:user' and `:full-name'.  Also return various environmental
properties needed by entry-point commands, like `erc-tls'.") (autoload 'erc-server-select "erc" "Interactively connect to a server from `erc-server-alist'." t) (make-obsolete 'erc-server-select 'erc-tls "30.1") (autoload 'erc "erc" "Connect to an Internet Relay Chat SERVER on a non-TLS PORT.
Use NICK and USER, when non-nil, to inform the IRC commands of
the same name, possibly factoring in a non-nil FULL-NAME as well.
When PASSWORD is non-nil, also send an opening server password
via the \"PASS\" command.  Interactively, prompt for SERVER,
PORT, NICK, and PASSWORD, along with USER and FULL-NAME when
given a prefix argument.  Non-interactively, expect the rarely
needed ID parameter, when non-nil, to be a symbol or a string for
naming the server buffer and identifying the connection
unequivocally.  Once connected, return the server buffer.  (See
Info node `(erc) Connecting' for details about all mentioned
parameters.)

Together with `erc-tls', this command serves as the main entry
point for ERC, the powerful, modular, and extensible IRC client.
Non-interactively, both commands accept the following keyword
arguments, with their defaults supplied by the indicated
\"compute\" functions:

  :server    `erc-compute-server'
  :port      `erc-compute-port'
  :nick      `erc-compute-nick'
  :user      `erc-compute-user'
  :password   N/A
  :full-name `erc-compute-full-name'
  :id'        N/A

For example, when called in the following manner

   (erc :server \"irc.libera.chat\" :full-name \"J. Random Hacker\")

ERC assigns SERVER and FULL-NAME the associated keyword values
and defers to `erc-compute-port', `erc-compute-user', and
`erc-compute-nick' for those respective parameters.

(fn &key SERVER PORT NICK USER PASSWORD FULL-NAME ID)" t) (defalias 'erc-select #'erc) (autoload 'erc-tls "erc" "Connect to an IRC server over a TLS-encrypted connection.
Interactively, prompt for SERVER, PORT, NICK, and PASSWORD, along
with USER and FULL-NAME when given a prefix argument.
Non-interactively, also accept a CLIENT-CERTIFICATE, which should
be a list containing the file name of the certificate's key
followed by that of the certificate itself.  Alternatively,
accept a value of t instead of a list, to tell ERC to query
`auth-source' for the certificate's details.

Example client certificate (CertFP) usage:

    (erc-tls :server \"irc.libera.chat\" :port 6697
             :client-certificate
             \\='(\"/home/bandali/my-cert.key\"
               \"/home/bandali/my-cert.crt\"))

See the alternative entry-point command `erc' as well as Info
node `(erc) Connecting' for a fuller description of the various
parameters, like ID.

(fn &key SERVER PORT NICK USER PASSWORD FULL-NAME CLIENT-CERTIFICATE ID)" t) (autoload 'erc-handle-irc-url "erc" "Use ERC to IRC on HOST:PORT in CHANNEL.
If ERC is already connected to HOST:PORT, simply /join CHANNEL.
Otherwise, connect to HOST:PORT as NICK and /join CHANNEL.

Beginning with ERC 5.5, new connections require human intervention.
Customize `erc-url-connect-function' to override this.

(fn HOST PORT CHANNEL NICK PASSWORD &optional SCHEME)") (register-definition-prefixes "erc" '("erc-")) (register-definition-prefixes "erc-autoaway" '("erc-auto")) (register-definition-prefixes "erc-backend" '("erc-")) (register-definition-prefixes "erc-button" '("erc-")) (register-definition-prefixes "erc-capab" '("erc-capab-identify-")) (register-definition-prefixes "erc-common" '("define-erc-module" "erc-")) (register-definition-prefixes "erc-compat" '("erc-")) (register-definition-prefixes "erc-dcc" '("erc-" "pcomplete/erc-mode/")) (register-definition-prefixes "erc-desktop-notifications" '("erc-notifications-")) (register-definition-prefixes "erc-ezbounce" '("erc-ezb-")) (register-definition-prefixes "erc-fill" '("erc-")) (register-definition-prefixes "erc-goodies" '("erc-")) (register-definition-prefixes "erc-ibuffer" '("erc-")) (register-definition-prefixes "erc-identd" '("erc-identd-")) (register-definition-prefixes "erc-imenu" '("erc-")) (register-definition-prefixes "erc-join" '("erc-")) (register-definition-prefixes "erc-lang" '("erc-cmd-LANG" "iso-639-1-languages" "language")) (register-definition-prefixes "erc-list" '("erc-")) (register-definition-prefixes "erc-log" '("erc-")) (register-definition-prefixes "erc-match" '("erc-")) (register-definition-prefixes "erc-menu" '("erc-menu-")) (register-definition-prefixes "erc-netsplit" '("erc-")) (autoload 'erc-determine-network "erc-networks" "Return the name of the network or \"Unknown\" as a symbol.
Use the server parameter NETWORK if provided, otherwise parse the
server name and search for a match in `erc-networks-alist'.") (make-obsolete 'erc-determine-network '"maybe see `erc-networks--determine'" "29.1") (register-definition-prefixes "erc-networks" '("erc-")) (register-definition-prefixes "erc-nicks" '("erc-nicks-")) (register-definition-prefixes "erc-notify" '("erc-")) (register-definition-prefixes "erc-page" '("erc-")) (register-definition-prefixes "erc-pcomplete" '("erc-pcomplet" "pcomplete")) (register-definition-prefixes "erc-replace" '("erc-replace-")) (register-definition-prefixes "erc-ring" '("erc-")) (register-definition-prefixes "erc-sasl" '("erc-sasl-")) (register-definition-prefixes "erc-services" '("erc-")) (register-definition-prefixes "erc-sound" '("erc-")) (register-definition-prefixes "erc-speedbar" '("erc-")) (register-definition-prefixes "erc-spelling" '("erc-spelling-")) (register-definition-prefixes "erc-stamp" '("erc-")) (register-definition-prefixes "erc-status-sidebar" '("erc-status-sidebar-")) (register-definition-prefixes "erc-track" '("erc-")) (register-definition-prefixes "erc-truncate" '("erc-")) (register-definition-prefixes "erc-xdcc" '("erc-")) (provide 'erc-autoloads)) "eldoc-box" ((eldoc-box-autoloads eldoc-box) (autoload 'eldoc-box-help-at-point "eldoc-box" "Display documentation of the symbol at point." t) (autoload 'eldoc-box-hover-mode "eldoc-box" "Display hover documentations in a childframe.

The default position of childframe is upper corner.

This is a minor mode.  If called interactively, toggle the
`Eldoc-Box-Hover mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `eldoc-box-hover-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'eldoc-box-hover-at-point-mode "eldoc-box" "A convenient minor mode to display doc at point.

You can use \\[keyboard-quit] to hide the doc.

This is a minor mode.  If called interactively, toggle the
`Eldoc-Box-Hover-At-Point mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `eldoc-box-hover-at-point-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "eldoc-box" '("eldoc-box-")) (provide 'eldoc-box-autoloads)) "json-snatcher" ((json-snatcher-autoloads json-snatcher) (autoload 'jsons-print-path "json-snatcher" "Print the path to the JSON value under point, and save it in the kill ring." t) (register-definition-prefixes "json-snatcher" '("jsons-")) (provide 'json-snatcher-autoloads)) "json-mode" ((json-mode json-mode-autoloads) (defconst json-mode-standard-file-ext '(".json" ".jsonld") "List of JSON file extensions.") (defsubst json-mode--update-auto-mode (filenames) "Update the `json-mode' entry of `auto-mode-alist'.

FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry" (let* ((new-regexp (rx-to-string `(seq (eval (cons 'or (append json-mode-standard-file-ext ',filenames))) eot))) (new-entry (cons new-regexp 'json-mode)) (old-entry (when (boundp 'json-mode--auto-mode-entry) json-mode--auto-mode-entry))) (setq auto-mode-alist (delete old-entry auto-mode-alist)) (add-to-list 'auto-mode-alist new-entry) new-entry)) (defvar json-mode-auto-mode-list '(".babelrc" ".bowerrc" "composer.lock") "List of filenames for the JSON entry of `auto-mode-alist'.

Note however that custom `json-mode' entries in `auto-mode-alist'
won’t be affected.") (custom-autoload 'json-mode-auto-mode-list "json-mode" nil) (defvar json-mode--auto-mode-entry (json-mode--update-auto-mode json-mode-auto-mode-list) "Regexp generated from the `json-mode-auto-mode-list'.") (autoload 'json-mode "json-mode" "Major mode for editing JSON files.

(fn)" t) (autoload 'jsonc-mode "json-mode" "Major mode for editing JSON files with comments.

(fn)" t) (add-to-list 'magic-fallback-mode-alist '("^[{[]$" . json-mode)) (autoload 'json-mode-show-path "json-mode" "Print the path to the node at point to the minibuffer." t) (autoload 'json-mode-kill-path "json-mode" "Save JSON path to object at point to kill ring." t) (autoload 'json-mode-beautify "json-mode" "Beautify/pretty-print from BEGIN to END.

If the region is not active, beautify the entire buffer .

(fn BEGIN END)" t) (register-definition-prefixes "json-mode" '("json")) (provide 'json-mode-autoloads)) "jsonrpc" ((jsonrpc-pkg jsonrpc-autoloads jsonrpc) (register-definition-prefixes "jsonrpc" '("jsonrpc-")) (provide 'jsonrpc-autoloads)) "pfuture" ((pfuture pfuture-autoloads) (autoload 'pfuture-new "pfuture" "Create a new future process for command CMD.
Any arguments after the command are interpreted as arguments to the command.
This will return a process object with additional \\='stderr and \\='stdout
properties, which can be read via (process-get process \\='stdout) and
(process-get process \\='stderr) or alternatively with
(pfuture-result process) or (pfuture-stderr process).

Note that CMD must be a *sequence* of strings, meaning
this is wrong: (pfuture-new \"git status\")
this is right: (pfuture-new \"git\" \"status\")

(fn &rest CMD)") (register-definition-prefixes "pfuture" '("pfuture-")) (provide 'pfuture-autoloads)) "lv" ((lv-autoloads lv) (register-definition-prefixes "lv" '("lv-")) (provide 'lv-autoloads)) "hydra" ((hydra-autoloads hydra hydra-ox hydra-examples) (autoload 'defhydra "hydra" "Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit, :bind, and :column.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

:column is a string that sets the column for all subsequent heads.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t) (function-put 'defhydra 'lisp-indent-function 'defun) (function-put 'defhydra 'doc-string-elt 3) (register-definition-prefixes "hydra" '("defhydra" "hydra-")) (register-definition-prefixes "hydra-examples" '("hydra-" "org-agenda-cts" "whitespace-mode")) (register-definition-prefixes "hydra-ox" '("hydra-ox")) (provide 'hydra-autoloads)) "cfrs" ((cfrs cfrs-autoloads) (autoload 'cfrs-read "cfrs" "Read a string using a pos-frame with given PROMPT and INITIAL-INPUT.

(fn PROMPT &optional INITIAL-INPUT)") (register-definition-prefixes "cfrs" '("cfrs-")) (provide 'cfrs-autoloads)) "treemacs" ((treemacs-mouse-interface treemacs-tag-follow-mode treemacs-header-line treemacs-filewatch-mode treemacs-visuals treemacs-tags treemacs-core-utils treemacs-macros treemacs-dom treemacs-scope treemacs-interface treemacs-workspaces treemacs-diagnostics treemacs-extensions treemacs-file-management treemacs-fringe-indicator treemacs-logging treemacs-mode treemacs-annotations treemacs-themes treemacs-treelib treemacs-git-commit-diff-mode treemacs-follow-mode treemacs-hydras treemacs-compatibility treemacs-async treemacs-icons treemacs-persistence treemacs-rendering treemacs treemacs-bookmarks treemacs-faces treemacs-peek-mode treemacs-project-follow-mode treemacs-customization treemacs-autoloads) (autoload 'treemacs-version "treemacs" "Return the `treemacs-version'." t) (autoload 'treemacs "treemacs" "Initialise or toggle treemacs.
- If the treemacs window is visible hide it.
- If a treemacs buffer exists, but is not visible show it.
- If no treemacs buffer exists for the current frame create and show it.
- If the workspace is empty additionally ask for the root path of the first
  project to add.
- With a prefix ARG launch treemacs and force it to select a workspace

(fn &optional ARG)" t) (autoload 'treemacs-select-directory "treemacs" "Select a directory to open in treemacs.
This command will open *just* the selected directory in treemacs.  If there are
other projects in the workspace they will be removed.

To *add* a project to the current workspace use
`treemacs-add-project-to-workspace' or
`treemacs-add-and-display-current-project' instead." t) (autoload 'treemacs-find-file "treemacs" "Find and focus the current file in the treemacs window.
If the current buffer has visits no file or with a prefix ARG ask for the
file instead.
Will show/create a treemacs buffers if it is not visible/does not exist.
For the most part only useful when `treemacs-follow-mode' is not active.

(fn &optional ARG)" t) (autoload 'treemacs-find-tag "treemacs" "Find and move point to the tag at point in the treemacs view.
Most likely to be useful when `treemacs-tag-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root.  If no treemacs buffer exists it will be created with the current file's
containing directory as root.  Will do nothing if the current buffer is not
visiting a file or Emacs cannot find any tags for the current file." t) (autoload 'treemacs-start-on-boot "treemacs" "Initialiser specifically to start treemacs as part of your init file.

Ensures that all visual elements are present which might otherwise be missing
because their setup requires an interactive command or a post-command hook.

FOCUS-TREEMACS indicates whether the treemacs window should be selected.

(fn &optional FOCUS-TREEMACS)") (autoload 'treemacs-select-window "treemacs" "Select the treemacs window if it is visible.
Bring it to the foreground if it is not visible.
Initialise a new treemacs buffer as calling `treemacs' would if there is no
treemacs buffer for this frame.

In case treemacs is already selected behaviour will depend on
`treemacs-select-when-already-in-treemacs'.

A non-nil prefix ARG will also force a workspace switch.

(fn &optional ARG)" t) (autoload 'treemacs-show-changelog "treemacs" "Show the changelog of treemacs." t) (autoload 'treemacs-edit-workspaces "treemacs" "Edit your treemacs workspaces and projects as an `org-mode' file." t) (autoload 'treemacs-add-and-display-current-project-exclusively "treemacs" "Display the current project, and *only* the current project.
Like `treemacs-add-and-display-current-project' this will add the current
project to treemacs based on either projectile, the built-in project.el, or the
current working directory.

However the \\='exclusive\\=' part means that it will make the current project
the only project, all other projects *will be removed* from the current
workspace." t) (autoload 'treemacs-add-and-display-current-project "treemacs" "Open treemacs and add the current project root to the workspace.
The project is determined first by projectile (if treemacs-projectile is
installed), then by project.el, then by the current working directory.

If the project is already registered with treemacs just move point to its root.
An error message is displayed if the current buffer is not part of any project." t) (register-definition-prefixes "treemacs" '("treemacs-version")) (register-definition-prefixes "treemacs-annotations" '("treemacs-")) (register-definition-prefixes "treemacs-async" '("treemacs-")) (autoload 'treemacs-bookmark "treemacs-bookmarks" "Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location, in a similar
fashion to `treemacs-find-file'.

With a prefix argument ARG treemacs will also open the bookmarked location.

(fn &optional ARG)" t) (autoload 'treemacs--bookmark-handler "treemacs-bookmarks" "Open Treemacs into a bookmark RECORD.

(fn RECORD)") (autoload 'treemacs-add-bookmark "treemacs-bookmarks" "Add the current node to Emacs' list of bookmarks.
For file and directory nodes their absolute path is saved.  Tag nodes
additionally also save the tag's position.  A tag can only be bookmarked if the
treemacs node is pointing to a valid buffer position." t) (register-definition-prefixes "treemacs-bookmarks" '("treemacs--")) (register-definition-prefixes "treemacs-compatibility" '("treemacs-")) (register-definition-prefixes "treemacs-core-utils" '("treemacs-")) (register-definition-prefixes "treemacs-customization" '("treemacs-")) (register-definition-prefixes "treemacs-diagnostics" '("treemacs-")) (register-definition-prefixes "treemacs-dom" '("treemacs-")) (register-definition-prefixes "treemacs-extensions" '("treemacs-")) (autoload 'treemacs-delete-file "treemacs-file-management" "Delete node at point.
A delete action must always be confirmed.  Directories are deleted recursively.
By default files are deleted by moving them to the trash.  With a prefix ARG
they will instead be wiped irreversibly.

(fn &optional ARG)" t) (autoload 'treemacs-delete-marked-files "treemacs-file-management" "Delete all marked files.

A delete action must always be confirmed.  Directories are deleted recursively.
By default files are deleted by moving them to the trash.  With a prefix ARG
they will instead be wiped irreversibly.

For marking files see `treemacs-bulk-file-actions'.

(fn &optional ARG)" t) (autoload 'treemacs-move-file "treemacs-file-management" "Move file (or directory) at point.

If the selected target is an existing directory the source file will be directly
moved into this directory.  If the given target instead does not exist then it
will be treated as the moved file's new name, meaning the original source file
will be both moved and renamed." t) (autoload 'treemacs-copy-file "treemacs-file-management" "Copy file (or directory) at point.

If the selected target is an existing directory the source file will be directly
copied into this directory.  If the given target instead does not exist then it
will be treated as the copied file's new name, meaning the original source file
will be both copied and renamed." t) (autoload 'treemacs-move-marked-files "treemacs-file-management" "Move all marked files.

For marking files see `treemacs-bulk-file-actions'." t) (autoload 'treemacs-copy-marked-files "treemacs-file-management" "Copy all marked files.

For marking files see `treemacs-bulk-file-actions'." t) (autoload 'treemacs-rename-file "treemacs-file-management" "Rename the file/directory at point.

Buffers visiting the renamed file or visiting a file inside the renamed
directory and windows showing them will be reloaded.  The list of recent files
will likewise be updated." t) (autoload 'treemacs-show-marked-files "treemacs-file-management" "Print a list of all files marked by treemacs." t) (autoload 'treemacs-mark-or-unmark-path-at-point "treemacs-file-management" "Mark or unmark the absolute path of the node at point." t) (autoload 'treemacs-reset-marks "treemacs-file-management" "Unmark all previously marked files in the current buffer." t) (autoload 'treemacs-delete-marked-paths "treemacs-file-management" "Delete all previously marked files." t) (autoload 'treemacs-bulk-file-actions "treemacs-file-management" "Activate the bulk file actions hydra.
This interface allows to quickly (unmark) files, so as to copy, move or delete
them in bulk.

Note that marking files is *permanent*, files will stay marked until they are
either manually unmarked or deleted.  You can show a list of all currently
marked files with `treemacs-show-marked-files' or `s' in the hydra." t) (autoload 'treemacs-create-file "treemacs-file-management" "Create a new file.
Enter first the directory to create the new file in, then the new file's name.
The pre-selection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using $HOME when there is no path at or near point to grab." t) (autoload 'treemacs-create-dir "treemacs-file-management" "Create a new directory.
Enter first the directory to create the new dir in, then the new dir's name.
The pre-selection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using $HOME when there is no path at or near point to grab." t) (register-definition-prefixes "treemacs-file-management" '("treemacs-")) (register-definition-prefixes "treemacs-filewatch-mode" '("treemacs-")) (register-definition-prefixes "treemacs-follow-mode" '("treemacs-")) (register-definition-prefixes "treemacs-fringe-indicator" '("treemacs-")) (defvar treemacs-git-commit-diff-mode nil "Non-nil if Treemacs-Git-Commit-Diff mode is enabled.
See the `treemacs-git-commit-diff-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-git-commit-diff-mode'.") (custom-autoload 'treemacs-git-commit-diff-mode "treemacs-git-commit-diff-mode" nil) (autoload 'treemacs-git-commit-diff-mode "treemacs-git-commit-diff-mode" "Minor mode to display commit differences for your git-tracked projects.

When enabled treemacs will add an annotation next to every git project showing
how many commits ahead or behind your current branch is compared to its remote
counterpart.

The difference will be shown using the format `↑x ↓y', where `x' and `y' are the
numbers of commits a project is ahead or behind.  The numbers are determined
based on the output of `git status -sb'.

By default the annotation is only updated when manually updating a project with
`treemacs-refresh'.  You can install `treemacs-magit' to enable automatic
updates whenever you commit/fetch/rebase etc. in magit.

Does not require `treemacs-git-mode' to be active.

This is a global minor mode.  If called interactively, toggle the
`Treemacs-Git-Commit-Diff mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='treemacs-git-commit-diff-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "treemacs-git-commit-diff-mode" '("treemacs--")) (defvar treemacs-indicate-top-scroll-mode nil "Non-nil if Treemacs-Indicate-Top-Scroll mode is enabled.
See the `treemacs-indicate-top-scroll-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-indicate-top-scroll-mode'.") (custom-autoload 'treemacs-indicate-top-scroll-mode "treemacs-header-line" nil) (autoload 'treemacs-indicate-top-scroll-mode "treemacs-header-line" "Minor mode which shows whether treemacs is scrolled all the way to the top.

When this mode is enabled the header line of the treemacs window will display
whether the window's first line is visible or not.

The strings used for the display are determined by
`treemacs-header-scroll-indicators'.

This mode makes use of `treemacs-user-header-line-format' - and thus
`header-line-format' - and is therefore incompatible with other modifications to
these options.

This is a global minor mode.  If called interactively, toggle the
`Treemacs-Indicate-Top-Scroll mode' mode.  If the prefix argument
is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='treemacs-indicate-top-scroll-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "treemacs-header-line" '("treemacs-")) (autoload 'treemacs-common-helpful-hydra "treemacs-hydras" "Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the most commonly used keybinds for treemacs.  For the more
advanced (probably rarely used keybinds) see `treemacs-advanced-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead." t) (autoload 'treemacs-advanced-helpful-hydra "treemacs-hydras" "Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the more advanced (rarely used) keybinds for treemacs.  For
the more commonly used keybinds see `treemacs-common-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead." t) (register-definition-prefixes "treemacs-hydras" '("treemacs-helpful-hydra")) (autoload 'treemacs-resize-icons "treemacs-icons" "Resize the current theme's icons to the given SIZE.

If SIZE is \\='nil' the icons are not resized and will retain their default size
of 22 pixels.

There is only one size, the icons are square and the aspect ratio will be
preserved when resizing them therefore width and height are the same.

Resizing the icons only works if Emacs was built with ImageMagick support, or if
using Emacs >= 27.1,which has native image resizing support.  If this is not the
case this function will not have any effect.

Custom icons are not taken into account, only the size of treemacs' own icons
png are changed.

(fn SIZE)" t) (autoload 'treemacs-define-custom-icon "treemacs-icons" "Define a custom ICON for the current theme to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period.  This makes it possible to match file names like
\\='.gitignore' and \\='Makefile'.

Additionally FILE-EXTENSIONS are also not case sensitive and will be stored in a
down-cased state.

(fn ICON &rest FILE-EXTENSIONS)") (autoload 'treemacs-define-custom-image-icon "treemacs-icons" "Same as `treemacs-define-custom-icon' but for image icons instead of strings.
FILE is the path to an icon image (and not the actual icon string).
FILE-EXTENSIONS are all the (not case-sensitive) file extensions the icon
should be used for.

(fn FILE &rest FILE-EXTENSIONS)") (autoload 'treemacs-map-icons-with-auto-mode-alist "treemacs-icons" "Remaps icons for EXTENSIONS according to `auto-mode-alist'.
EXTENSIONS should be a list of file extensions such that they match the regex
stored in `auto-mode-alist', for example \\='(\".cc\").
MODE-ICON-ALIST is an alist that maps which mode from `auto-mode-alist' should
be assigned which treemacs icon, for example
`((c-mode . ,(treemacs-get-icon-value \"c\"))
  (c++-mode . ,(treemacs-get-icon-value \"cpp\")))

(fn EXTENSIONS MODE-ICON-ALIST)") (register-definition-prefixes "treemacs-icons" '("treemacs-")) (register-definition-prefixes "treemacs-interface" '("treemacs-")) (register-definition-prefixes "treemacs-logging" '("treemacs-")) (register-definition-prefixes "treemacs-macros" '("treemacs-")) (autoload 'treemacs-mode "treemacs-mode" "A major mode for displaying the file system in a tree layout.

(fn)" t) (register-definition-prefixes "treemacs-mode" '("treemacs-")) (autoload 'treemacs-leftclick-action "treemacs-mouse-interface" "Move focus to the clicked line.
Must be bound to a mouse click, or EVENT will not be supplied.

(fn EVENT)" t) (autoload 'treemacs-doubleclick-action "treemacs-mouse-interface" "Run the appropriate double-click action for the current node.
In the default configuration this means to expand/collapse directories and open
files and tags in the most recently used window.

This function's exact configuration is stored in
`treemacs-doubleclick-actions-config'.

Must be bound to a mouse double click to properly handle a click EVENT.

(fn EVENT)" t) (autoload 'treemacs-single-click-expand-action "treemacs-mouse-interface" "A modified single-leftclick action that expands the clicked nodes.
Can be bound to <mouse1> if you prefer to expand nodes with a single click
instead of a double click.  Either way it must be bound to a mouse click, or
EVENT will not be supplied.

Clicking on icons will expand a file's tags, just like
`treemacs-leftclick-action'.

(fn EVENT)" t) (autoload 'treemacs-dragleftclick-action "treemacs-mouse-interface" "Drag a file/dir node to be opened in a window.
Must be bound to a mouse click, or EVENT will not be supplied.

(fn EVENT)" t) (autoload 'treemacs-define-doubleclick-action "treemacs-mouse-interface" "Define the behaviour of `treemacs-doubleclick-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.

The list of possible states can be found in `treemacs-valid-button-states'.
ACTION should be one of the `treemacs-visit-node-*' commands.

(fn STATE ACTION)") (autoload 'treemacs-node-buffer-and-position "treemacs-mouse-interface" "Return source buffer or list of buffer and position for the current node.
This information can be used for future display.  Stay in the selected window
and ignore any prefix argument.

(fn &optional _)" t) (autoload 'treemacs-rightclick-menu "treemacs-mouse-interface" "Show a contextual right click menu based on click EVENT.

(fn EVENT)" t) (register-definition-prefixes "treemacs-mouse-interface" '("treemacs--")) (defvar treemacs-peek-mode nil "Non-nil if Treemacs-Peek mode is enabled.
See the `treemacs-peek-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-peek-mode'.") (custom-autoload 'treemacs-peek-mode "treemacs-peek-mode" nil) (autoload 'treemacs-peek-mode "treemacs-peek-mode" "Minor mode that allows you to peek at buffers before deciding to open them.

While the mode is active treemacs will automatically display the file at point,
without leaving the treemacs window.

Peeking will stop when you leave the treemacs window, be it through a command
like `treemacs-RET-action' or some other window selection change.

Files' buffers that have been opened for peeking will be cleaned up if they did
not exist before peeking started.

The peeked window can be scrolled using
`treemacs-next/previous-line-other-window' and
`treemacs-next/previous-page-other-window'

This is a global minor mode.  If called interactively, toggle the
`Treemacs-Peek mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='treemacs-peek-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "treemacs-peek-mode" '("treemacs--")) (register-definition-prefixes "treemacs-persistence" '("treemacs-")) (defvar treemacs-project-follow-mode nil "Non-nil if Treemacs-Project-Follow mode is enabled.
See the `treemacs-project-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-project-follow-mode'.") (custom-autoload 'treemacs-project-follow-mode "treemacs-project-follow-mode" nil) (autoload 'treemacs-project-follow-mode "treemacs-project-follow-mode" "Toggle `treemacs-only-current-project-mode'.

This is a minor mode meant for those who do not care about treemacs' workspace
features, or its preference to work with multiple projects simultaneously.  When
enabled it will function as an automated version of
`treemacs-display-current-project-exclusively', making sure that, after a small
idle delay, the current project, and *only* the current project, is displayed in
treemacs.

The project detection is based on the current buffer, and will try to determine
the project using the following methods, in the order they are listed:

- the current projectile.el project, if `treemacs-projectile' is installed
- the current project.el project
- the current `default-directory'

The update will only happen when treemacs is in the foreground, meaning a
treemacs window must exist in the current scope.

This mode requires at least Emacs version 27 since it relies on
`window-buffer-change-functions' and `window-selection-change-functions'.

This is a global minor mode.  If called interactively, toggle the
`Treemacs-Project-Follow mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='treemacs-project-follow-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "treemacs-project-follow-mode" '("treemacs--")) (register-definition-prefixes "treemacs-rendering" '("treemacs-")) (register-definition-prefixes "treemacs-scope" '("treemacs-")) (autoload 'treemacs--flatten&sort-imenu-index "treemacs-tag-follow-mode" "Flatten current file's imenu index and sort it by tag position.
The tags are sorted into the order in which they appear, regardless of section
or nesting depth.") (defvar treemacs-tag-follow-mode nil "Non-nil if Treemacs-Tag-Follow mode is enabled.
See the `treemacs-tag-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-tag-follow-mode'.") (custom-autoload 'treemacs-tag-follow-mode "treemacs-tag-follow-mode" nil) (autoload 'treemacs-tag-follow-mode "treemacs-tag-follow-mode" "Toggle `treemacs-tag-follow-mode'.

This acts as more fine-grained alternative to `treemacs-follow-mode' and will
thus disable `treemacs-follow-mode' on activation.  When enabled treemacs will
focus not only the file of the current buffer, but also the tag at point.

The follow action is attached to Emacs' idle timer and will run
`treemacs-tag-follow-delay' seconds of idle time.  The delay value is not an
integer, meaning it accepts floating point values like 1.5.

Every time a tag is followed a re--scan of the imenu index is forced by
temporarily setting `imenu-auto-rescan' to t (though a cache is applied as long
as the buffer is unmodified).  This is necessary to assure that creation or
deletion of tags does not lead to errors and guarantees an always up-to-date tag
view.

Note that in order to move to a tag in treemacs the treemacs buffer's window
needs to be temporarily selected, which will reset blink-cursor-mode's timer if
it is enabled.  This will result in the cursor blinking seemingly pausing for a
short time and giving the appearance of the tag follow action lasting much
longer than it really does.

This is a global minor mode.  If called interactively, toggle the
`Treemacs-Tag-Follow mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='treemacs-tag-follow-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "treemacs-tag-follow-mode" '("treemacs--")) (autoload 'treemacs--expand-file-node "treemacs-tags" "Open tag items for file BTN.
Recursively open all tags below BTN when RECURSIVE is non-nil.

(fn BTN &optional RECURSIVE)") (autoload 'treemacs--collapse-file-node "treemacs-tags" "Close node given by BTN.
Remove all open tag entries under BTN when RECURSIVE.

(fn BTN &optional RECURSIVE)") (autoload 'treemacs--visit-or-expand/collapse-tag-node "treemacs-tags" "Visit tag section BTN if possible, expand or collapse it otherwise.
Pass prefix ARG on to either visit or toggle action.

FIND-WINDOW is a special provision depending on this function's invocation
context and decides whether to find the window to display in (if the tag is
visited instead of the node being expanded).

On the one hand it can be called based on `treemacs-RET-actions-config' (or
TAB).  The functions in these configs are expected to find the windows they need
to display in themselves, so FIND-WINDOW must be t. On the other hand this
function is also called from the top level vist-node functions like
`treemacs-visit-node-vertical-split' which delegates to the
`treemacs--execute-button-action' macro which includes the determination of
the display window.

(fn BTN ARG FIND-WINDOW)") (autoload 'treemacs--expand-tag-node "treemacs-tags" "Open tags node items for BTN.
Open all tag section under BTN when call is RECURSIVE.

(fn BTN &optional RECURSIVE)") (autoload 'treemacs--collapse-tag-node "treemacs-tags" "Close tags node at BTN.
Remove all open tag entries under BTN when RECURSIVE.

(fn BTN &optional RECURSIVE)") (autoload 'treemacs--goto-tag "treemacs-tags" "Go to the tag at BTN.

(fn BTN)") (autoload 'treemacs--create-imenu-index-function "treemacs-tags" "The `imenu-create-index-function' for treemacs buffers.") (function-put 'treemacs--create-imenu-index-function 'side-effect-free 't) (register-definition-prefixes "treemacs-tags" '("treemacs--")) (register-definition-prefixes "treemacs-themes" '("treemacs-")) (register-definition-prefixes "treemacs-treelib" '("treemacs-")) (register-definition-prefixes "treemacs-visuals" '("treemacs-")) (register-definition-prefixes "treemacs-workspaces" '("treemacs-")) (provide 'treemacs-autoloads)) "duplicate-thing" ((duplicate-thing duplicate-thing-autoloads) (autoload 'duplicate-thing "duplicate-thing" "Duplicate line or region N times.
If it has active mark, it will expand the selection and duplicate it.
If it doesn't have active mark, it will select current line and duplicate it.

(fn N)" t) (register-definition-prefixes "duplicate-thing" '("duplicate-thing-")) (provide 'duplicate-thing-autoloads)) "breadcrumb" ((breadcrumb breadcrumb-pkg breadcrumb-autoloads) (autoload 'breadcrumb-imenu-crumbs "breadcrumb" "Describe point inside the Imenu tree of current file.") (autoload 'breadcrumb-project-crumbs "breadcrumb" "Describing the current file inside project.") (autoload 'breadcrumb-local-mode "breadcrumb" "Header lines with breadcrumbs.

This is a minor mode.  If called interactively, toggle the
`Breadcrumb-Local mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `breadcrumb-local-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'breadcrumb-mode 'globalized-minor-mode t) (defvar breadcrumb-mode nil "Non-nil if Breadcrumb mode is enabled.
See the `breadcrumb-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `breadcrumb-mode'.") (custom-autoload 'breadcrumb-mode "breadcrumb" nil) (autoload 'breadcrumb-mode "breadcrumb" "Toggle Bc-Local mode in all buffers.
With prefix ARG, enable Breadcrumb mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Bc-Local mode is enabled in all buffers where `bc--turn-on-local-mode-on-behalf-of-global-mode' would do it.

See `bc-local-mode' for more information on Bc-Local mode.

(fn &optional ARG)" t) (autoload 'breadcrumb-jump "breadcrumb" "Like \\[execute-extended-command] `imenu', but breadcrumb-powered." t) (register-definition-prefixes "breadcrumb" '("breadcrumb-")) (provide 'breadcrumb-autoloads)) "rmsbolt" ((rmsbolt rmsbolt-java rmsbolt-autoloads rmsbolt-split) (autoload 'rmsbolt-starter "rmsbolt" "Setup new file based on the sample for the language provided.

Uses LANG-NAME to determine the language.

(fn LANG-NAME)" t) (autoload 'rmsbolt-mode "rmsbolt" "Toggle `rmsbolt-mode'.

This mode is enabled in both src and assembly output buffers.

This is a minor mode.  If called interactively, toggle the
`Rmsbolt mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `rmsbolt-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'rmsbolt "rmsbolt" "Start a rmsbolt compilation and enable `rmsbolt-mode'.

Provides code region highlighting and automatic recompilation." t) (register-definition-prefixes "rmsbolt" '("rmsbolt-")) (register-definition-prefixes "rmsbolt-java" '("rmsbolt-java-")) (register-definition-prefixes "rmsbolt-split" '("rmsbolt-split-")) (provide 'rmsbolt-autoloads)) "parrot" ((parrot-autoloads parrot parrot-rotate) (defvar parrot-mode nil "Non-nil if Parrot mode is enabled.
See the `parrot-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `parrot-mode'.") (custom-autoload 'parrot-mode "parrot" nil) (autoload 'parrot-mode "parrot" "Use Parrot to show when you're rotating.

You can customize this minor mode, see option `parrot-mode'.

This is a global minor mode.  If called interactively, toggle the
`Parrot mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='parrot-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "parrot" '("parrot-")) (register-definition-prefixes "parrot-rotate" '("parrot-rotate-" "pulse-flag")) (provide 'parrot-autoloads)) "php-mode" ((php-complete php-defs php-mode-autoloads php php-local-manual php-ide php-align php-mode-debug php-face php-flymake php-ide-phpactor php-format php-project php-mode) (let ((loads (get 'php 'custom-loads))) (if (member '"php" loads) nil (put 'php 'custom-loads (cons '"php" loads)) (put 'languages 'custom-loads (cons 'php (get 'languages 'custom-loads))))) (autoload 'php-base-mode "php" "Generic major mode for editing PHP.

This mode is intended to be inherited by concrete major modes.
Currently there are `php-mode' and `php-ts-mode'.

(fn)" t) (autoload 'php-mode-maybe "php" "Select PHP mode or other major mode." t) (autoload 'php-current-class "php" "Insert current class name if cursor in class context." t) (autoload 'php-current-namespace "php" "Insert current namespace if cursor in namespace context." t) (autoload 'php-copyit-fqsen "php" "Copy/kill class/method FQSEN." t) (autoload 'php-run-builtin-web-server "php" "Run PHP Built-in web server.

`ROUTER-OR-DIR': Path to router PHP script or Document root.
`HOSTNAME': Hostname or IP address of Built-in web server.
`PORT': Port number of Built-in web server.
`DOCUMENT-ROOT': Path to Document root.

When `DOCUMENT-ROOT' is NIL, the document root is obtained from `ROUTER-OR-DIR'.

(fn ROUTER-OR-DIR HOSTNAME PORT &optional DOCUMENT-ROOT)" t) (autoload 'php-find-system-php-ini-file "php" "Find php.ini FILE by `php --ini'.

(fn &optional FILE)" t) (register-definition-prefixes "php" '("php-")) (autoload 'php-align-setup "php-align" "Setup alignment configuration for PHP code.") (autoload 'php-align-mode "php-align" "Alignment lines for PHP script.

This is a minor mode.  If called interactively, toggle the
`Php-Align mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `php-align-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "php-align" '("php-align-")) (let ((loads (get 'php-complete 'custom-loads))) (if (member '"php-complete" loads) nil (put 'php-complete 'custom-loads (cons '"php-complete" loads)) (put 'php-mode 'custom-loads (cons 'php-complete (get 'php-mode 'custom-loads))))) (defvar php-complete-function-modules '(bcmath core gmp libxml intl mbstring pcntl posix sodium xml xmlwriter) "Module names for function names completion.") (custom-autoload 'php-complete-function-modules "php-complete" t) (put 'php-complete-function-modules 'safe-local-variable (lambda (value) (and (listp value) (cl-loop for v in values always (assq v php-defs-functions-alist))))) (autoload 'php-complete-complete-function "php-complete" "Complete PHP keyword at point.

If INTERACTIVE is nil the function acts like a capf.

(fn &optional INTERACTIVE)" t) (register-definition-prefixes "php-complete" '("php-complete--")) (register-definition-prefixes "php-defs" '("php-defs-functions-alist")) (let ((loads (get 'php-faces 'custom-loads))) (if (member '"php-face" loads) nil (put 'php-faces 'custom-loads (cons '"php-face" loads)) (put 'php-mode 'custom-loads (cons 'php-faces (get 'php-mode 'custom-loads))))) (register-definition-prefixes "php-face" '("php-")) (autoload 'php-flymake "php-flymake" "Flymake backend for PHP syntax check.

See `flymake-diagnostic-functions' about REPORT-FN and ARGS parameters.

(fn REPORT-FN &rest ARGS)") (register-definition-prefixes "php-flymake" '("php-flymake-")) (autoload 'php-format-this-buffer-file "php-format" "Apply format this buffer file." t) (autoload 'php-format-project "php-format" "Apply format this buffer file." t) (autoload 'php-format-on-after-save-hook "php-format" "Apply format on after save hook.") (autoload 'php-format-auto-mode "php-format" "Automatically apply formatting when saving an edited file.

This is a minor mode.  If called interactively, toggle the
`Php-Format-Auto mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `php-format-auto-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "php-format" '("php-format-")) (defvar php-ide-features nil "A set of PHP-IDE features symbol.") (custom-autoload 'php-ide-features "php-ide" t) (put 'php-ide-features 'safe-local-variable (lambda (v) (cl-loop for feature in (if (listp v) v (list v)) always (symbolp feature)))) (defvar php-ide-eglot-executable nil "Command name or path to the command of Eglot LSP executable.") (custom-autoload 'php-ide-eglot-executable "php-ide" t) (put 'php-ide-eglot-executable 'safe-local-variable (lambda (v) (cond ((stringp v) (file-exists-p v)) ((listp v) (cl-every #'stringp v)) ((assq v php-ide-lsp-command-alist))))) (autoload 'php-ide-eglot-server-program "php-ide" "Return a list of command to execute LSP Server.") (defvar php-ide-mode-functions nil "Hook functions called when before activating or deactivating PHP-IDE.
Notice that two arguments (FEATURE ACTIVATE) are given.

FEATURE: A symbol, like \\='lsp-mode.
ACTIVATE: T is given when activeting, NIL when deactivating PHP-IDE.") (custom-autoload 'php-ide-mode-functions "php-ide" t) (put 'php-ide-mode-functions 'safe-local-variable (lambda (functions) (and (listp functions) (cl-every #'functionp functions)))) (autoload 'php-ide-mode "php-ide" "Minor mode for integrate IDE-like tools.

This is a minor mode.  If called interactively, toggle the
`Php-Ide mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `php-ide-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'php-ide-turn-on "php-ide" "Turn on PHP IDE-FEATURES and execute `php-ide-mode'.") (register-definition-prefixes "php-ide" '("php-ide-")) (autoload 'php-ide-phpactor-activate "php-ide-phpactor" "Activate PHP-IDE using phpactor.el." t) (autoload 'php-ide-phpactor-deactivate "php-ide-phpactor" "Dectivate PHP-IDE using phpactor.el." t) (register-definition-prefixes "php-ide-phpactor" '("php-ide-phpactor-")) (autoload 'php-local-manual-search "php-local-manual" "Search the local PHP documentation (i.e. in `php-manual-path') for
the word at point.  The function returns t if the requested documentation
exists, and nil otherwise.

With a prefix argument, prompt (with completion) for a word to search for.

(fn WORD)" t) (define-obsolete-function-alias 'php-search-local-documentation #'php-local-manual-search "2.0.0") (register-definition-prefixes "php-local-manual" '("php-local-manual-")) (let ((loads (get 'php-mode 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php-mode 'custom-loads (cons '"php-mode" loads)) (put 'languages 'custom-loads (cons 'php-mode (get 'languages 'custom-loads))))) (add-to-list 'interpreter-mode-alist (cons "php\\(?:-?[34578]\\(?:\\.[0-9]+\\)*\\)?" 'php-mode)) (define-obsolete-variable-alias 'php-available-project-root-files 'php-project-available-root-files "1.19.0") (autoload 'php-mode "php-mode" "Major mode for editing PHP code.

\\{php-mode-map}

(fn)" t) (add-to-list 'auto-mode-alist '("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-mode)) (add-to-list 'auto-mode-alist '("\\.\\(?:php\\.inc\\|stub\\)\\'" . php-mode)) (add-to-list 'auto-mode-alist '("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-mode-maybe)) (register-definition-prefixes "php-mode" '("php-")) (register-definition-prefixes "php-mode-debug" '("php-mode-debug")) (defvar-local php-project-root 'auto "Method of searching for the top level directory.

`auto' (default)
      Try to search file in order of `php-project-available-root-files'.

SYMBOL
      Key of `php-project-available-root-files'.

STRING
      A file/directory name of top level marker.
      If the string is an actual directory path, it is set as the absolute path
      of the root directory, not the marker.") (put 'php-project-root 'safe-local-variable #'(lambda (v) (or (stringp v) (assq v php-project-available-root-files)))) (defvar-local php-project-etags-file nil) (put 'php-project-etags-file 'safe-local-variable #'(lambda (v) (or (functionp v) (eq v t) (php-project--eval-bootstrap-scripts v)))) (defvar-local php-project-bootstrap-scripts nil "List of path to bootstrap php script file.

The ideal bootstrap file is silent, it only includes dependent files,
defines constants, and sets the class loaders.") (put 'php-project-bootstrap-scripts 'safe-local-variable #'php-project--eval-bootstrap-scripts) (defvar-local php-project-php-executable nil "Path to php executable file.") (put 'php-project-php-executable 'safe-local-variable #'(lambda (v) (and (stringp v) (file-executable-p v)))) (defvar-local php-project-coding-style nil "Symbol value of the coding style of the project that PHP major mode refers to.

Typically it is `pear', `drupal', `wordpress', `symfony2' and `psr2'.") (put 'php-project-coding-style 'safe-local-variable #'symbolp) (defvar-local php-project-align-lines t "If T, automatically turn on `php-align-mode' by `php-align-setup'.") (put 'php-project-align-lines 'safe-local-variable #'booleanp) (defvar-local php-project-php-file-as-template 'auto "
`auto' (default)
      Automatically switch to mode for template when HTML tag detected in file.

`t'
      Switch all PHP files in that directory to mode for HTML template.

`nil'
      Any .php  in that directory is just a PHP script.

((PATTERN . SYMBOL))
      Alist of file name pattern regular expressions and the above symbol pairs.
      PATTERN is regexp pattern.
") (put 'php-project-php-file-as-template 'safe-local-variable #'php-project--validate-php-file-as-template) (defvar-local php-project-repl nil "Function name or path to REPL (interactive shell) script.") (put 'php-project-repl 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v)))) (defvar-local php-project-unit-test nil "Function name or path to unit test script.") (put 'php-project-unit-test 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v)))) (defvar-local php-project-deploy nil "Function name or path to deploy script.") (put 'php-project-deploy 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v)))) (defvar-local php-project-build nil "Function name or path to build script.") (put 'php-project-build 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v)))) (defvar-local php-project-server-start nil "Function name or path to server-start script.") (put 'php-project-server-start 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v)))) (autoload 'php-project-get-bootstrap-scripts "php-project" "Return list of bootstrap script.") (autoload 'php-project-get-root-dir "php-project" "Return path to current PHP project.") (autoload 'php-project-project-find-function "php-project" "Return path to current PHP project from DIR.

This function is compatible with `project-find-functions'.

(fn DIR)") (register-definition-prefixes "php-project" '("php-project-")) (provide 'php-mode-autoloads)) "yaml-mode" ((yaml-mode yaml-mode-autoloads) (let ((loads (get 'yaml 'custom-loads))) (if (member '"yaml-mode" loads) nil (put 'yaml 'custom-loads (cons '"yaml-mode" loads)) (put 'languages 'custom-loads (cons 'yaml (get 'languages 'custom-loads))))) (autoload 'yaml-mode "yaml-mode" "Simple mode to edit YAML.

\\{yaml-mode-map}

(fn)" t) (add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)) (add-to-list 'magic-mode-alist '("^%YAML\\s-+[0-9]+\\.[0-9]+\\(\\s-+#\\|\\s-*$\\)" . yaml-mode)) (register-definition-prefixes "yaml-mode" '("yaml-")) (provide 'yaml-mode-autoloads)) "json-reformat" ((json-reformat-autoloads json-reformat) (autoload 'json-reformat-region "json-reformat" "Reformat the JSON in the specified region.

If you want to customize the reformat style,
please see the documentation of `json-reformat:indent-width'
and `json-reformat:pretty-string?'.

(fn BEGIN END)" t) (register-definition-prefixes "json-reformat" '("json-reformat")) (provide 'json-reformat-autoloads)) "flymake-diagnostic-at-point" ((flymake-diagnostic-at-point flymake-diagnostic-at-point-autoloads) (autoload 'flymake-diagnostic-at-point-set-timer "flymake-diagnostic-at-point" "Set the error display timer for the current buffer." t) (autoload 'flymake-diagnostic-at-point-cancel-timer "flymake-diagnostic-at-point" "Cancel the error display timer for the current buffer." t) (register-definition-prefixes "flymake-diagnostic-at-point" '("flymake-diagnostic-at-point-")) (provide 'flymake-diagnostic-at-point-autoloads)) "magit-section" ((magit-section magit-section-autoloads) (autoload 'magit-add-section-hook "magit-section" "Add to the value of section hook HOOK the function FUNCTION.

Add FUNCTION at the beginning of the hook list unless optional
APPEND is non-nil, in which case FUNCTION is added at the end.
If FUNCTION already is a member, then move it to the new location.

If optional AT is non-nil and a member of the hook list, then
add FUNCTION next to that instead.  Add before or after AT, or
replace AT with FUNCTION depending on APPEND.  If APPEND is the
symbol `replace', then replace AT with FUNCTION.  For any other
non-nil value place FUNCTION right after AT.  If nil, then place
FUNCTION right before AT.  If FUNCTION already is a member of the
list but AT is not, then leave FUNCTION where ever it already is.

If optional LOCAL is non-nil, then modify the hook's buffer-local
value rather than its global value.  This makes the hook local by
copying the default value.  That copy is then modified.

HOOK should be a symbol.  If HOOK is void, it is first set to nil.
HOOK's value must not be a single hook function.  FUNCTION should
be a function that takes no arguments and inserts one or multiple
sections at point, moving point forward.  FUNCTION may choose not
to insert its section(s), when doing so would not make sense.  It
should not be abused for other side-effects.  To remove FUNCTION
again use `remove-hook'.

(fn HOOK FUNCTION &optional AT APPEND LOCAL)") (autoload 'magit--handle-bookmark "magit-section" "Open a bookmark created by `magit--make-bookmark'.

Call the generic function `magit-bookmark-get-buffer-create' to get
the appropriate buffer without displaying it.

Then call the `magit-*-setup-buffer' function of the the major-mode
with the variables' values as arguments, which were recorded by
`magit--make-bookmark'.

(fn BOOKMARK)") (register-definition-prefixes "magit-section" '("context-menu-region" "isearch-clean-overlays" "magit-")) (provide 'magit-section-autoloads)) "magit" ((git-commit magit-patch magit-repos magit-status magit-log magit-files magit-margin magit-transient magit-gitignore magit-fetch magit-commit magit-diff magit-reset magit-bundle magit-reflog magit-merge magit magit-sequence magit-refs magit-autoloads magit-subtree magit-tag git-rebase magit-base magit-submodule magit-bisect magit-extras magit-worktree magit-blame magit-apply magit-sparse-checkout magit-clone magit-core magit-pull magit-notes magit-branch magit-bookmark magit-autorevert magit-push magit-stash magit-process magit-ediff magit-remote magit-mode magit-wip magit-git) (put 'git-commit-major-mode 'safe-local-variable (lambda (val) (memq val '(text-mode markdown-mode org-mode fundamental-mode git-commit-elisp-text-mode)))) (register-definition-prefixes "git-commit" '("git-commit-" "global-git-commit-mode")) (autoload 'git-rebase-current-line "git-rebase" "Parse current line into a `git-rebase-action' instance.
If the current line isn't recognized as a rebase line, an
instance with all nil values is returned.") (autoload 'git-rebase-mode "git-rebase" "Major mode for editing of a Git rebase file.

Rebase files are generated when you run \"git rebase -i\" or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running \"man git-rebase\" at the command line) for details.

(fn)" t) (defconst git-rebase-filename-regexp "/git-rebase-todo\\'") (add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp #'git-rebase-mode)) (register-definition-prefixes "git-rebase" '("git-rebase-" "magit-imenu--rebase-")) (defvar magit-define-global-key-bindings 'default "Which set of key bindings to add to the global keymap, if any.

This option controls which set of Magit key bindings, if any, may
be added to the global keymap, even before Magit is first used in
the current Emacs session.

If the value is nil, no bindings are added.

If `default', maybe add:

    C-x g     `magit-status'
    C-x M-g   `magit-dispatch'
    C-c M-g   `magit-file-dispatch'

If `recommended', maybe add:

    C-x g     `magit-status'
    C-c g     `magit-dispatch'
    C-c f     `magit-file-dispatch'

    These bindings are strongly recommended, but we cannot use
    them by default, because the \"C-c <LETTER>\" namespace is
    strictly reserved for bindings added by the user.

The bindings in the chosen set may be added when
`after-init-hook' is run.  Each binding is added if, and only
if, at that time no other key is bound to the same command,
and no other command is bound to the same key.  In other words
we try to avoid adding bindings that are unnecessary, as well
as bindings that conflict with other bindings.

Adding these bindings is delayed until `after-init-hook' is
run to allow users to set the variable anywhere in their init
file (without having to make sure to do so before `magit' is
loaded or autoloaded) and to increase the likelihood that all
the potentially conflicting user bindings have already been
added.

To set this variable use either `setq' or the Custom interface.
Do not use the function `customize-set-variable' because doing
that would cause Magit to be loaded immediately, when that form
is evaluated (this differs from `custom-set-variables', which
doesn't load the libraries that define the customized variables).

Setting this variable has no effect if `after-init-hook' has
already been run.") (custom-autoload 'magit-define-global-key-bindings "magit" t) (defun magit-maybe-define-global-key-bindings (&optional force) "See variable `magit-define-global-key-bindings'." (when magit-define-global-key-bindings (let ((map (current-global-map))) (pcase-dolist (`(,key \, def) (cond ((eq magit-define-global-key-bindings 'recommended) '(("C-x g" . magit-status) ("C-c g" . magit-dispatch) ("C-c f" . magit-file-dispatch))) ('(("C-x g" . magit-status) ("C-x M-g" . magit-dispatch) ("C-c M-g" . magit-file-dispatch))))) (when (or force (not (or (lookup-key map (kbd key)) (where-is-internal def (make-sparse-keymap) t)))) (define-key map (kbd key) def)))))) (if after-init-time (magit-maybe-define-global-key-bindings) (add-hook 'after-init-hook #'magit-maybe-define-global-key-bindings t)) (autoload 'magit-dispatch "magit" nil t) (autoload 'magit-run "magit" nil t) (autoload 'magit-git-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t) (autoload 'magit-git-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree.

(fn COMMAND)" t) (autoload 'magit-shell-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t) (autoload 'magit-shell-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree.

(fn COMMAND)" t) (autoload 'magit-version "magit" "Return the version of Magit currently in use.

If optional argument PRINT-DEST is non-nil, also print the used
versions of Magit, Transient, Git and Emacs to the output stream
selected by that argument.  Interactively use the echo area, or
with a prefix argument use the current buffer.  Additionally put
the output in the kill ring.

(fn &optional PRINT-DEST)" t) (register-definition-prefixes "magit" '("magit-")) (autoload 'magit-stage-buffer-file "magit-apply" "Stage all changes to the file being visited in the current buffer." t) (autoload 'magit-stage-file "magit-apply" "Read one or more files and stage all changes in those files.
With prefix argument FORCE, offer ignored files for completion.

(fn FILES &optional FORCE)" t) (autoload 'magit-stage-modified "magit-apply" "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.

(fn &optional ALL)" t) (autoload 'magit-unstage-buffer-file "magit-apply" "Unstage all changes to the file being visited in the current buffer." t) (autoload 'magit-unstage-file "magit-apply" "Read one or more files and unstage all changes to those files.

(fn FILES)" t) (autoload 'magit-unstage-all "magit-apply" "Remove all changes from the staging area." t) (register-definition-prefixes "magit-apply" '("magit-")) (put 'magit-auto-revert-mode 'globalized-minor-mode t) (defvar magit-auto-revert-mode (not (or global-auto-revert-mode noninteractive)) "Non-nil if Magit-Auto-Revert mode is enabled.
See the `magit-auto-revert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-auto-revert-mode'.") (custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil) (autoload 'magit-auto-revert-mode "magit-autorevert" "Toggle Auto-Revert mode in all buffers.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Auto-Revert mode is enabled in all buffers where `magit-turn-on-auto-revert-mode-if-desired' would do it.

See `auto-revert-mode' for more information on Auto-Revert mode.

(fn &optional ARG)" t) (register-definition-prefixes "magit-autorevert" '("auto-revert-buffer" "magit-")) (autoload 'magit-emacs-Q-command "magit-base" "Show a shell command that runs an uncustomized Emacs with only Magit loaded.
See info node `(magit)Debugging Tools' for more information." t) (define-advice Info-follow-nearest-node (:around (fn &optional fork) gitman) (let ((node (Info-get-token (point) "\\*note[ 
	]+" "\\*note[ 
	]+\\([^:]*\\):\\(:\\|[ 
	]*(\\)?"))) (if (and node (string-match "^(gitman)\\(.+\\)" node)) (pcase magit-view-git-manual-method ('info (funcall fn fork)) ('man (require 'man) (man (match-string 1 node))) ('woman (require 'woman) (woman (match-string 1 node))) (_ (user-error "Invalid value for `magit-view-git-manual-method'"))) (funcall fn fork)))) (define-advice org-man-export (:around (fn link description format) gitman) (if (and (eq format 'texinfo) (string-prefix-p "git" link)) (string-replace "%s" link "
@ifinfo
@ref{%s,,,gitman,}.
@end ifinfo
@ifhtml
@html
the <a href=\"http://git-scm.com/docs/%s\">%s(1)</a> manpage.
@end html
@end ifhtml
@iftex
the %s(1) manpage.
@end iftex
") (funcall fn link description format))) (register-definition-prefixes "magit-base" '("magit-")) (autoload 'magit-bisect "magit-bisect" nil t) (autoload 'magit-bisect-start "magit-bisect" "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a known
good and a known bad commit.  To move the session forward use the
other actions from the bisect transient command (\\<magit-status-mode-map>\\[magit-bisect]).

(fn BAD GOOD ARGS)" t) (autoload 'magit-bisect-reset "magit-bisect" "After bisecting, cleanup bisection state and return to original `HEAD'." t) (autoload 'magit-bisect-good "magit-bisect" "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question." t) (autoload 'magit-bisect-bad "magit-bisect" "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question." t) (autoload 'magit-bisect-mark "magit-bisect" "While bisecting, mark the current commit with a bisect term.
During a bisect using alternate terms, commits can still be
marked with `magit-bisect-good' and `magit-bisect-bad', as those
commands map to the correct term (\"good\" to --term-old's value
and \"bad\" to --term-new's).  However, in some cases, it can be
difficult to keep that mapping straight in your head; this
command provides an interface that exposes the underlying terms." t) (autoload 'magit-bisect-skip "magit-bisect" "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one." t) (autoload 'magit-bisect-run "magit-bisect" "Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'.

(fn CMDLINE &optional BAD GOOD ARGS)" t) (register-definition-prefixes "magit-bisect" '("magit-")) (autoload 'magit-blame-echo "magit-blame" nil t) (autoload 'magit-blame-addition "magit-blame" nil t) (autoload 'magit-blame-removal "magit-blame" nil t) (autoload 'magit-blame-reverse "magit-blame" nil t) (autoload 'magit-blame "magit-blame" nil t) (register-definition-prefixes "magit-blame" '("magit-")) (autoload 'magit-branch "magit" nil t) (autoload 'magit-checkout "magit-branch" "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch, then that becomes the current
branch.  If it is something else, then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

(git checkout REVISION).

(fn REVISION &optional ARGS)" t) (function-put 'magit-checkout 'interactive-only 'magit--checkout) (autoload 'magit-branch-create "magit-branch" "Create BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT)" t) (function-put 'magit-branch-create 'interactive-only 'magit-call-git) (autoload 'magit-branch-and-checkout "magit-branch" "Create and checkout BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT &optional ARGS)" t) (function-put 'magit-branch-and-checkout 'interactive-only 'magit-call-git) (autoload 'magit-branch-or-checkout "magit-branch" "Hybrid between `magit-checkout' and `magit-branch-and-checkout'.

Ask the user for an existing branch or revision.  If the user
input actually can be resolved as a branch or revision, then
check that out, just like `magit-checkout' would.

Otherwise create and checkout a new branch using the input as
its name.  Before doing so read the starting-point for the new
branch.  This is similar to what `magit-branch-and-checkout'
does.

(fn ARG &optional START-POINT)" t) (function-put 'magit-branch-or-checkout 'interactive-only 'magit-call-git) (autoload 'magit-branch-checkout "magit-branch" "Checkout an existing or new local branch.

Read a branch name from the user offering all local branches and
a subset of remote branches as candidates.  Omit remote branches
for which a local branch by the same name exists from the list
of candidates.  The user can also enter a completely new branch
name.

- If the user selects an existing local branch, then check that
  out.

- If the user selects a remote branch, then create and checkout
  a new local branch with the same name.  Configure the selected
  remote branch as push target.

- If the user enters a new branch name, then create and check
  that out, after also reading the starting-point from the user.

In the latter two cases the upstream is also set.  Whether it is
set to the chosen START-POINT or something else depends on the
value of `magit-branch-adjust-remote-upstream-alist', just like
when using `magit-branch-and-checkout'.

(fn BRANCH &optional START-POINT)" t) (function-put 'magit-branch-checkout 'interactive-only 'magit-call-git) (autoload 'magit-branch-orphan "magit-branch" "Create and checkout an orphan BRANCH with contents from revision START-POINT.

(fn BRANCH START-POINT)" t) (autoload 'magit-branch-spinout "magit-branch" "Create new branch from the unpushed commits.
Like `magit-branch-spinoff' but remain on the current branch.
If there are any uncommitted changes, then behave exactly like
`magit-branch-spinoff'.

(fn BRANCH &optional FROM)" t) (autoload 'magit-branch-spinoff "magit-branch" "Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself.

If optional FROM is non-nil, then the source branch is reset
to `FROM~', instead of to the last commit it shares with its
upstream.  Interactively, FROM is only ever non-nil, if the
region selects some commits, and among those commits, FROM is
the commit that is the fewest commits ahead of the source
branch.

The commit at the other end of the selection actually does not
matter, all commits between FROM and `HEAD' are moved to the new
branch.  If FROM is not reachable from `HEAD' or is reachable
from the source branch's upstream, then an error is raised.

(fn BRANCH &optional FROM)" t) (autoload 'magit-branch-reset "magit-branch" "Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirm the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset.

(fn BRANCH TO &optional SET-UPSTREAM)" t) (autoload 'magit-branch-delete "magit-branch" "Delete one or multiple branches.

If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

Require confirmation when deleting branches is dangerous in some
way.  Option `magit-no-confirm' can be customized to not require
confirmation in certain cases.  See its docstring to learn why
confirmation is required by default in certain cases or if a
prompt is confusing.

(fn BRANCHES &optional FORCE)" t) (autoload 'magit-branch-rename "magit-branch" "Rename the branch named OLD to NEW.

With a prefix argument FORCE, rename even if a branch named NEW
already exists.

If `branch.OLD.pushRemote' is set, then unset it.  Depending on
the value of `magit-branch-rename-push-target' (which see) maybe
set `branch.NEW.pushRemote' and maybe rename the push-target on
the remote.

(fn OLD NEW &optional FORCE)" t) (autoload 'magit-branch-shelve "magit-branch" "Shelve a BRANCH.
Rename \"refs/heads/BRANCH\" to \"refs/shelved/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t) (autoload 'magit-branch-unshelve "magit-branch" "Unshelve a BRANCH
Rename \"refs/shelved/BRANCH\" to \"refs/heads/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t) (autoload 'magit-branch-configure "magit-branch" nil t) (register-definition-prefixes "magit-branch" '("magit-")) (autoload 'magit-bundle "magit-bundle" nil t) (autoload 'magit-bundle-import "magit-bundle" nil t) (autoload 'magit-bundle-create-tracked "magit-bundle" "Create and track a new bundle.

(fn FILE TAG BRANCH REFS ARGS)" t) (autoload 'magit-bundle-update-tracked "magit-bundle" "Update a bundle that is being tracked using TAG.

(fn TAG)" t) (autoload 'magit-bundle-verify "magit-bundle" "Check whether FILE is valid and applies to the current repository.

(fn FILE)" t) (autoload 'magit-bundle-list-heads "magit-bundle" "List the refs in FILE.

(fn FILE)" t) (register-definition-prefixes "magit-bundle" '("magit-")) (autoload 'magit-clone "magit-clone" nil t) (autoload 'magit-clone-regular "magit-clone" "Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t) (autoload 'magit-clone-shallow "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
With a prefix argument read the DEPTH of the clone;
otherwise use 1.

(fn REPOSITORY DIRECTORY ARGS DEPTH)" t) (autoload 'magit-clone-shallow-since "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits before DATE, which is read from the
user.

(fn REPOSITORY DIRECTORY ARGS DATE)" t) (autoload 'magit-clone-shallow-exclude "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits reachable from EXCLUDE, which is a
branch or tag read from the user.

(fn REPOSITORY DIRECTORY ARGS EXCLUDE)" t) (autoload 'magit-clone-bare "magit-clone" "Create a bare clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t) (autoload 'magit-clone-mirror "magit-clone" "Create a mirror of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t) (autoload 'magit-clone-sparse "magit-clone" "Clone REPOSITORY into DIRECTORY and create a sparse checkout.

(fn REPOSITORY DIRECTORY ARGS)" t) (register-definition-prefixes "magit-clone" '("magit-")) (autoload 'magit-commit "magit-commit" nil t) (autoload 'magit-commit-create "magit-commit" "Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.

(git commit [--amend] ARGS)

(fn &optional ARGS)" t) (autoload 'magit-commit-amend "magit-commit" "Amend the last commit.

(git commit --amend ARGS)

(fn &optional ARGS)" t) (autoload 'magit-commit-extend "magit-commit" "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.

(git commit --amend --no-edit)

(fn &optional ARGS OVERRIDE-DATE)" t) (autoload 'magit-commit-reword "magit-commit" "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

(git commit --amend --only)

(fn &optional ARGS OVERRIDE-DATE)" t) (autoload 'magit-commit-fixup "magit-commit" "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-squash "magit-commit" "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

If you want to immediately add a message to the squash commit,
then use `magit-commit-augment' instead of this command.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-augment "magit-commit" "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-instant-fixup "magit-commit" "Create a fixup commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-instant-squash "magit-commit" "Create a squash commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t) (autoload 'magit-commit-reshelve "magit-commit" "Change the committer date and possibly the author date of `HEAD'.

The current time is used as the initial minibuffer input and the
original author or committer date is available as the previous
history element.

Both the author and the committer dates are changed, unless one
of the following is true, in which case only the committer date
is updated:
- You are not the author of the commit that is being reshelved.
- The command was invoked with a prefix argument.
- Non-interactively if UPDATE-AUTHOR is nil.

(fn DATE UPDATE-AUTHOR &optional ARGS)" t) (autoload 'magit-commit-absorb-modules "magit-commit" "Spread modified modules across recent commits.

(fn PHASE COMMIT)" t) (autoload 'magit-commit-absorb "magit-commit" nil t) (autoload 'magit-commit-autofixup "magit-commit" nil t) (register-definition-prefixes "magit-commit" '("magit-")) (autoload 'magit-diff "magit-diff" nil t) (autoload 'magit-diff-refresh "magit-diff" nil t) (autoload 'magit-diff-dwim "magit-diff" "Show changes for the thing at point.

(fn &optional ARGS FILES)" t) (autoload 'magit-diff-range "magit-diff" "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range).

(fn REV-OR-RANGE &optional ARGS FILES)" t) (autoload 'magit-diff-working-tree "magit-diff" "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t) (autoload 'magit-diff-staged "magit-diff" "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t) (autoload 'magit-diff-unstaged "magit-diff" "Show changes between the working tree and the index.

(fn &optional ARGS FILES)" t) (autoload 'magit-diff-unmerged "magit-diff" "Show changes that are being merged.

(fn &optional ARGS FILES)" t) (autoload 'magit-diff-while-committing "magit-diff" "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed." t) (autoload 'magit-diff-buffer-file "magit-diff" "Show diff for the blob or file visited in the current buffer.

When the buffer visits a blob, then show the respective commit.
When the buffer visits a file, then show the differences between
`HEAD' and the working tree.  In both cases limit the diff to
the file or blob." t) (autoload 'magit-diff-paths "magit-diff" "Show changes between any two files on disk.

(fn A B)" t) (autoload 'magit-show-commit "magit-diff" "Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

(fn REV &optional ARGS FILES MODULE)" t) (register-definition-prefixes "magit-diff" '("magit-")) (autoload 'magit-ediff "magit-ediff" nil) (autoload 'magit-ediff-resolve-all "magit-ediff" "Resolve all conflicts in the FILE at point using Ediff.

If there is no file at point or if it doesn't have any unmerged
changes, then prompt for a file.

See info node `(magit) Ediffing' for more information about this
and alternative commands.

(fn FILE)" t) (autoload 'magit-ediff-resolve-rest "magit-ediff" "Resolve outstanding conflicts in the FILE at point using Ediff.

If there is no file at point or if it doesn't have any unmerged
changes, then prompt for a file.

See info node `(magit) Ediffing' for more information about this
and alternative commands.

(fn FILE)" t) (autoload 'magit-ediff-stage "magit-ediff" "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-compare "magit-ediff" "Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range).

(fn REVA REVB FILEA FILEB)" t) (autoload 'magit-ediff-dwim "magit-ediff" "Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run." t) (autoload 'magit-ediff-show-staged "magit-ediff" "Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-show-unstaged "magit-ediff" "Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-show-working-tree "magit-ediff" "Show changes between `HEAD' and working tree using Ediff.
FILE must be relative to the top directory of the repository.

(fn FILE)" t) (autoload 'magit-ediff-show-commit "magit-ediff" "Show changes introduced by COMMIT using Ediff.

(fn COMMIT)" t) (autoload 'magit-ediff-show-stash "magit-ediff" "Show changes introduced by STASH using Ediff.
`magit-ediff-show-stash-with-index' controls whether a
three-buffer Ediff is used in order to distinguish changes in the
stash that were staged.

(fn STASH)" t) (register-definition-prefixes "magit-ediff" '("magit-ediff-")) (autoload 'magit-git-mergetool "magit-extras" nil t) (autoload 'magit-run-git-gui-blame "magit-extras" "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

(fn COMMIT FILENAME &optional LINENUM)" t) (autoload 'magit-run-git-gui "magit-extras" "Run `git gui' for the current git repository." t) (autoload 'magit-run-gitk "magit-extras" "Run `gitk' in the current repository." t) (autoload 'magit-run-gitk-branches "magit-extras" "Run `gitk --branches' in the current repository." t) (autoload 'magit-run-gitk-all "magit-extras" "Run `gitk --all' in the current repository." t) (autoload 'ido-enter-magit-status "magit-extras" "Drop into `magit-status' from file switching.

To make this command available use something like:

  (keymap-set ido-common-completion-map
              \"C-x g\" \\='ido-enter-magit-status)

This command does not work in Emacs 26.1.
See https://github.com/magit/magit/issues/3634
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707." t) (autoload 'magit-project-status "magit-extras" "Run `magit-status' in the current project's root." t) (autoload 'magit-dired-jump "magit-extras" "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'.

(fn &optional OTHER-WINDOW)" t) (autoload 'magit-dired-log "magit-extras" "Show log for all marked files, or the current file.

(fn &optional FOLLOW)" t) (autoload 'magit-dired-am-apply-patches "magit-extras" "In Dired, apply the marked (or next ARG) files as patches.
If inside a repository, then apply in that.  Otherwise prompt
for a repository.

(fn REPO &optional ARG)" t) (autoload 'magit-do-async-shell-command "magit-extras" "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point.

(fn FILE)" t) (autoload 'magit-previous-line "magit-extras" "Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t) (function-put 'magit-previous-line 'interactive-only '"use `forward-line' with negative argument instead.") (autoload 'magit-next-line "magit-extras" "Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t) (function-put 'magit-next-line 'interactive-only 'forward-line) (autoload 'magit-clean "magit-extras" "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

(git clean -f -d [-x|-X])

(fn &optional ARG)" t) (autoload 'magit-generate-changelog "magit-extras" "Insert ChangeLog entries into the current buffer.

The entries are generated from the diff being committed.
If prefix argument, AMENDING, is non-nil, include changes
in HEAD as well as staged changes in the diff to check.

(fn &optional AMENDING)" t) (autoload 'magit-add-change-log-entry "magit-extras" "Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t) (autoload 'magit-add-change-log-entry-other-window "magit-extras" "Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME)" t) (autoload 'magit-edit-line-commit "magit-extras" "Edit the commit that added the current line.

With a prefix argument edit the commit that removes the line,
if any.  The commit is determined using `git blame' and made
editable using `git rebase --interactive' if it is reachable
from `HEAD', or by checking out the commit (or a branch that
points at it) otherwise.

(fn &optional TYPE)" t) (autoload 'magit-diff-edit-hunk-commit "magit-extras" "From a hunk, edit the respective commit and visit the file.

First visit the file being modified by the hunk at the correct
location using `magit-diff-visit-file'.  This actually visits a
blob.  When point is on a diff header, not within an individual
hunk, then this visits the blob the first hunk is about.

Then invoke `magit-edit-line-commit', which uses an interactive
rebase to make the commit editable, or if that is not possible
because the commit is not reachable from `HEAD' by checking out
that commit directly.  This also causes the actual worktree file
to be visited.

Neither the blob nor the file buffer are killed when finishing
the rebase.  If that is undesirable, then it might be better to
use `magit-rebase-edit-commit' instead of this command.

(fn FILE)" t) (autoload 'magit-reshelve-since "magit-extras" "Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  Then read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history.

If KEYID is non-nil, then use that to sign all reshelved commits.
Interactively use the value of the \"--gpg-sign\" option in the
list returned by `magit-rebase-arguments'.

(fn REV KEYID)" t) (autoload 'magit-pop-revision-stack "magit-extras" "Insert a representation of a revision into the current buffer.

Pop a revision from the `magit-revision-stack' and insert it into
the current buffer according to `magit-pop-revision-stack-format'.
Revisions can be put on the stack using `magit-copy-section-value'
and `magit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g., while composing a commit
message), then that repository is used.  Otherwise (e.g., while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too.

(fn REV TOPLEVEL)" t) (autoload 'magit-copy-section-value "magit-extras" "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `magit-revision-stack' for use
with `magit-pop-revision-stack'.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.  If a prefix argument is used and the region is within
a hunk, then strip the diff marker column and keep only either
the added or removed lines, depending on the sign of the prefix
argument.

(fn ARG)" t) (autoload 'magit-copy-buffer-revision "magit-extras" "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `magit-revision-stack'.

This command is mainly intended for use in `magit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Magit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `magit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'." t) (autoload 'magit-display-repository-buffer "magit-extras" "Display a Magit buffer belonging to the current Git repository.
The buffer is displayed using `magit-display-buffer', which see.

(fn BUFFER)" t) (autoload 'magit-switch-to-repository-buffer "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t) (autoload 'magit-switch-to-repository-buffer-other-window "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t) (autoload 'magit-switch-to-repository-buffer-other-frame "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t) (autoload 'magit-abort-dwim "magit-extras" "Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect." t) (autoload 'magit-back-to-indentation "magit-extras" "Move point to the first non-whitespace character on this line.
In Magit diffs, also skip over - and + at the beginning of the line." t) (register-definition-prefixes "magit-extras" '("magit-")) (autoload 'magit-fetch "magit-fetch" nil t) (autoload 'magit-fetch-from-pushremote "magit-fetch" nil t) (autoload 'magit-fetch-from-upstream "magit-fetch" nil t) (autoload 'magit-fetch-other "magit-fetch" "Fetch from another repository.

(fn REMOTE ARGS)" t) (autoload 'magit-fetch-branch "magit-fetch" "Fetch a BRANCH from a REMOTE.

(fn REMOTE BRANCH ARGS)" t) (autoload 'magit-fetch-refspec "magit-fetch" "Fetch a REFSPEC from a REMOTE.

(fn REMOTE REFSPEC ARGS)" t) (autoload 'magit-fetch-all "magit-fetch" "Fetch from all remotes.

(fn ARGS)" t) (autoload 'magit-fetch-all-prune "magit-fetch" "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote." t) (autoload 'magit-fetch-all-no-prune "magit-fetch" "Fetch from all remotes." t) (autoload 'magit-fetch-modules "magit-fetch" nil t) (register-definition-prefixes "magit-fetch" '("magit-")) (autoload 'magit-find-file "magit-files" "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location.

(fn REV FILE)" t) (autoload 'magit-find-file-other-window "magit-files" "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t) (autoload 'magit-find-file-other-frame "magit-files" "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t) (autoload 'magit-file-dispatch "magit" nil t) (autoload 'magit-blob-visit-file "magit-files" "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree." t) (autoload 'magit-file-checkout "magit-files" "Checkout FILE from REV.

(fn REV FILE)" t) (register-definition-prefixes "magit-files" '("lsp" "magit-")) (register-definition-prefixes "magit-git" '("magit-")) (autoload 'magit-gitignore "magit-gitignore" nil t) (autoload 'magit-gitignore-in-topdir "magit-gitignore" "Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file.

(fn RULE)" t) (autoload 'magit-gitignore-in-subdir "magit-gitignore" "Add the Git ignore RULE to a \".gitignore\" file in DIRECTORY.
Prompt the user for a directory and add the rule to the
\".gitignore\" file in that directory.  Since such files are
tracked, they are shared with other clones of the repository.
Also stage the file.

(fn RULE DIRECTORY)" t) (autoload 'magit-gitignore-in-gitdir "magit-gitignore" "Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository.

(fn RULE)" t) (autoload 'magit-gitignore-on-system "magit-gitignore" "Add the Git ignore RULE to the file specified by `core.excludesFile'.
Rules that are defined in that file affect all local repositories.

(fn RULE)" t) (autoload 'magit-skip-worktree "magit-gitignore" "Call \"git update-index --skip-worktree -- FILE\".

(fn FILE)" t) (autoload 'magit-no-skip-worktree "magit-gitignore" "Call \"git update-index --no-skip-worktree -- FILE\".

(fn FILE)" t) (autoload 'magit-assume-unchanged "magit-gitignore" "Call \"git update-index --assume-unchanged -- FILE\".

(fn FILE)" t) (autoload 'magit-no-assume-unchanged "magit-gitignore" "Call \"git update-index --no-assume-unchanged -- FILE\".

(fn FILE)" t) (register-definition-prefixes "magit-gitignore" '("magit-")) (autoload 'magit-log "magit-log" nil t) (autoload 'magit-log-refresh "magit-log" nil t) (autoload 'magit-log-current "magit-log" "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer.

(fn REVS &optional ARGS FILES)" t) (autoload 'magit-log-head "magit-log" "Show log for `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-related "magit-log" "Show log for the current branch, its upstream and its push target.
When the upstream is a local branch, then also show its own
upstream.  When `HEAD' is detached, then show log for that, the
previously checked out branch and its upstream and push-target.

(fn REVS &optional ARGS FILES)" t) (autoload 'magit-log-other "magit-log" "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates.

(fn REVS &optional ARGS FILES)" t) (autoload 'magit-log-branches "magit-log" "Show log for all local branches and `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-matching-branches "magit-log" "Show log for all branches matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t) (autoload 'magit-log-matching-tags "magit-log" "Show log for all tags matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t) (autoload 'magit-log-all-branches "magit-log" "Show log for all local and remote branches and `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-all "magit-log" "Show log for all references and `HEAD'.

(fn &optional ARGS FILES)" t) (autoload 'magit-log-buffer-file "magit-log" "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is an active log
argument, then follow renames.  When the region is active,
restrict the log to the lines that the region touches.

(fn &optional FOLLOW BEG END)" t) (autoload 'magit-log-trace-definition "magit-log" "Show log for the definition at point.

(fn FILE FN REV)" t) (autoload 'magit-log-merged "magit-log" "Show log for the merge of COMMIT into BRANCH.

More precisely, find merge commit M that brought COMMIT into
BRANCH, and show the log of the range \"M^1..M\". If COMMIT is
directly on BRANCH, then show approximately
`magit-log-merged-commit-count' surrounding commits instead.

This command requires git-when-merged, which is available from
https://github.com/mhagger/git-when-merged.

(fn COMMIT BRANCH &optional ARGS FILES)" t) (autoload 'magit-log-move-to-parent "magit-log" "Move to the Nth parent of the current commit.

(fn &optional N)" t) (autoload 'magit-shortlog "magit-log" nil t) (autoload 'magit-shortlog-since "magit-log" "Show a history summary for commits since REV.

(fn REV ARGS)" t) (autoload 'magit-shortlog-range "magit-log" "Show a history summary for commit or range REV-OR-RANGE.

(fn REV-OR-RANGE ARGS)" t) (autoload 'magit-cherry "magit-log" "Show commits in a branch that are not merged in the upstream branch.

(fn HEAD UPSTREAM)" t) (register-definition-prefixes "magit-log" '("magit-")) (register-definition-prefixes "magit-margin" '("magit-")) (autoload 'magit-merge "magit" nil t) (autoload 'magit-merge-plain "magit-merge" "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

(git merge --no-edit|--no-commit [ARGS] REV)

(fn REV &optional ARGS NOCOMMIT)" t) (autoload 'magit-merge-editmsg "magit-merge" "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

(git merge --edit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t) (autoload 'magit-merge-nocommit "magit-merge" "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

(git merge --no-commit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t) (autoload 'magit-merge-into "magit-merge" "Merge the current branch into BRANCH and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t) (autoload 'magit-merge-absorb "magit-merge" "Merge BRANCH into the current branch and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t) (autoload 'magit-merge-squash "magit-merge" "Squash commit REV into the current branch; don't create a commit.

(git merge --squash REV)

(fn REV)" t) (autoload 'magit-merge-preview "magit-merge" "Preview result of merging REV into the current branch.

(fn REV)" t) (autoload 'magit-merge-abort "magit-merge" "Abort the current merge operation.

(git merge --abort)" t) (register-definition-prefixes "magit-merge" '("magit-")) (autoload 'magit-info "magit-mode" "Visit the Magit manual." t) (register-definition-prefixes "magit-mode" '("magit-")) (autoload 'magit-notes "magit" nil t) (register-definition-prefixes "magit-notes" '("magit-notes-")) (autoload 'magit-patch "magit-patch" nil t) (autoload 'magit-patch-create "magit-patch" nil t) (autoload 'magit-patch-apply "magit-patch" nil t) (autoload 'magit-patch-save "magit-patch" "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used.

(fn FILE &optional ARG)" t) (autoload 'magit-request-pull "magit-patch" "Request upstream to pull from your public repository.

URL is the url of your publicly accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit.

(fn URL START END)" t) (register-definition-prefixes "magit-patch" '("magit-")) (register-definition-prefixes "magit-process" '("magit-")) (autoload 'magit-pull "magit-pull" nil t) (autoload 'magit-pull-from-pushremote "magit-pull" nil t) (autoload 'magit-pull-from-upstream "magit-pull" nil t) (autoload 'magit-pull-branch "magit-pull" "Pull from a branch read in the minibuffer.

(fn SOURCE ARGS)" t) (register-definition-prefixes "magit-pull" '("magit-pull-")) (autoload 'magit-push "magit-push" nil t) (autoload 'magit-push-current-to-pushremote "magit-push" nil t) (autoload 'magit-push-current-to-upstream "magit-push" nil t) (autoload 'magit-push-current "magit-push" "Push the current branch to a branch read in the minibuffer.

(fn TARGET ARGS)" t) (autoload 'magit-push-other "magit-push" "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer.

(fn SOURCE TARGET ARGS)" t) (autoload 'magit-push-refspecs "magit-push" "Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used.

(fn REMOTE REFSPECS ARGS)" t) (autoload 'magit-push-matching "magit-push" "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation.

(fn REMOTE &optional ARGS)" t) (autoload 'magit-push-tags "magit-push" "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default.

(fn REMOTE &optional ARGS)" t) (autoload 'magit-push-tag "magit-push" "Push a tag to another repository.

(fn TAG REMOTE &optional ARGS)" t) (autoload 'magit-push-notes-ref "magit-push" "Push a notes ref to another repository.

(fn REF REMOTE &optional ARGS)" t) (autoload 'magit-push-implicitly "magit-push" nil t) (autoload 'magit-push-to-remote "magit-push" nil t) (register-definition-prefixes "magit-push" '("magit-")) (autoload 'magit-reflog-current "magit-reflog" "Display the reflog of the current branch.
If `HEAD' is detached, then show the reflog for that instead." t) (autoload 'magit-reflog-other "magit-reflog" "Display the reflog of a branch or another ref.

(fn REF)" t) (autoload 'magit-reflog-head "magit-reflog" "Display the `HEAD' reflog." t) (register-definition-prefixes "magit-reflog" '("magit-reflog-")) (autoload 'magit-show-refs "magit-refs" nil t) (autoload 'magit-show-refs-head "magit-refs" "List and compare references in a dedicated buffer.
Compared with `HEAD'.

(fn &optional ARGS)" t) (autoload 'magit-show-refs-current "magit-refs" "List and compare references in a dedicated buffer.
Compare with the current branch or `HEAD' if it is detached.

(fn &optional ARGS)" t) (autoload 'magit-show-refs-other "magit-refs" "List and compare references in a dedicated buffer.
Compared with a branch read from the user.

(fn &optional REF ARGS)" t) (register-definition-prefixes "magit-refs" '("magit-")) (autoload 'magit-remote "magit-remote" nil t) (autoload 'magit-remote-add "magit-remote" "Add a remote named REMOTE and fetch it.

(fn REMOTE URL &optional ARGS)" t) (autoload 'magit-remote-rename "magit-remote" "Rename the remote named OLD to NEW.

(fn OLD NEW)" t) (autoload 'magit-remote-remove "magit-remote" "Delete the remote named REMOTE.

(fn REMOTE)" t) (autoload 'magit-remote-prune "magit-remote" "Remove stale remote-tracking branches for REMOTE.

(fn REMOTE)" t) (autoload 'magit-remote-prune-refspecs "magit-remote" "Remove stale refspecs for REMOTE.

A refspec is stale if there no longer exists at least one branch
on the remote that would be fetched due to that refspec.  A stale
refspec is problematic because its existence causes Git to refuse
to fetch according to the remaining non-stale refspecs.

If only stale refspecs remain, then offer to either delete the
remote or to replace the stale refspecs with the default refspec.

Also remove the remote-tracking branches that were created due to
the now stale refspecs.  Other stale branches are not removed.

(fn REMOTE)" t) (autoload 'magit-remote-set-head "magit-remote" "Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that.

(fn REMOTE &optional BRANCH)" t) (autoload 'magit-remote-unset-head "magit-remote" "Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\".

(fn REMOTE)" t) (autoload 'magit-update-default-branch "magit-remote" nil t) (autoload 'magit-remote-unshallow "magit-remote" "Convert a shallow remote into a full one.
If only a single refspec is set and it does not contain a
wildcard, then also offer to replace it with the standard
refspec.

(fn REMOTE)" t) (autoload 'magit-remote-configure "magit-remote" nil t) (register-definition-prefixes "magit-remote" '("magit-")) (autoload 'magit-list-repositories "magit-repos" "Display a list of repositories.

Use the option `magit-repository-directories' to control which
repositories are displayed." t) (register-definition-prefixes "magit-repos" '("magit-")) (autoload 'magit-reset "magit" nil t) (autoload 'magit-reset-mixed "magit-reset" "Reset the `HEAD' and index to COMMIT, but not the working tree.

(git reset --mixed COMMIT)

(fn COMMIT)" t) (autoload 'magit-reset-soft "magit-reset" "Reset the `HEAD' to COMMIT, but not the index and working tree.

(git reset --soft REVISION)

(fn COMMIT)" t) (autoload 'magit-reset-hard "magit-reset" "Reset the `HEAD', index, and working tree to COMMIT.

(git reset --hard REVISION)

(fn COMMIT)" t) (autoload 'magit-reset-keep "magit-reset" "Reset the `HEAD' and index to COMMIT, while keeping uncommitted changes.

(git reset --keep REVISION)

(fn COMMIT)" t) (autoload 'magit-reset-index "magit-reset" "Reset the index to COMMIT.
Keep the `HEAD' and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.

(git reset COMMIT .)

(fn COMMIT)" t) (autoload 'magit-reset-worktree "magit-reset" "Reset the worktree to COMMIT.
Keep the `HEAD' and index as-is.

(fn COMMIT)" t) (autoload 'magit-reset-quickly "magit-reset" "Reset the `HEAD' and index to COMMIT, and possibly the working tree.
With a prefix argument reset the working tree otherwise don't.

(git reset --mixed|--hard COMMIT)

(fn COMMIT &optional HARD)" t) (register-definition-prefixes "magit-reset" '("magit-reset-")) (autoload 'magit-sequencer-continue "magit-sequence" "Resume the current cherry-pick or revert sequence." t) (autoload 'magit-sequencer-skip "magit-sequence" "Skip the stopped at commit during a cherry-pick or revert sequence." t) (autoload 'magit-sequencer-abort "magit-sequence" "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started." t) (autoload 'magit-cherry-pick "magit-sequence" nil t) (autoload 'magit-cherry-copy "magit-sequence" "Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting.

(fn COMMITS &optional ARGS)" t) (autoload 'magit-cherry-apply "magit-sequence" "Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting.

(fn COMMITS &optional ARGS)" t) (autoload 'magit-cherry-harvest "magit-sequence" "Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH &optional ARGS)" t) (autoload 'magit-cherry-donate "magit-sequence" "Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.  `HEAD' is allowed to be detached initially.

(fn COMMITS BRANCH &optional ARGS)" t) (autoload 'magit-cherry-spinout "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t) (autoload 'magit-cherry-spinoff "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t) (autoload 'magit-revert "magit-sequence" nil t) (autoload 'magit-revert-and-commit "magit-sequence" "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t) (autoload 'magit-revert-no-commit "magit-sequence" "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t) (autoload 'magit-am "magit-sequence" nil t) (autoload 'magit-am-apply-patches "magit-sequence" "Apply the patches FILES.

(fn &optional FILES ARGS)" t) (autoload 'magit-am-apply-maildir "magit-sequence" "Apply the patches from MAILDIR.

(fn &optional MAILDIR ARGS)" t) (autoload 'magit-am-continue "magit-sequence" "Resume the current patch applying sequence." t) (autoload 'magit-am-skip "magit-sequence" "Skip the stopped at patch during a patch applying sequence." t) (autoload 'magit-am-abort "magit-sequence" "Abort the current patch applying sequence.
This discards all changes made since the sequence started." t) (autoload 'magit-rebase "magit-sequence" nil t) (autoload 'magit-rebase-onto-pushremote "magit-sequence" nil t) (autoload 'magit-rebase-onto-upstream "magit-sequence" nil t) (autoload 'magit-rebase-branch "magit-sequence" "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased.

(fn TARGET ARGS)" t) (autoload 'magit-rebase-subset "magit-sequence" "Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits.

(fn NEWBASE START ARGS)" t) (autoload 'magit-rebase-interactive "magit-sequence" "Start an interactive rebase sequence.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-autosquash "magit-sequence" "Combine squash and fixup commits with their intended targets.

(fn ARGS)" t) (autoload 'magit-rebase-edit-commit "magit-sequence" "Edit a single older commit using rebase.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-reword-commit "magit-sequence" "Reword a single older commit using rebase.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-remove-commit "magit-sequence" "Remove a single older commit using rebase.

(fn COMMIT ARGS)" t) (autoload 'magit-rebase-continue "magit-sequence" "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is.

(fn &optional NOEDIT)" t) (autoload 'magit-rebase-skip "magit-sequence" "Skip the current commit and restart the current rebase operation." t) (autoload 'magit-rebase-edit "magit-sequence" "Edit the todo list of the current rebase operation." t) (autoload 'magit-rebase-abort "magit-sequence" "Abort the current rebase operation, restoring the original branch." t) (register-definition-prefixes "magit-sequence" '("magit-")) (autoload 'magit-sparse-checkout "magit-sparse-checkout" nil t) (autoload 'magit-sparse-checkout-enable "magit-sparse-checkout" "Convert the working tree to a sparse checkout.

(fn &optional ARGS)" t) (autoload 'magit-sparse-checkout-set "magit-sparse-checkout" "Restrict working tree to DIRECTORIES.
To extend rather than override the currently configured
directories, call `magit-sparse-checkout-add' instead.

(fn DIRECTORIES)" t) (autoload 'magit-sparse-checkout-add "magit-sparse-checkout" "Add DIRECTORIES to the working tree.
To override rather than extend the currently configured
directories, call `magit-sparse-checkout-set' instead.

(fn DIRECTORIES)" t) (autoload 'magit-sparse-checkout-reapply "magit-sparse-checkout" "Reapply the sparse checkout rules to the working tree.
Some operations such as merging or rebasing may need to check out
files that aren't included in the sparse checkout.  Call this
command to reset to the sparse checkout state." t) (autoload 'magit-sparse-checkout-disable "magit-sparse-checkout" "Convert sparse checkout to full checkout.
Note that disabling the sparse checkout does not clear the
configured directories.  Call `magit-sparse-checkout-enable' to
restore the previous sparse checkout." t) (register-definition-prefixes "magit-sparse-checkout" '("magit-sparse-checkout-")) (autoload 'magit-stash "magit-stash" nil t) (autoload 'magit-stash-both "magit-stash" "Create a stash of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-stash-index "magit-stash" "Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect.

(fn MESSAGE)" t) (autoload 'magit-stash-worktree "magit-stash" "Create a stash of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-stash-keep-index "magit-stash" "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-snapshot-both "magit-stash" "Create a snapshot of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-snapshot-index "magit-stash" "Create a snapshot of the index only.
Unstaged and untracked changes are not stashed." t) (autoload 'magit-snapshot-worktree "magit-stash" "Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t) (autoload 'magit-stash-push "magit-stash" nil t) (autoload 'magit-stash-apply "magit-stash" "Apply a stash to the working tree.

First try \"git stash apply --index\", which tries to preserve
the index stored in the stash, if any.  This may fail because
applying the stash could result in conflicts and those have to
be stored in the index, making it impossible to also store the
stash's index there as well.

If the above failed, then try \"git stash apply\".  This fails
(with or without \"--index\") if there are any uncommitted
changes to files that are also modified in the stash.

If both of the above failed, then apply using \"git apply\".
If there are no conflicting files, use \"--3way\".  If there are
conflicting files, then using \"--3way\" requires that those
files are staged first, which may be undesirable, so prompt
the user whether to use \"--3way\" or \"--reject\".

(fn STASH)" t) (autoload 'magit-stash-pop "magit-stash" "Apply a stash to the working tree, on success remove it from stash list.

First try \"git stash pop --index\", which tries to preserve
the index stored in the stash, if any.  This may fail because
applying the stash could result in conflicts and those have to
be stored in the index, making it impossible to also store the
stash's index there as well.

If the above failed, then try \"git stash apply\".  This fails
(with or without \"--index\") if there are any uncommitted
changes to files that are also modified in the stash.

If both of the above failed, then apply using \"git apply\".
If there are no conflicting files, use \"--3way\".  If there are
conflicting files, then using \"--3way\" requires that those
files are staged first, which may be undesirable, so prompt
the user whether to use \"--3way\" or \"--reject\".

(fn STASH)" t) (autoload 'magit-stash-drop "magit-stash" "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.

(fn STASH)" t) (autoload 'magit-stash-clear "magit-stash" "Remove all stashes saved in REF's reflog by deleting REF.

(fn REF)" t) (autoload 'magit-stash-branch "magit-stash" "Create and checkout a new BRANCH from an existing STASH.
The new branch starts at the commit that was current when the
stash was created.  If the stash applies cleanly, then drop it.

(fn STASH BRANCH)" t) (autoload 'magit-stash-branch-here "magit-stash" "Create and checkout a new BRANCH from an existing STASH.
Use the current branch or `HEAD' as the starting-point of BRANCH.
Then apply STASH, dropping it if it applies cleanly.

(fn STASH BRANCH)" t) (autoload 'magit-stash-format-patch "magit-stash" "Create a patch from STASH

(fn STASH)" t) (autoload 'magit-stash-list "magit-stash" "List all stashes in a buffer." t) (autoload 'magit-stash-show "magit-stash" "Show all diffs of a stash in a buffer.

(fn STASH &optional ARGS FILES)" t) (register-definition-prefixes "magit-stash" '("magit-")) (autoload 'magit-init "magit-status" "Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

(fn DIRECTORY)" t) (autoload 'magit-status "magit-status" "Show the status of the current Git repository in a buffer.

If the current directory isn't located within a Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `magit-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `magit-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `magit-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments.

(fn &optional DIRECTORY CACHE)" t) (defalias 'magit #'magit-status "Begin using Magit.

This alias for `magit-status' exists for better discoverability.

Instead of invoking this alias for `magit-status' using
\"M-x magit RET\", you should bind a key to `magit-status'
and read the info node `(magit)Getting Started', which
also contains other useful hints.") (autoload 'magit-status-here "magit-status" "Like `magit-status' but with non-nil `magit-status-goto-file-position'." t) (autoload 'magit-status-quick "magit-status" "Show the status of the current Git repository, maybe without refreshing.

If the status buffer of the current Git repository exists but
isn't being displayed in the selected frame, then display it
without refreshing it.

If the status buffer is being displayed in the selected frame,
then also refresh it.

Prefix arguments have the same meaning as for `magit-status',
and additionally cause the buffer to be refresh.

To use this function instead of `magit-status', add this to your
init file: (global-set-key (kbd \"C-x g\") \\='magit-status-quick)." t) (autoload 'magit-status-setup-buffer "magit-status" "

(fn &optional DIRECTORY)") (register-definition-prefixes "magit-status" '("magit-")) (autoload 'magit-submodule "magit-submodule" nil t) (autoload 'magit-submodule-add "magit-submodule" nil t) (autoload 'magit-submodule-read-name-for-path "magit-submodule" "

(fn PATH &optional PREFER-SHORT)") (autoload 'magit-submodule-register "magit-submodule" nil t) (autoload 'magit-submodule-populate "magit-submodule" nil t) (autoload 'magit-submodule-update "magit-submodule" nil t) (autoload 'magit-submodule-synchronize "magit-submodule" nil t) (autoload 'magit-submodule-unpopulate "magit-submodule" nil t) (autoload 'magit-submodule-remove "magit-submodule" "Unregister MODULES and remove their working directories.

For safety reasons, do not remove the gitdirs and if a module has
uncommitted changes, then do not remove it at all.  If a module's
gitdir is located inside the working directory, then move it into
the gitdir of the superproject first.

With the \"--force\" argument offer to remove dirty working
directories and with a prefix argument offer to delete gitdirs.
Both actions are very dangerous and have to be confirmed.  There
are additional safety precautions in place, so you might be able
to recover from making a mistake here, but don't count on it.

(fn MODULES ARGS TRASH-GITDIRS)" t) (autoload 'magit-insert-modules "magit-submodule" "Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-module-sections-nested' controls
whether they are wrapped in an additional section.") (autoload 'magit-insert-modules-overview "magit-submodule" "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash.") (autoload 'magit-insert-modules-unpulled-from-upstream "magit-submodule" "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits.") (autoload 'magit-insert-modules-unpulled-from-pushremote "magit-submodule" "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits.") (autoload 'magit-insert-modules-unpushed-to-upstream "magit-submodule" "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits.") (autoload 'magit-insert-modules-unpushed-to-pushremote "magit-submodule" "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits.") (autoload 'magit-list-submodules "magit-submodule" "Display a list of the current repository's populated submodules." t) (register-definition-prefixes "magit-submodule" '("magit-")) (autoload 'magit-subtree "magit-subtree" nil t) (autoload 'magit-subtree-import "magit-subtree" nil t) (autoload 'magit-subtree-export "magit-subtree" nil t) (autoload 'magit-subtree-add "magit-subtree" "Add REF from REPOSITORY as a new subtree at PREFIX.

(fn PREFIX REPOSITORY REF ARGS)" t) (autoload 'magit-subtree-add-commit "magit-subtree" "Add COMMIT as a new subtree at PREFIX.

(fn PREFIX COMMIT ARGS)" t) (autoload 'magit-subtree-merge "magit-subtree" "Merge COMMIT into the PREFIX subtree.

(fn PREFIX COMMIT ARGS)" t) (autoload 'magit-subtree-pull "magit-subtree" "Pull REF from REPOSITORY into the PREFIX subtree.

(fn PREFIX REPOSITORY REF ARGS)" t) (autoload 'magit-subtree-push "magit-subtree" "Extract the history of the subtree PREFIX and push it to REF on REPOSITORY.

(fn PREFIX REPOSITORY REF ARGS)" t) (autoload 'magit-subtree-split "magit-subtree" "Extract the history of the subtree PREFIX.

(fn PREFIX COMMIT ARGS)" t) (register-definition-prefixes "magit-subtree" '("magit-")) (autoload 'magit-tag "magit" nil t) (autoload 'magit-tag-create "magit-tag" "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

(git tag [--annotate] NAME REV)

(fn NAME REV &optional ARGS)" t) (autoload 'magit-tag-delete "magit-tag" "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

(git tag -d TAGS)

(fn TAGS)" t) (autoload 'magit-tag-prune "magit-tag" "Offer to delete tags missing locally from REMOTE, and vice versa.

(fn TAGS REMOTE-TAGS REMOTE)" t) (autoload 'magit-tag-release "magit-tag" "Create a release tag for `HEAD'.

Assume that release tags match `magit-release-tag-regexp'.

If `HEAD's message matches `magit-release-commit-regexp', then
base the tag on the version string specified by that.  Otherwise
prompt for the name of the new tag using the highest existing
tag as initial input and leaving it to the user to increment the
desired part of the version string.

When creating an annotated tag, prepare a message based on the message
of the highest existing tag, provided that contains the corresponding
version string, and substituting the new version string for that.  If
that is not the case, propose a message using a reasonable format.

(fn TAG MSG &optional ARGS)" t) (register-definition-prefixes "magit-tag" '("magit-")) (register-definition-prefixes "magit-transient" '("magit-")) (defvar magit-wip-mode nil "Non-nil if Magit-Wip mode is enabled.
See the `magit-wip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.") (custom-autoload 'magit-wip-mode "magit-wip" nil) (autoload 'magit-wip-mode "magit-wip" "Save uncommitted changes to work-in-progress refs.

Whenever appropriate (i.e., when dataloss would be a possibility
otherwise) this mode causes uncommitted changes to be committed
to dedicated work-in-progress refs.

For historic reasons this mode is implemented on top of four
other `magit-wip-*' modes, which can also be used individually,
if you want finer control over when the wip refs are updated;
but that is discouraged.

This is a global minor mode.  If called interactively, toggle the
`Magit-Wip mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='magit-wip-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'magit-wip-after-save-mode 'globalized-minor-mode t) (defvar magit-wip-after-save-mode nil "Non-nil if Magit-Wip-After-Save mode is enabled.
See the `magit-wip-after-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-after-save-mode'.") (custom-autoload 'magit-wip-after-save-mode "magit-wip" nil) (autoload 'magit-wip-after-save-mode "magit-wip" "Toggle Magit-Wip-After-Save-Local mode in all buffers.
With prefix ARG, enable Magit-Wip-After-Save mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Magit-Wip-After-Save-Local mode is enabled in all buffers where `magit-wip-after-save-local-mode-turn-on' would do it.

See `magit-wip-after-save-local-mode' for more information on Magit-Wip-After-Save-Local mode.

(fn &optional ARG)" t) (defvar magit-wip-after-apply-mode nil "Non-nil if Magit-Wip-After-Apply mode is enabled.
See the `magit-wip-after-apply-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-after-apply-mode "magit-wip" nil) (autoload 'magit-wip-after-apply-mode "magit-wip" "Commit to work-in-progress refs.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index.

This is a global minor mode.  If called interactively, toggle the
`Magit-Wip-After-Apply mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='magit-wip-after-apply-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (defvar magit-wip-before-change-mode nil "Non-nil if Magit-Wip-Before-Change mode is enabled.
See the `magit-wip-before-change-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-before-change-mode "magit-wip" nil) (autoload 'magit-wip-before-change-mode "magit-wip" "Commit to work-in-progress refs before certain destructive changes.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed.

This is a global minor mode.  If called interactively, toggle the
`Magit-Wip-Before-Change mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='magit-wip-before-change-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'magit-wip-commit-initial-backup "magit-wip" "Before saving, commit current file to a worktree wip ref.

The user has to add this function to `before-save-hook'.

Commit the current state of the visited file before saving the
current buffer to that file.  This backs up the same version of
the file as `backup-buffer' would, but stores the backup in the
worktree wip ref, which is also used by the various Magit Wip
modes, instead of in a backup file as `backup-buffer' would.

This function ignores the variables that affect `backup-buffer'
and can be used along-side that function, which is recommended
because this function only backs up files that are tracked in
a Git repository.") (register-definition-prefixes "magit-wip" '("magit-")) (autoload 'magit-worktree "magit-worktree" nil t) (autoload 'magit-worktree-checkout "magit-worktree" "Checkout BRANCH in a new worktree at PATH.

(fn PATH BRANCH)" t) (autoload 'magit-worktree-branch "magit-worktree" "Create a new BRANCH and check it out in a new worktree at PATH.

(fn PATH BRANCH START-POINT)" t) (autoload 'magit-worktree-move "magit-worktree" "Move WORKTREE to PATH.

(fn WORKTREE PATH)" t) (register-definition-prefixes "magit-worktree" '("magit-")) (provide 'magit-autoloads)) "magit-filenotify" ((magit-filenotify-autoloads magit-filenotify) (autoload 'magit-filenotify-mode "magit-filenotify" "Refresh status buffer if source tree changes.

This is a minor mode.  If called interactively, toggle the
`Magit-Filenotify mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `magit-filenotify-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "magit-filenotify" '("magit-filenotify-")) (provide 'magit-filenotify-autoloads)) "sqlite3" ((sqlite3-autoloads sqlite3) (register-definition-prefixes "sqlite3" '("sqlite3-api-build-command")) (provide 'sqlite3-autoloads)) "diff-hl" ((diff-hl-flydiff diff-hl-show-hunk diff-hl-dired diff-hl diff-hl-show-hunk-posframe diff-hl-inline-popup diff-hl-autoloads diff-hl-margin diff-hl-amend) (autoload 'diff-hl-mode "diff-hl" "Toggle VC diff highlighting.

This is a minor mode.  If called interactively, toggle the
`Diff-Hl mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `diff-hl-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'turn-on-diff-hl-mode "diff-hl" "Turn on `diff-hl-mode' or `diff-hl-dir-mode' in a buffer if appropriate.") (autoload 'diff-hl--global-turn-on "diff-hl" "Call `turn-on-diff-hl-mode' if the current major mode is applicable.") (autoload 'diff-hl-set-reference-rev "diff-hl" "Set the reference revision globally to REV.
When called interactively, REV read with completion.

The default value chosen using one of methods below:

- In a log view buffer, it uses the revision of current entry.
Call `vc-print-log' or `vc-print-root-log' first to open a log
view buffer.
- In a VC annotate buffer, it uses the revision of current line.
- In other situations, it uses the symbol at point.

Notice that this sets the reference revision globally, so in
files from other repositories, `diff-hl-mode' will not highlight
changes correctly, until you run `diff-hl-reset-reference-rev'.

Also notice that this will disable `diff-hl-amend-mode' in
buffers that enables it, since `diff-hl-amend-mode' overrides its
effect.

(fn REV)" t) (autoload 'diff-hl-reset-reference-rev "diff-hl" "Reset the reference revision globally to the most recent one." t) (put 'global-diff-hl-mode 'globalized-minor-mode t) (defvar global-diff-hl-mode nil "Non-nil if Global Diff-Hl mode is enabled.
See the `global-diff-hl-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-diff-hl-mode'.") (custom-autoload 'global-diff-hl-mode "diff-hl" nil) (autoload 'global-diff-hl-mode "diff-hl" "Toggle Diff-Hl mode in all buffers.
With prefix ARG, enable Global Diff-Hl mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Diff-Hl mode is enabled in all buffers where `diff-hl--global-turn-on' would do it.

See `diff-hl-mode' for more information on Diff-Hl mode.

(fn &optional ARG)" t) (register-definition-prefixes "diff-hl" '("diff-hl-")) (autoload 'diff-hl-amend-mode "diff-hl-amend" "Show changes against the second-last revision in `diff-hl-mode'.

Most useful with backends that support rewriting local commits,
and most importantly, \"amending\" the most recent one.
Currently only supports Git, Mercurial and Bazaar.

This is a minor mode.  If called interactively, toggle the
`Diff-Hl-Amend mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `diff-hl-amend-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-diff-hl-amend-mode 'globalized-minor-mode t) (defvar global-diff-hl-amend-mode nil "Non-nil if Global Diff-Hl-Amend mode is enabled.
See the `global-diff-hl-amend-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-diff-hl-amend-mode'.") (custom-autoload 'global-diff-hl-amend-mode "diff-hl-amend" nil) (autoload 'global-diff-hl-amend-mode "diff-hl-amend" "Toggle Diff-Hl-Amend mode in all buffers.
With prefix ARG, enable Global Diff-Hl-Amend mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Diff-Hl-Amend mode is enabled in all buffers where `turn-on-diff-hl-amend-mode' would do it.

See `diff-hl-amend-mode' for more information on Diff-Hl-Amend mode.

(fn &optional ARG)" t) (register-definition-prefixes "diff-hl-amend" '("diff-hl-amend-setup" "turn-on-diff-hl-amend-mode")) (autoload 'diff-hl-dired-mode "diff-hl-dired" "Toggle VC diff highlighting on the side of a Dired window.

This is a minor mode.  If called interactively, toggle the
`Diff-Hl-Dired mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `diff-hl-dired-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'diff-hl-dired-mode-unless-remote "diff-hl-dired") (register-definition-prefixes "diff-hl-dired" '("diff-hl-dired-")) (defvar diff-hl-flydiff-mode nil "Non-nil if Diff-Hl-Flydiff mode is enabled.
See the `diff-hl-flydiff-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `diff-hl-flydiff-mode'.") (custom-autoload 'diff-hl-flydiff-mode "diff-hl-flydiff" nil) (autoload 'diff-hl-flydiff-mode "diff-hl-flydiff" "Perform highlighting on-the-fly.

This is a global minor mode.  It alters how `diff-hl-mode' works.

This is a global minor mode.  If called interactively, toggle the
`Diff-Hl-Flydiff mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='diff-hl-flydiff-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "diff-hl-flydiff" '("diff-hl-flydiff")) (autoload 'diff-hl-inline-popup-hide "diff-hl-inline-popup" "Hide the current inline popup." t) (autoload 'diff-hl-inline-popup-show "diff-hl-inline-popup" "Create a phantom overlay to show the inline popup, with some
content LINES, and a HEADER and a FOOTER, at POINT.  KEYMAP is
added to the current keymaps.  CLOSE-HOOK is called when the popup
is closed.

(fn LINES &optional HEADER FOOTER KEYMAP CLOSE-HOOK POINT HEIGHT)") (register-definition-prefixes "diff-hl-inline-popup" '("diff-hl-inline-popup-")) (defvar diff-hl-margin-mode nil "Non-nil if Diff-Hl-Margin mode is enabled.
See the `diff-hl-margin-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `diff-hl-margin-mode'.") (custom-autoload 'diff-hl-margin-mode "diff-hl-margin" nil) (autoload 'diff-hl-margin-mode "diff-hl-margin" "Toggle displaying `diff-hl-mode' highlights on the margin.

This is a global minor mode.  If called interactively, toggle the
`Diff-Hl-Margin mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='diff-hl-margin-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'diff-hl-margin-local-mode "diff-hl-margin" "Toggle displaying `diff-hl-mode' highlights on the margin locally.

You probably shouldn't use this function directly.

This is a minor mode.  If called interactively, toggle the
`Diff-Hl-Margin-Local mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `diff-hl-margin-local-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "diff-hl-margin" '("diff-hl-")) (autoload 'diff-hl-show-hunk-inline-popup "diff-hl-show-hunk" "Implementation to show the hunk in a inline popup.
BUFFER is a buffer with the hunk.

(fn BUFFER &optional IGNORED-LINE)") (autoload 'diff-hl-show-hunk-previous "diff-hl-show-hunk" "Go to previous hunk/change and show it." t) (autoload 'diff-hl-show-hunk-next "diff-hl-show-hunk" "Go to next hunk/change and show it." t) (autoload 'diff-hl-show-hunk "diff-hl-show-hunk" "Show the VC diff hunk at point.
The backend is determined by `diff-hl-show-hunk-function'." t) (autoload 'diff-hl-show-hunk-mouse-mode "diff-hl-show-hunk" "Enable margin and fringe to show a posframe/popup with vc diffs when clicked.

By default, the popup shows only the current hunk, and
the line of the hunk that matches the current position is
highlighted.  The face, border and other visual preferences are
customizable.  It can be also invoked with the command
`diff-hl-show-hunk'
\\{diff-hl-show-hunk-mouse-mode-map}

This is a minor mode.  If called interactively, toggle the
`Diff-Hl-Show-Hunk-Mouse mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `diff-hl-show-hunk-mouse-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (put 'global-diff-hl-show-hunk-mouse-mode 'globalized-minor-mode t) (defvar global-diff-hl-show-hunk-mouse-mode nil "Non-nil if Global Diff-Hl-Show-Hunk-Mouse mode is enabled.
See the `global-diff-hl-show-hunk-mouse-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-diff-hl-show-hunk-mouse-mode'.") (custom-autoload 'global-diff-hl-show-hunk-mouse-mode "diff-hl-show-hunk" nil) (autoload 'global-diff-hl-show-hunk-mouse-mode "diff-hl-show-hunk" "Toggle Diff-Hl-Show-Hunk-Mouse mode in all buffers.
With prefix ARG, enable Global Diff-Hl-Show-Hunk-Mouse mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Diff-Hl-Show-Hunk-Mouse mode is enabled in all buffers where `diff-hl-show-hunk-mouse-mode' would do it.

See `diff-hl-show-hunk-mouse-mode' for more information on Diff-Hl-Show-Hunk-Mouse mode.

(fn &optional ARG)" t) (register-definition-prefixes "diff-hl-show-hunk" '("diff-hl-show-hunk-")) (autoload 'diff-hl-show-hunk-posframe "diff-hl-show-hunk-posframe" "Implementation to show the hunk in a posframe.

(fn BUFFER &optional LINE)") (register-definition-prefixes "diff-hl-show-hunk-posframe" '("diff-hl-show-hunk-")) (provide 'diff-hl-autoloads)) "treesit-auto" ((treesit-auto-autoloads treesit-auto) (register-definition-prefixes "treesit-auto" '("global-treesit-auto-mode" "treesit-auto-")) (provide 'treesit-auto-autoloads)) "external-completion" ((external-completion external-completion-autoloads) (register-definition-prefixes "external-completion" '("external-completion-")) (provide 'external-completion-autoloads)) "track-changes" ((track-changes track-changes-autoloads track-changes-pkg) (register-definition-prefixes "track-changes" '("track-changes-" "with--track-changes")) (provide 'track-changes-autoloads)) "eglot" ((eglot eglot-pkg eglot-autoloads) (define-obsolete-function-alias 'eglot-update #'eglot-upgrade-eglot "29.1") (autoload 'eglot "eglot" "Start LSP server for PROJECT's buffers under MANAGED-MAJOR-MODES.

This starts a Language Server Protocol (LSP) server suitable for
the buffers of PROJECT whose `major-mode' is among
MANAGED-MAJOR-MODES.  CLASS is the class of the LSP server to
start and CONTACT specifies how to connect to the server.

Interactively, the command attempts to guess MANAGED-MAJOR-MODES,
CLASS, CONTACT, and LANGUAGE-IDS from `eglot-server-programs',
according to the current buffer's `major-mode'.  PROJECT is
guessed from `project-find-functions'.  The search for active
projects in this context binds `eglot-lsp-context' (which see).

If it can't guess, it prompts the user for the mode and the
server.  With a single \\[universal-argument] prefix arg, it
always prompts for COMMAND.  With two \\[universal-argument], it
also always prompts for MANAGED-MAJOR-MODE.

The LSP server of CLASS is started (or contacted) via CONTACT.
If this operation is successful, current *and future* file
buffers of MANAGED-MAJOR-MODE inside PROJECT become \"managed\"
by the LSP server, meaning the information about their contents is
exchanged periodically with the server to provide enhanced
code-analysis via `xref-find-definitions', `flymake-mode',
`eldoc-mode', and `completion-at-point', among others.

PROJECT is a project object as returned by `project-current'.

CLASS is a subclass of `eglot-lsp-server'.

CONTACT specifies how to contact the server.  It is a
keyword-value plist used to initialize CLASS or a plain list as
described in `eglot-server-programs', which see.

LANGUAGE-IDS is a list of language ID string to send to the
server for each element in MANAGED-MAJOR-MODES.

INTERACTIVE is ignored and provided for backward compatibility.

(fn MANAGED-MAJOR-MODES PROJECT CLASS CONTACT LANGUAGE-IDS &optional INTERACTIVE)" t) (autoload 'eglot-ensure "eglot" "Start Eglot session for current buffer if there isn't one.

Only use this function (in major mode hooks, etc) if you are
confident that Eglot can be started safely and efficiently for
*every* buffer visited where these hooks may execute.

Since it is difficult to establish this confidence fully, it's
often wise to use the interactive command `eglot' instead.  This
command only needs to be invoked once per project, as all other
files of a given major mode visited within the same project will
automatically become managed with no further user intervention
needed.") (autoload 'eglot-upgrade-eglot "eglot" "Update Eglot to latest version.

(fn &rest _)" t) (put 'eglot-workspace-configuration 'safe-local-variable #'listp) (put 'eglot--debbugs-or-github-bug-uri 'bug-reference-url-format t) (defun eglot--debbugs-or-github-bug-uri nil (format (if (string= (match-string 2) "github") "https://github.com/joaotavora/eglot/issues/%s" "https://debbugs.gnu.org/%s") (match-string 3))) (register-definition-prefixes "eglot" '("eglot-")) (provide 'eglot-autoloads)) "dape" ((dape-autoloads dape-pkg dape) (put 'dape-command 'safe-local-variable #'listp) (autoload 'dape "dape" "Start debugging session.
Start a debugging session for CONFIG.
See `dape-configs' for more information on CONFIG.

When called as an interactive command, the first symbol like
is read as key in the `dape-configs' alist and rest as elements
which override value plist in `dape-configs'.

Interactive example:
  launch :program \"bin\"

Executes alist key `launch' in `dape-configs' with :program as \"bin\".

Use SKIP-COMPILE to skip compilation.

(fn CONFIG &optional SKIP-COMPILE)" t) (register-definition-prefixes "dape" '("dape-")) (provide 'dape-autoloads)) "pyvenv" ((pyvenv-autoloads pyvenv) (autoload 'pyvenv-activate "pyvenv" "Activate the virtual environment in DIRECTORY.

(fn DIRECTORY)" t) (autoload 'pyvenv-deactivate "pyvenv" "Deactivate any current virtual environment." t) (autoload 'pyvenv-workon "pyvenv" "Activate a virtual environment from $WORKON_HOME.

If the virtual environment NAME is already active, this function
does not try to reactivate the environment.

(fn NAME)" t) (defvar pyvenv-mode nil "Non-nil if Pyvenv mode is enabled.
See the `pyvenv-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyvenv-mode'.") (custom-autoload 'pyvenv-mode "pyvenv" nil) (autoload 'pyvenv-mode "pyvenv" "Global minor mode for pyvenv.

Will show the current virtualenv in the mode line, and respect a
`pyvenv-workon' setting in files.

This is a global minor mode.  If called interactively, toggle the
`Pyvenv mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pyvenv-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (defvar pyvenv-tracking-mode nil "Non-nil if Pyvenv-Tracking mode is enabled.
See the `pyvenv-tracking-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyvenv-tracking-mode'.") (custom-autoload 'pyvenv-tracking-mode "pyvenv" nil) (autoload 'pyvenv-tracking-mode "pyvenv" "Global minor mode to track the current virtualenv.

When this mode is active, pyvenv will activate a buffer-specific
virtualenv whenever the user switches to a buffer with a
buffer-local `pyvenv-workon' or `pyvenv-activate' variable.

This is a global minor mode.  If called interactively, toggle the
`Pyvenv-Tracking mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pyvenv-tracking-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (autoload 'pyvenv-restart-python "pyvenv" "Restart Python inferior processes." t) (register-definition-prefixes "pyvenv" '("pyvenv-")) (provide 'pyvenv-autoloads)) "auto-virtualenv" ((auto-virtualenv auto-virtualenv-autoloads) (autoload 'auto-virtualenv-set-virtualenv "auto-virtualenv" "Activate virtualenv for buffer-filename.") (register-definition-prefixes "auto-virtualenv" '("auto-virtualenv-")) (provide 'auto-virtualenv-autoloads)) "lldb-voltron" ((lldb-voltron-autoloads lldb-voltron) (register-definition-prefixes "lldb-voltron" '("rgr/lldb-")) (provide 'lldb-voltron-autoloads)) "rust-mode" ((rust-common rust-prog-mode rust-rustfmt rust-mode rust-playpen rust-utils rust-mode-autoloads rust-compile rust-cargo rust-mode-treesitter) (register-definition-prefixes "rust-cargo" '("rust-")) (register-definition-prefixes "rust-common" '("rust-")) (register-definition-prefixes "rust-compile" '("cargo-compilation-regexps" "rustc-")) (autoload 'rust-mode "rust-mode" "Major mode for Rust code." t) (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)) (register-definition-prefixes "rust-mode" '("rust-")) (register-definition-prefixes "rust-playpen" '("rust-")) (register-definition-prefixes "rust-prog-mode" '("rust-")) (register-definition-prefixes "rust-rustfmt" '("rust-")) (autoload 'rust-dbg-wrap-or-unwrap "rust-utils" "Either remove or add the dbg! macro." t) (register-definition-prefixes "rust-utils" '("rust-")) (provide 'rust-mode-autoloads)) "markdown-mode" ((markdown-mode markdown-mode-autoloads) (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files.

(fn)" t) (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)) (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files.

(fn)" t) (autoload 'markdown-view-mode "markdown-mode" "Major mode for viewing Markdown content.

(fn)" t) (autoload 'gfm-view-mode "markdown-mode" "Major mode for viewing GitHub Flavored Markdown content.

(fn)" t) (autoload 'markdown-live-preview-mode "markdown-mode" "Toggle native previewing on save for a specific markdown file.

This is a minor mode.  If called interactively, toggle the
`Markdown-Live-Preview mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `markdown-live-preview-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "markdown-mode" '("defun-markdown-" "gfm-" "markdown")) (provide 'markdown-mode-autoloads)) "xterm-color" ((xterm-color-autoloads xterm-color) (autoload 'xterm-color-filter-strip "xterm-color" "Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

In order to get maximum performance, this function strips text properties
if they are present in STRING.

(fn STRING)") (autoload 'xterm-color-filter "xterm-color" "Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

This function checks if `xterm-color-preserve-properties' is non-nil
and only calls `xterm-color-filter-strip' on substrings that do not
have text properties applied (passing through the rest unmodified).
Preserving properties in this fashion is not very robust as there may
be situations where text properties are applied on ANSI data, which
will desync the state machine.

Preserving properties works ok with and is really meant for eshell.

This can be inserted into `comint-preoutput-filter-functions'.

(fn STRING)") (autoload 'xterm-color-256 "xterm-color" "

(fn COLOR)") (autoload 'xterm-color-colorize-buffer "xterm-color" "Apply `xterm-color-filter' to current buffer, and replace its contents.
Colors are applied using \\='face, unless font-lock-mode is active, in
which case \\='font-lock-face is used. Operation with font-lock mode active
is not recommended.

If USE-OVERLAYS is non-nil, colors are applied to the buffer using overlays
instead of text properties. A C-u prefix arg causes overlays to be used.

(fn &optional USE-OVERLAYS)" t) (autoload 'xterm-color-clear-cache "xterm-color" "Clear xterm color face attribute cache.
You may want to call this if you change `xterm-color-names' or
`xterm-color-names-bright' at runtime and you want to see the changes
take place in a pre-existing buffer that has had xterm-color initialized.

Since the cache is buffer-local and created on-demand when needed, this has no
effect when called from a buffer that does not have a cache." t) (autoload 'xterm-color-test "xterm-color" "Create, display and render a new buffer containing ANSI control sequences." t) (autoload 'xterm-color-test-raw "xterm-color" "Create and display a new buffer containing ANSI SGR control sequences.
ANSI sequences are not processed. One can use a different Emacs package,
such as ansi-color.el to do so. This is really meant to be used for easy
comparisons/benchmarks with libraries that offer similar functionality." t) (register-definition-prefixes "xterm-color" '("+xterm-color--table-256+" "xterm-color-")) (provide 'xterm-color-autoloads)) "flycheck" ((flycheck-buttercup flycheck-autoloads flycheck flycheck-ert) (autoload 'flycheck-manual "flycheck" "Open the Flycheck manual." t) (autoload 'flycheck-quick-help "flycheck" "Display brief Flycheck help." t) (autoload 'flycheck-mode "flycheck" "Flycheck is a minor mode for on-the-fly syntax checking.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

If you run into issues, use `\\[flycheck-verify-setup]' to get help.

Flycheck supports many languages out of the box, and many
additional ones are available on MELPA.  Adding new ones is very
easy.  Complete documentation is available online at URL
`https://www.flycheck.org/en/latest/'.  Please report issues and
request features at URL `https://github.com/flycheck/flycheck'.

Flycheck displays its status in the mode line.  In the default
configuration, it looks like this:

`FlyC'     This buffer has not been checked yet.
`FlyC*'    Flycheck is running.  Expect results soon!
`FlyC:0'   Last check resulted in no errors and no warnings.
`FlyC:3|5' This buffer contains three errors and five warnings.
           Use `\\[flycheck-list-errors]' to see the list.
`FlyC-'    Flycheck doesn't have a checker for this buffer.

You may also see the following icons:
`FlyC!'    The checker crashed.
`FlyC.'    The last syntax check was manually interrupted.
`FlyC?'    The checker did something unexpected, like exiting with 1
           but returning no errors.

The following keybindings are available in `flycheck-mode':

\\{flycheck-mode-map}
(you can change the prefix by customizing
`flycheck-keymap-prefix')

If called interactively, enable Flycheck mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is ‘toggle’; disable the mode otherwise.

(fn &optional ARG)" t) (put 'global-flycheck-mode 'globalized-minor-mode t) (defvar global-flycheck-mode nil "Non-nil if Global Flycheck mode is enabled.
See the `global-flycheck-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-mode'.") (custom-autoload 'global-flycheck-mode "flycheck" nil) (autoload 'global-flycheck-mode "flycheck" "Toggle Flycheck mode in all buffers.
With prefix ARG, enable Global Flycheck mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Flycheck mode is enabled in all buffers where `flycheck-mode-on-safe' would do it.

See `flycheck-mode' for more information on Flycheck mode.

(fn &optional ARG)" t) (autoload 'flycheck-define-error-level "flycheck" "Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0; info is -10, warning is 10, and
     error is 100.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:compilation-level LEVEL'

     A number indicating the broad class of messages that errors
     at this level belong to: one of 0 (info), 1 (warning), or
     2 or nil (error).  Defaults to nil.

     This is used by `flycheck-checker-pattern-to-error-regexp'
     to map error levels into `compilation-mode''s hierarchy and
     to get proper highlighting of errors in `compilation-mode'.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Another useful
     property for error level categories is `priority', to
     influence the stacking of multiple error level overlays.

`:fringe-bitmap BITMAPS'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level, or a cons of two bitmaps (one for
     narrow fringes and one for wide fringes).  See Info node
     `(elisp)Fringe Bitmaps' for more information about fringe
     bitmaps, including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:margin-spec SPEC'
     A display specification indicating what to display in the
     margin when `flycheck-indication-mode' is `left-margin' or
     `right-margin'.  See Info node `(elisp)Displaying in the
     Margins'.  If omitted, Flycheck generates an image spec from
     the fringe bitmap.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'.

(fn LEVEL &rest PROPERTIES)") (function-put 'flycheck-define-error-level 'lisp-indent-function 1) (autoload 'flycheck-define-command-checker "flycheck" "Define SYMBOL as syntax checker to run a command.

Define SYMBOL as generic syntax checker via
`flycheck-define-generic-checker', which uses an external command
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`flycheck-define-generic-checker'.

In addition to the properties understood by
`flycheck-define-generic-checker', the following PROPERTIES
constitute a command syntax checker.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of command checkers is `flycheck-sanitize-errors'.

`:command COMMAND'
     The command to run for syntax checking.

     COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns PATTERNS'
     A list of patterns to parse the output of the `:command'.

     Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
     LEVEL is a Flycheck error level (see
     `flycheck-define-error-level'), followed by one or more RX
     `SEXP's which parse an error of that level and extract line,
     column, file name and the message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for some special RX forms provided
     by Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

     This property is optional.  If omitted, however, an
     `:error-parser' is mandatory.

`:error-parser FUNCTION'
     A function to parse errors with.

     The function shall accept three arguments OUTPUT CHECKER
     BUFFER.  OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

     This property is optional.  If omitted, it defaults to
     `flycheck-parse-with-patterns'.  In this case,
     `:error-patterns' is mandatory.

`:standard-input t'
     Whether to send the buffer contents on standard input.

     If this property is given and has a non-nil value, send the
     contents of the buffer on standard input.

     Some checkers that support reading from standard input have
     a separate flag to indicate the name of the file whose
     contents are being passed on standard input (typically
     `stdin-filename').  In that case, use the `(option)' form in
     `:command' to pass the value of variable `buffer-file-name'
     when the current buffer has a file name (that is,
     use `option \"--stdin-file-name\" buffer-file-name').

     For buffers not backed by files, checkers that support input
     on stdin typically report a file name like `-' or `<stdin>'.
     Make sure your error parser or patterns expect these file
     names (for example, use `(or \"<stdin>\" (file-name))') or
     call `flycheck-remove-error-file-names' in a custom
     `:error-filter'.

     Defaults to nil.

Note that you may not give `:start', `:interrupt', and
`:print-doc' for a command checker.  You can give a custom
`:verify' function, though, whose results will be appended to the
default `:verify' function of command checkers.

(fn SYMBOL DOCSTRING &rest PROPERTIES)") (function-put 'flycheck-define-command-checker 'lisp-indent-function 1) (function-put 'flycheck-define-command-checker 'doc-string-elt 2) (autoload 'flycheck-def-config-file-var "flycheck" "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable using `defcustom', to
provide configuration files for the given syntax CHECKER.
CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.  It can be either a string or a list of
strings.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'.

(fn SYMBOL CHECKER &optional FILE-NAME &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-config-file-var 'lisp-indent-function 3) (autoload 'flycheck-def-option-var "flycheck" "Define SYMBOL as option variable with INIT-VALUE for CHECKER.

SYMBOL is declared as customizable variable using `defcustom', to
provide an option for the given syntax CHECKERS (a checker or a
list of checkers).  INIT-VALUE is the initial value of the
variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
forwarded to `defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'.

(fn SYMBOL INIT-VALUE CHECKERS DOCSTRING &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-option-var 'lisp-indent-function 3) (function-put 'flycheck-def-option-var 'doc-string-elt 4) (autoload 'flycheck-define-checker "flycheck" "Define SYMBOL as command syntax checker with DOCSTRING and PROPERTIES.

Like `flycheck-define-command-checker', but PROPERTIES must not
be quoted.  Also, implicitly define the executable variable for
SYMBOL with `flycheck-def-executable-var'.

(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil t) (function-put 'flycheck-define-checker 'lisp-indent-function 1) (function-put 'flycheck-define-checker 'doc-string-elt 2) (register-definition-prefixes "flycheck" '("flycheck-" "help-flycheck-checker-d" "list-flycheck-errors")) (register-definition-prefixes "flycheck-buttercup" '("flycheck-buttercup-format-error-list")) (register-definition-prefixes "flycheck-ert" '("flycheck-er")) (provide 'flycheck-autoloads)) "rustic" ((rustic-flycheck rustic-spellcheck rustic-popup rustic-comint rustic-autoloads rustic-rustfmt rustic-clippy rustic-lsp rustic-doc rustic rustic-interaction rustic-babel rustic-expand rustic-cargo rustic-compile rustic-rustfix rustic-playground) (autoload 'rustic-mode "rustic" "Major mode for Rust code.

\\{rustic-mode-map}

(fn)" t) (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode)) (register-definition-prefixes "rustic" '("rustic-")) (register-definition-prefixes "rustic-babel" '("cargo-toml-dependencies" "crate-dependencies" "org-babel-execute:rust" "rustic-")) (autoload 'rustic-cargo-test-run "rustic-cargo" "Start compilation process for 'cargo test' with optional TEST-ARGS.

(fn &optional TEST-ARGS)" t) (autoload 'rustic-cargo-test "rustic-cargo" "Run 'cargo test'.

If ARG is not nil, use value as argument and store it in
`rustic-test-arguments'.  When calling this function from
`rustic-popup-mode', always use the value of
`rustic-test-arguments'.

(fn &optional ARG)" t) (autoload 'rustic-cargo-test-rerun "rustic-cargo" "Run 'cargo test' with `rustic-test-arguments'." t) (autoload 'rustic-cargo-current-test "rustic-cargo" "Run 'cargo test' for the test near point." t) (autoload 'rustic-cargo-test-dwim "rustic-cargo" "Run test or mod at point. Otherwise run `rustic-cargo-test'." t) (autoload 'rustic-cargo-outdated "rustic-cargo" "Use 'cargo outdated' to list outdated packages in `tabulated-list-mode'.
Execute process in PATH.

(fn &optional PATH)" t) (autoload 'rustic-cargo-reload-outdated "rustic-cargo" "Update list of outdated packages." t) (autoload 'rustic-cargo-mark-upgrade "rustic-cargo" "Mark an upgradable package." t) (autoload 'rustic-cargo-mark-latest-upgrade "rustic-cargo" "Mark an upgradable package to the latest available version." t) (autoload 'rustic-cargo-mark-all-upgrades-latest "rustic-cargo" "Mark all packages in the Package Menu to latest version." t) (autoload 'rustic-cargo-mark-all-upgrades "rustic-cargo" "Mark all upgradable packages in the Package Menu." t) (autoload 'rustic-cargo-menu-mark-unmark "rustic-cargo" "Clear any marks on a package." t) (autoload 'rustic-cargo-upgrade-execute "rustic-cargo" "Perform marked menu actions." t) (autoload 'rustic-cargo-new "rustic-cargo" "Run 'cargo new' to start a new package in the path specified by PROJECT-PATH.
If BIN is not nil, create a binary application, otherwise a library.

(fn PROJECT-PATH &optional BIN)" t) (autoload 'rustic-cargo-init "rustic-cargo" "Run 'cargo init' to initialize a directory in the path specified by PROJECT-PATH.
If BIN is not nil, create a binary application, otherwise a library.

(fn PROJECT-PATH &optional BIN)" t) (autoload 'rustic-cargo-run-command "rustic-cargo" "Start compilation process for 'cargo run' with optional RUN-ARGS.

(fn &optional RUN-ARGS)" t) (autoload 'rustic-cargo-run "rustic-cargo" "Run 'cargo run'.

If ARG is not nil, use value as argument and store it in `rustic-run-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-run-arguments'.

(fn &optional ARG)" t) (autoload 'rustic-cargo-run-rerun "rustic-cargo" "Run 'cargo run' with `rustic-run-arguments'." t) (autoload 'rustic-run-shell-command "rustic-cargo" "Run an arbitrary shell command using ARG for the current project.
Example: use it to provide an environment variable to your
application like this `env MYVAR=1 cargo run' so that it can read
it at the runtime.  As a byproduct, you can run any shell command
in your project like `pwd'

(fn &optional ARG)" t) (autoload 'rustic-cargo-build "rustic-cargo" "Run 'cargo build' for the current project, allow configuring
`rustic-cargo-build-arguments' when prefix argument (C-u) is enabled.

(fn &optional ARG)" t) (autoload 'rustic-cargo-clean "rustic-cargo" "Run 'cargo clean' for the current project.

If ARG is not nil, use value as argument and store it in `rustic-clean-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-clean-arguments'.

(fn &optional ARG)" t) (autoload 'rustic-cargo-check "rustic-cargo" "Run 'cargo check' for the current project, allow configuring
`rustic-cargo-check-arguments' when prefix argument (C-u) is enabled.

(fn &optional ARG)" t) (autoload 'rustic-cargo-bench "rustic-cargo" "Run 'cargo bench' for the current project." t) (autoload 'rustic-cargo-build-doc "rustic-cargo" "Build the documentation for the current project." t) (autoload 'rustic-cargo-doc "rustic-cargo" "Open the documentation for the current project in a browser.
The documentation is built if necessary." t) (autoload 'rustic-cargo-add "rustic-cargo" "Add crate to Cargo.toml using 'cargo add'.
If running with prefix command `C-u', read whole command from minibuffer.

(fn &optional ARG)" t) (autoload 'rustic-cargo-rm "rustic-cargo" "Remove crate from Cargo.toml using 'cargo rm'.
If running with prefix command `C-u', read whole command from minibuffer.

(fn &optional ARG)" t) (autoload 'rustic-cargo-upgrade "rustic-cargo" "Upgrade dependencies as specified in the local manifest file using 'cargo upgrade'.
If running with prefix command `C-u', read whole command from minibuffer.

(fn &optional ARG)" t) (autoload 'rustic-cargo-update "rustic-cargo" "Update dependencies as recorded in the local lock file.
If running with prefix command `C-u', use ARG by reading whole
command from minibuffer.

(fn &optional ARG)" t) (autoload 'rustic-cargo-login "rustic-cargo" "Add crates.io API token using `cargo login'.

`TOKEN' the token for interacting with crates.io. Visit [1] for
        how to get one

[1] https://doc.rust-lang.org/cargo/reference/publishing.html#before-your-first-publish

(fn TOKEN)" t) (autoload 'rustic-cargo-install-rerun "rustic-cargo" "Run 'cargo install' with `rustic-install-arguments'." t) (autoload 'rustic-cargo-install "rustic-cargo" "Install rust binary using 'cargo install'.
If running with prefix command `C-u', read whole command from minibuffer.

(fn &optional ARG)" t) (register-definition-prefixes "rustic-cargo" '("rustic-")) (autoload 'rustic-cargo-clippy-run "rustic-clippy" "Run `cargo clippy' with optional ARGS.

(fn &rest ARGS)" t) (autoload 'rustic-cargo-lints "rustic-clippy" "Run cargo-lints with optional ARGS." t) (autoload 'rustic-cargo-clippy "rustic-clippy" "Run 'cargo clippy'.

If ARG is not nil, use value as argument and store it in `rustic-clippy-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-clippy-arguments'.

(fn &optional ARG)" t) (autoload 'rustic-cargo-clippy-rerun "rustic-clippy" "Run 'cargo clippy' with `rustic-clippy-arguments'." t) (register-definition-prefixes "rustic-clippy" '("rustic-")) (autoload 'rustic-cargo-comint-run "rustic-comint" "Run 'cargo run' but for interactive programs.

If ARG is not nil, use value as argument and store it in `rustic-run-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-run-arguments'.

(fn &optional ARG)" t) (autoload 'rustic-cargo-comint-run-rerun "rustic-comint" "Run `cargo run' with `rustic-run-comint-arguments'." t) (autoload 'rustic-cargo-plain-run "rustic-comint" "Run `cargo run' for the current project.
Read the full command from the minibuffer when ARG is non-nil or
when called with a prefix command \\[universal-argument].

(fn &optional ARG)" t) (register-definition-prefixes "rustic-comint" '("poly-rustic-cargo-comint-switch-buffer-hook" "rustic-")) (autoload 'rustic-compile "rustic-compile" "Run COMMAND in the current Rust workspace root.

Interactively, prompts for the command if the variable
`compilation-read-command' is non-nil; otherwise uses
`rustic-compile-command'.

(fn COMMAND)" t) (autoload 'rustic-recompile "rustic-compile" "Re-compile the program using `compilation-arguments'." t) (register-definition-prefixes "rustic-compile" '("rust")) (autoload 'rustic-doc-dumb-search "rustic-doc" "Search all projects and std for SEARCH-TERM.
Use this when `rustic-doc-search' does not find what you're looking for.
Add `universal-argument' to only search level 1 headers.
See `rustic-doc-search' for more information.

(fn SEARCH-TERM)" t) (autoload 'rustic-doc-search "rustic-doc" "Search the rust documentation for SEARCH-TERM.
Only searches in headers (structs, functions, traits, enums, etc)
to limit the number of results.
To limit search results to only level 1 headers, add `universal-argument'
Level 1 headers are things like struct or enum names.
if ROOT is non-nil the search is performed from the root dir.
This function tries to be smart and limits the search results
as much as possible. If it ends up being so smart that
it doesn't manage to find what you're looking for, try `rustic-doc-dumb-search'.

(fn SEARCH-TERM &optional ROOT)" t) (autoload 'rustic-doc-convert-current-package "rustic-doc" "Convert the documentation for a project and its dependencies." t) (autoload 'rustic-doc-setup "rustic-doc" "Setup or update rustic-doc filter and convert script. Convert std.
If NO-DL is non-nil, will not try to re-download
the pandoc filter and bash script.
NO-DL is primarily used for development of the filters.
If NOCONFIRM is non-nil, install all dependencies without prompting user.

(fn &optional NO-DL NOCONFIRM)" t) (autoload 'rustic-doc-mode "rustic-doc" "Convert rust html docs to .org, and browse the converted docs.

This is a minor mode.  If called interactively, toggle the
`Rustic-Doc mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `rustic-doc-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "rustic-doc" '("rustic-doc-")) (autoload 'rustic-cargo-expand "rustic-expand" "Run 'cargo expand'.

If ARG is not nil, use value as argument and store it in
`rustic-expand-arguments'.  When calling this function from
`rustic-popup-mode', always use the value of
`rustic-expand-arguments'.

(fn &optional ARG)" t) (autoload 'rustic-cargo-expand-rerun "rustic-expand" "Run 'cargo expand' with `rustic-expand-arguments'." t) (register-definition-prefixes "rustic-expand" '("rustic-")) (autoload 'rustic-flycheck-setup "rustic-flycheck" "Setup Rust in Flycheck.

If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout." t) (register-definition-prefixes "rustic-flycheck" '("rustic-flycheck-")) (autoload 'rustic-open-dependency-file "rustic-interaction" "Open the 'Cargo.toml' file at the project root if the current buffer is
visiting a project." t) (register-definition-prefixes "rustic-interaction" '("rustic-")) (autoload 'rustic-analyzer-macro-expand "rustic-lsp" "Default method for displaying macro expansion RESULT .

(fn RESULT)") (register-definition-prefixes "rustic-lsp" '("rustic-")) (autoload 'rustic-playground "rustic-playground" "Create a shareable URL for the contents of the current region,
src-block or buffer on the Rust playground.

(fn BEGIN END)" t) (register-definition-prefixes "rustic-playground" '("rustic-")) (autoload 'rustic-popup "rustic-popup" "Setup popup.
If directory is not in a rust project call `read-directory-name'.

(fn &optional ARGS)" t) (autoload 'rustic-popup-invoke-popup-action "rustic-popup" "Execute commands which are listed in `rustic-popup-commands'.

(fn EVENT)" t) (autoload 'rustic-popup-default-action "rustic-popup" "Change backtrace and `compilation-arguments' when executed on
corresponding line." t) (autoload 'rustic-popup-cargo-command-help "rustic-popup" "Display help buffer for cargo command at point." t) (autoload 'rustic-popup-kill-help-buffer "rustic-popup" "Kill popup help buffer and switch to popup buffer." t) (register-definition-prefixes "rustic-popup" '("rustic-")) (autoload 'rustic-rustfix "rustic-rustfix" "Run 'cargo fix'." t) (register-definition-prefixes "rustic-rustfix" '("rustic-rustfix-")) (autoload 'rustic-cargo-fmt "rustic-rustfmt" "Use rustfmt via cargo." t) (autoload 'rustic-format-region "rustic-rustfmt" "Format the current active region using rustfmt.

This operation requires a nightly version of rustfmt.

(fn BEGIN END)" t) (autoload 'rustic-format-buffer "rustic-rustfmt" "Format the current buffer using rustfmt." t) (autoload 'rustic-format-file "rustic-rustfmt" "Unlike `rustic-format-buffer' format file directly and revert the buffer.

(fn &optional FILE)" t) (register-definition-prefixes "rustic-rustfmt" '("rustic-")) (autoload 'rustic-cargo-spellcheck "rustic-spellcheck" "Run `cargo spellcheck'.

If ARG is not nil, use value as argument and store it in
`rustic-spellcheck-arguments'.  When calling this function from
`rustic-popup-mode', always use the value of
`rustic-spellcheck-arguments'.

(fn &optional ARG)" t) (autoload 'rustic-cargo-spellcheck-rerun "rustic-spellcheck" "Run `cargo spellcheck' with `rustic-spellcheck-arguments'." t) (register-definition-prefixes "rustic-spellcheck" '("rustic-")) (provide 'rustic-autoloads)) "extmap" ((extmap extmap-autoloads) (register-definition-prefixes "extmap" '("extmap-")) (provide 'extmap-autoloads)) "datetime" ((datetime-autoloads datetime) (register-definition-prefixes "datetime" '("datetime-")) (provide 'datetime-autoloads)) "logview" ((logview logview-autoloads) (add-to-list 'auto-mode-alist '("\\.log\\(?:\\.[0-9\\-]+\\)?\\'" . logview-mode) t) (autoload 'logview-mode "logview" "Major mode for viewing and filtering various log files.

(fn)" t) (register-definition-prefixes "logview" '("logview-")) (provide 'logview-autoloads)) "strace-mode" ((strace-mode strace-mode-autoloads) (autoload 'strace-mode "strace-mode" "Major mode for strace output.

(fn)" t) (add-to-list 'auto-mode-alist '("\\.strace\\'" . strace-mode)) (provide 'strace-mode-autoloads)) "elf-mode" ((elf-mode-autoloads elf-mode) (autoload 'elf-mode "elf-mode" nil t) (register-definition-prefixes "elf-mode" '("elf-")) (provide 'elf-mode-autoloads)) "rgr-kill-dwim" ((rgr-kill-dwim)) "package-lint" ((package-lint-autoloads package-lint) (autoload 'package-lint-describe-symbol-history "package-lint" "Show the version history of SYM, if any.

(fn SYM)" t) (autoload 'package-lint-buffer "package-lint" "Get linter errors and warnings for BUFFER.

Returns a list, each element of which is list of

   (LINE COL TYPE MESSAGE)

where TYPE is either `warning' or `error'.

Current buffer is used if none is specified.

(fn &optional BUFFER)") (autoload 'package-lint-current-buffer "package-lint" "Display lint errors and warnings for the current buffer." t) (autoload 'package-lint-batch-and-exit "package-lint" "Run `package-lint-buffer' on the files remaining on the command line.
Use this only with -batch, it won't work interactively.

When done, exit Emacs with status 1 in case of any errors, otherwise exit
with status 0.  The variable `package-lint-batch-fail-on-warnings' controls
whether or not warnings alone produce a non-zero exit code.") (autoload 'package-lint-looks-like-a-package-p "package-lint" "Return non-nil if the current buffer appears to be intended as a package.") (register-definition-prefixes "package-lint" '("package-lint-")) (provide 'package-lint-autoloads)) "elisp-refs" ((elisp-refs-autoloads elisp-refs) (autoload 'elisp-refs-function "elisp-refs" "Display all the references to function SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

This searches for functions, not macros. For that, see
`elisp-refs-macro'.

(fn SYMBOL &optional PATH-PREFIX)" t) (autoload 'elisp-refs-macro "elisp-refs" "Display all the references to macro SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

This searches for macros, not functions. For that, see
`elisp-refs-function'.

(fn SYMBOL &optional PATH-PREFIX)" t) (autoload 'elisp-refs-special "elisp-refs" "Display all the references to special form SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

(fn SYMBOL &optional PATH-PREFIX)" t) (autoload 'elisp-refs-variable "elisp-refs" "Display all the references to variable SYMBOL, in all loaded
elisp files.

If called with a prefix, prompt for a directory to limit the search.

(fn SYMBOL &optional PATH-PREFIX)" t) (autoload 'elisp-refs-symbol "elisp-refs" "Display all the references to SYMBOL in all loaded elisp files.

If called with a prefix, prompt for a directory to limit the
search.

(fn SYMBOL &optional PATH-PREFIX)" t) (register-definition-prefixes "elisp-refs" '("elisp-")) (provide 'elisp-refs-autoloads)) "helpful" ((helpful helpful-autoloads) (autoload 'helpful-function "helpful" "Show help for function named SYMBOL.

See also `helpful-macro', `helpful-command' and `helpful-callable'.

(fn SYMBOL)" t) (autoload 'helpful-command "helpful" "Show help for interactive function named SYMBOL.

See also `helpful-function'.

(fn SYMBOL)" t) (autoload 'helpful-key "helpful" "Show help for interactive command bound to KEY-SEQUENCE.

(fn KEY-SEQUENCE)" t) (autoload 'helpful-macro "helpful" "Show help for macro named SYMBOL.

(fn SYMBOL)" t) (autoload 'helpful-callable "helpful" "Show help for function, macro or special form named SYMBOL.

See also `helpful-macro', `helpful-function' and `helpful-command'.

(fn SYMBOL)" t) (autoload 'helpful-symbol "helpful" "Show help for SYMBOL, a variable, function, macro, or face.

See also `helpful-callable' and `helpful-variable'.

(fn SYMBOL)" t) (autoload 'helpful-variable "helpful" "Show help for variable named SYMBOL.

(fn SYMBOL)" t) (autoload 'helpful-at-point "helpful" "Show help for the symbol at point." t) (register-definition-prefixes "helpful" '("helpful-")) (provide 'helpful-autoloads)) "el-docstring-sap" ((el-docstring-sap-autoloads el-docstring-sap) (autoload 'el-docstring-sap-display "el-docstring-sap" "Display docstring for optional SYM, defaulting to `symbol-at-point', using `el-docstring-sap--display-func'.

(fn &optional SYM)" t) (autoload 'el-docstring-sap-select-display-func "el-docstring-sap" "Select a `el-docstring-sap-mode' display function from `el-docstring-sap--display-funcs'." t) (autoload 'el-docstring-sap-mode "el-docstring-sap" "minor-mode to popup help for the elisp symbol at point.

This is a minor mode.  If called interactively, toggle the
`El-Docstring-Sap mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `el-docstring-sap-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "el-docstring-sap" '("el-docstring-sap--")) (provide 'el-docstring-sap-autoloads)) "quick-peek" ((quick-peek-autoloads quick-peek) (autoload 'quick-peek-overlay-at "quick-peek" "Find overlay for line at POS.

(fn POS)") (autoload 'quick-peek-show "quick-peek" "Show STR in an inline window at POS.
MIN-H (default: 4) and MAX-H (default: 16) are bounds on the
height of the window.  Setting MAX-H to `none' allows the inline
window to expand past the bottom of the current Emacs window.

(fn STR &optional POS MIN-H MAX-H)") (autoload 'quick-peek-hide "quick-peek" "Hide inline windows.
With non-nil POS, clear only overlays on line of POS.
Return the number of overlays hidden.

(fn &optional POS)" t) (register-definition-prefixes "quick-peek" '("quick-peek-")) (provide 'quick-peek-autoloads)) "edebug-x" ((edebug-x-autoloads edebug-x) (autoload 'edebug-x-modify-breakpoint-wrapper "edebug-x" "Set a breakpoint from an Elisp file.
The current function that pointer is in will be instrumented if
not already. When called with a prefix argument a conditional
breakpoint is set.

(fn ARG)" t) (autoload 'edebug-x-evaluate-function "edebug-x" "Evaluate function on line.
This removes all breakpoints in this function." t) (autoload 'edebug-x-show-data "edebug-x" "Display instrumented functions and edebug breakpoints.
Frame is split into two vertically showing the tabluated buffers
for each." t) (autoload 'edebug-x-show-breakpoints "edebug-x" "Display breakpoints in a tabulated list buffer." t) (autoload 'edebug-x-show-instrumented "edebug-x" "Display instrumented functions in a tabluated list buffer." t) (autoload 'edebug-x-mode "edebug-x" "A minor mode that makes it easier to use Edebug

This is a minor mode.  If called interactively, toggle the
`Edebug-X mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `edebug-x-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (add-hook 'emacs-lisp-mode-hook 'edebug-x-mode) (register-definition-prefixes "edebug-x" '("edebug-x-" "instrumented" "list-edebug-x-")) (provide 'edebug-x-autoloads)) "elisp-format" ((elisp-format elisp-format-autoloads) (autoload 'elisp-format-region "elisp-format" "Format current region or buffer.
This function will format region from START to END.
Or try to format `defun' around point.

(fn &optional START END)" t) (autoload 'elisp-format-buffer "elisp-format" "Format current buffer." t) (autoload 'elisp-format-file "elisp-format" "Format file with FILENAME.

(fn FILENAME)" t) (autoload 'elisp-format-file-batch "elisp-format" "Format elisp FILENAME.
But instead in `batch-mode'.
If SURPRESS-POPUP-WINDOW is non-nil, don't show output window.

(fn FILENAME &optional SURPRESS-POPUP-WINDOW)" t) (autoload 'elisp-format-directory "elisp-format" "Format recursive elisp files under DIR.

(fn DIR)" t) (autoload 'elisp-format-directory-batch "elisp-format" "Format recursive elisp files under DIR.
But instead in `batch-mode'.
If SURPRESS-POPUP-WINDOW is non-nil, don't show output window.

(fn DIR &optional SURPRESS-POPUP-WINDOW)" t) (autoload 'elisp-format-dired-mark-files "elisp-format" "Format dired mark files." t) (autoload 'elisp-format-library "elisp-format" "Format LIBRARY.

(fn LIBRARY)" t) (register-definition-prefixes "elisp-format" '("elisp-format-")) (provide 'elisp-format-autoloads)) "modus-themes" ((modus-vivendi-deuteranopia-theme modus-themes modus-operandi-tinted-theme modus-operandi-deuteranopia-theme modus-operandi-tritanopia-theme theme-loaddefs modus-vivendi-tritanopia-theme modus-themes-autoloads modus-vivendi-theme modus-operandi-theme modus-vivendi-tinted-theme) (autoload 'modus-themes-contrast "modus-themes" "Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB.

(fn C1 C2)") (autoload 'modus-themes-select "modus-themes" "Load a Modus THEME using minibuffer completion.
Run `modus-themes-after-load-theme-hook' after loading the theme.
Disable other themes per `modus-themes-disable-other-themes'.

(fn THEME)" t) (autoload 'modus-themes-toggle "modus-themes" "Toggle between the two `modus-themes-to-toggle'.
If `modus-themes-to-toggle' does not specify two Modus themes,
prompt with completion for a theme among our collection (this is
practically the same as the `modus-themes-select' command).

Run `modus-themes-after-load-theme-hook' after loading the theme.
Disable other themes per `modus-themes-disable-other-themes'." t) (autoload 'modus-themes-theme "modus-themes" "Bind NAME's color PALETTE around face specs and variables.
Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.
Those are stored in `modus-themes-faces' and
`modus-themes-custom-variables' respectively.

Optional OVERRIDES are appended to PALETTE, overriding
corresponding entries.

(fn NAME PALETTE &optional OVERRIDES)" nil t) (function-put 'modus-themes-theme 'lisp-indent-function 0) (when load-file-name (let ((dir (file-name-directory load-file-name))) (unless (equal dir (expand-file-name "themes/" data-directory)) (add-to-list 'custom-theme-load-path dir)))) (register-definition-prefixes "modus-themes" '("modus-themes-")) (provide 'modus-themes-autoloads)) "mu4e-alert" ((mu4e-alert mu4e-alert-autoloads) (autoload 'mu4e-alert-set-default-style "mu4e-alert" "Set the default style for unread email notifications.

VALUE is the value to be used as the default style.

(fn VALUE)") (autoload 'mu4e-alert-enable-mode-line-display "mu4e-alert" "Enable display of unread emails in mode-line." t) (autoload 'mu4e-alert-enable-notifications "mu4e-alert" "Enable desktop notifications for unread emails." t) (register-definition-prefixes "mu4e-alert" '("mu4e-alert-")) (provide 'mu4e-alert-autoloads)) "mu4e-column-faces" ((mu4e-column-faces mu4e-column-faces-autoloads) (defvar mu4e-column-faces-mode nil "Non-nil if Mu4e-Column-Faces mode is enabled.
See the `mu4e-column-faces-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mu4e-column-faces-mode'.") (custom-autoload 'mu4e-column-faces-mode "mu4e-column-faces" nil) (autoload 'mu4e-column-faces-mode "mu4e-column-faces" "Global minor mode for individual column faces in mu4e's email overview.

The view must be refreshed with `mu4e-headers-rerun-search' for the changes to
take effect.
Requires at least mu4e v1.8.0.

This is a global minor mode.  If called interactively, toggle the
`Mu4e-Column-Faces mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='mu4e-column-faces-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t) (register-definition-prefixes "mu4e-column-faces" '("mu4e-column-faces-")) (provide 'mu4e-column-faces-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 217 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 15 "melpa" nil "gnu-elpa-mirror" nil "nongnu-elpa" nil "el-get" nil "emacsmirror-mirror" nil "straight" nil "notifications" nil "no-littering" nil "compat" nil "seq" nil "org" (org :type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))) "project" nil "xref" nil "flymake" nil "eldoc" nil "pass" nil "password-store" nil "with-editor" nil "password-store-otp" nil "s" nil "f" nil "dash" nil "gif-screencast" nil "lazy-lang-learn" nil "google-translate" nil "popup" nil "alert" nil "gntp" nil "log4e" nil "cl-lib" nil "repeat" nil "browse-url-dwim" nil "string-utils" nil "list-utils" nil "posframe" nil "ace-window" nil "avy" nil "ace-link" nil "ace-jump-mode" nil "golden-ratio" nil "pulsar" nil "blackout" nil "boxquote" nil "dpaste" nil "darkroom" nil "tab-bar" nil "bookmark+" nil "emojify" nil "ht" nil "multiple-cursors" nil "hideshow" nil "jinx" nil "ripgrep" nil "sudo-edit" nil "ffap" nil "prescient" nil "consult" nil "marginalia" nil "all-the-icons" nil "all-the-icons-completion" nil "all-the-icons-dired" nil "corfu" nil "orderless" nil "dabbrev" nil "cape" nil "which-key" nil "yasnippet" nil "yasnippet-snippets" nil "vertico" nil "org-contrib" (org-contrib :type git :includes (ob-csharp ob-eukleides ob-fomus ob-julia ob-mathomatic ob-oz ob-stata ob-tcl ob-vbnet ol-bookmark ol-elisp-symbol ol-git-link ol-man ol-mew ol-vm ol-wl org-annotate-file org-bibtex-extras org-checklist org-choose org-collector org-contribdir org-depend org-effectiveness org-eldoc org-eval org-eval-light org-expiry org-interactive-query org-invoice org-learn org-license org-mac-iCal org-mairix org-panel org-registry org-screen org-screenshot org-secretary org-static-mathjax org-sudoku orgtbl-sqlinsert org-toc org-track org-wikinodes ox-bibtex ox-confluence ox-deck ox-extra ox-freemind ox-groff ox-koma-letter ox-s5 ox-taskjuggler) :repo "https://git.sr.ht/~bzg/org-contrib" :files (:defaults "lisp/*.el")) "ob-async" nil "async" nil "org-super-agenda" nil "ts" nil "ox-gfm" nil "gptel" nil "transient" nil "ellama" nil "llm" nil "plz" nil "spinner" nil "eww" nil "go-translate" nil "dictionary" nil "connection" nil "link" nil "goldendict" nil "devdocs-browser" nil "elfeed" nil "pdf-tools" nil "tablist" nil "let-alist" nil "eat" nil "vterm" nil "notmuch" nil "mu4e" nil "erc" nil "eldoc-box" nil "json-mode" nil "json-snatcher" nil "jsonrpc" nil "treemacs" nil "pfuture" nil "hydra" nil "lv" nil "cfrs" nil "duplicate-thing" nil "breadcrumb" nil "rmsbolt" nil "parrot" nil "compile" nil "php-mode" nil "yaml-mode" nil "json-reformat" nil "flymake-diagnostic-at-point" nil "magit" nil "magit-section" nil "magit-filenotify" nil "sqlite3" nil "diff-hl" nil "treesit-auto" nil "js" nil "typescript-ts-mode" nil "eglot" nil "external-completion" nil "track-changes" nil "dape" nil "auto-virtualenv" nil "pyvenv" nil "lldb-voltron" nil "rust-mode" nil "rustic" nil "markdown-mode" nil "xterm-color" nil "flycheck" nil "c-ts-mode" nil "logview" nil "datetime" nil "extmap" nil "strace-mode" nil "elf-mode" nil "rgr-kill-dwim" nil "package-lint" nil "helpful" nil "elisp-refs" nil "el-docstring-sap" nil "quick-peek" nil "edebug-x" nil "elisp-format" nil "modus-themes" nil "mu4e-alert" nil "mu4e-column-faces" nil)) melpa #s(hash-table size 217 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "nongnu-elpa" nil "el-get" (el-get :type git :flavor melpa :files (:defaults "methods" ("recipes" "recipes/el-get.rcp") "el-get-pkg.el") :host github :repo "dimitri/el-get") "emacsmirror-mirror" nil "straight" nil "notifications" nil "no-littering" (no-littering :type git :flavor melpa :host github :repo "emacscollective/no-littering") "compat" nil "seq" nil "project" nil "xref" nil "flymake" nil "eldoc" nil "pass" (pass :type git :flavor melpa :host github :repo "NicolasPetton/pass") "password-store" (password-store :type git :flavor melpa :files ("contrib/emacs/*.el" "password-store-pkg.el") :host github :repo "zx2c4/password-store") "with-editor" (with-editor :type git :flavor melpa :host github :repo "magit/with-editor") "password-store-otp" (password-store-otp :type git :flavor melpa :host github :repo "volrath/password-store-otp.el") "s" (s :type git :flavor melpa :host github :repo "magnars/s.el") "f" (f :type git :flavor melpa :host github :repo "rejeep/f.el") "dash" (dash :type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el") "gif-screencast" (gif-screencast :type git :flavor melpa :host gitlab :repo "Ambrevar/emacs-gif-screencast") "lazy-lang-learn" nil "google-translate" (google-translate :type git :flavor melpa :host github :repo "atykhonov/google-translate") "popup" (popup :type git :flavor melpa :host github :repo "auto-complete/popup-el") "alert" (alert :type git :flavor melpa :host github :repo "jwiegley/alert") "gntp" (gntp :type git :flavor melpa :host github :repo "tekai/gntp.el") "log4e" (log4e :type git :flavor melpa :host github :repo "aki2o/log4e") "cl-lib" nil "repeat" nil "browse-url-dwim" (browse-url-dwim :type git :flavor melpa :host github :repo "rolandwalker/browse-url-dwim") "string-utils" (string-utils :type git :flavor melpa :host github :repo "rolandwalker/string-utils") "list-utils" (list-utils :type git :flavor melpa :host github :repo "rolandwalker/list-utils") "posframe" (posframe :type git :flavor melpa :host github :repo "tumashu/posframe") "ace-window" (ace-window :type git :flavor melpa :host github :repo "abo-abo/ace-window") "avy" (avy :type git :flavor melpa :host github :repo "abo-abo/avy") "ace-link" (ace-link :type git :flavor melpa :host github :repo "abo-abo/ace-link") "ace-jump-mode" (ace-jump-mode :type git :flavor melpa :host github :repo "winterTTr/ace-jump-mode") "golden-ratio" (golden-ratio :type git :flavor melpa :host github :repo "roman/golden-ratio.el") "pulsar" nil "blackout" (blackout :type git :flavor melpa :host github :repo "radian-software/blackout") "boxquote" (boxquote :type git :flavor melpa :host github :repo "davep/boxquote.el") "dpaste" (dpaste :type git :flavor melpa :host github :repo "gregnewman/dpaste.el") "darkroom" nil "tab-bar" nil "bookmark+" nil "emojify" (emojify :type git :flavor melpa :files (:defaults "data" "images" "emojify-pkg.el") :host github :repo "iqbalansari/emacs-emojify") "ht" (ht :type git :flavor melpa :host github :repo "Wilfred/ht.el") "multiple-cursors" (multiple-cursors :type git :flavor melpa :host github :repo "magnars/multiple-cursors.el") "hideshow" nil "jinx" (jinx :type git :flavor melpa :files (:defaults "jinx-mod.c" "emacs-module.h" "jinx-pkg.el") :host github :repo "minad/jinx") "ripgrep" (ripgrep :type git :flavor melpa :files ("ripgrep.el" "ripgrep-pkg.el") :host github :repo "nlamirault/ripgrep.el") "sudo-edit" (sudo-edit :type git :flavor melpa :host github :repo "nflath/sudo-edit") "ffap" nil "prescient" (prescient :type git :flavor melpa :files ("prescient.el" "prescient-pkg.el") :host github :repo "radian-software/prescient.el") "consult" (consult :type git :flavor melpa :host github :repo "minad/consult") "marginalia" (marginalia :type git :flavor melpa :host github :repo "minad/marginalia") "all-the-icons" (all-the-icons :type git :flavor melpa :files (:defaults "data" "all-the-icons-pkg.el") :host github :repo "domtronn/all-the-icons.el") "all-the-icons-completion" (all-the-icons-completion :type git :flavor melpa :host github :repo "iyefrat/all-the-icons-completion") "all-the-icons-dired" (all-the-icons-dired :type git :flavor melpa :host github :repo "wyuenho/all-the-icons-dired") "corfu" (corfu :type git :flavor melpa :files (:defaults "extensions/corfu-*.el" "corfu-pkg.el") :host github :repo "minad/corfu") "orderless" (orderless :type git :flavor melpa :host github :repo "oantolin/orderless") "dabbrev" nil "cape" (cape :type git :flavor melpa :host github :repo "minad/cape") "which-key" (which-key :type git :flavor melpa :host github :repo "justbur/emacs-which-key") "yasnippet" (yasnippet :type git :flavor melpa :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :host github :repo "joaotavora/yasnippet") "yasnippet-snippets" (yasnippet-snippets :type git :flavor melpa :files ("*.el" "snippets" ".nosearch" "yasnippet-snippets-pkg.el") :host github :repo "AndreaCrotti/yasnippet-snippets") "vertico" (vertico :type git :flavor melpa :files (:defaults "extensions/vertico-*.el" "vertico-pkg.el") :host github :repo "minad/vertico") "ob-async" (ob-async :type git :flavor melpa :host github :repo "astahlman/ob-async") "async" (async :type git :flavor melpa :host github :repo "jwiegley/emacs-async") "org-super-agenda" (org-super-agenda :type git :flavor melpa :host github :repo "alphapapa/org-super-agenda") "ts" (ts :type git :flavor melpa :host github :repo "alphapapa/ts.el") "ox-gfm" (ox-gfm :type git :flavor melpa :host github :repo "larstvei/ox-gfm") "gptel" (gptel :type git :flavor melpa :host github :repo "karthink/gptel") "transient" (transient :type git :flavor melpa :host github :repo "magit/transient") "ellama" (ellama :type git :flavor melpa :host github :repo "s-kostyaev/ellama") "llm" nil "plz" nil "spinner" nil "eww" nil "go-translate" (go-translate :type git :flavor melpa :host github :repo "lorniu/go-translate") "dictionary" (dictionary :type git :flavor melpa :files ("dictionary.el" "dictionary-pkg.el") :host github :repo "myrkr/dictionary-el") "connection" (connection :type git :flavor melpa :files ("connection.el" "connection-pkg.el") :host github :repo "myrkr/dictionary-el") "link" (link :type git :flavor melpa :files ("link.el" "link-pkg.el") :host github :repo "myrkr/dictionary-el") "goldendict" nil "devdocs-browser" (devdocs-browser :type git :flavor melpa :host github :repo "blahgeek/emacs-devdocs-browser") "elfeed" (elfeed :type git :flavor melpa :files (:defaults "README.md" "elfeed-pkg.el") :host github :repo "skeeto/elfeed") "pdf-tools" (pdf-tools :type git :flavor melpa :files (:defaults "README" ("build" "Makefile") ("build" "server") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools") "tablist" (tablist :type git :flavor melpa :host github :repo "emacsorphanage/tablist") "let-alist" nil "eat" nil "vterm" (vterm :type git :flavor melpa :files ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el" "vterm-module.c" "vterm-module.h" "vterm-pkg.el") :host github :repo "akermu/emacs-libvterm") "notmuch" (notmuch :type git :flavor melpa :files ("emacs/*.el" "emacs/*.svg" "notmuch-pkg.el") :repo "https://git.notmuchmail.org/git/notmuch") "mu4e" nil "erc" nil "eldoc-box" (eldoc-box :type git :flavor melpa :host github :repo "casouri/eldoc-box") "json-mode" (json-mode :type git :flavor melpa :host github :repo "json-emacs/json-mode") "json-snatcher" (json-snatcher :type git :flavor melpa :host github :repo "Sterlingg/json-snatcher") "jsonrpc" nil "treemacs" (treemacs :type git :flavor melpa :files (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el" "src/scripts/treemacs*.py" (:exclude "src/extra/*") "treemacs-pkg.el") :host github :repo "Alexander-Miller/treemacs") "pfuture" (pfuture :type git :flavor melpa :host github :repo "Alexander-Miller/pfuture") "hydra" (hydra :type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra") "lv" (lv :type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra") "cfrs" (cfrs :type git :flavor melpa :host github :repo "Alexander-Miller/cfrs") "duplicate-thing" (duplicate-thing :type git :flavor melpa :host github :repo "ongaeshi/duplicate-thing") "breadcrumb" nil "rmsbolt" (rmsbolt :type git :flavor melpa :files (:defaults "starters" "rmsbolt-pkg.el") :host gitlab :repo "jgkamat/rmsbolt") "parrot" (parrot :type git :flavor melpa :files (:defaults "img" "parrot-pkg.el") :host github :repo "dp12/parrot") "compile" nil "php-mode" (php-mode :type git :flavor melpa :host github :repo "emacs-php/php-mode") "yaml-mode" (yaml-mode :type git :flavor melpa :host github :repo "yoshiki/yaml-mode") "json-reformat" (json-reformat :type git :flavor melpa :host github :repo "gongo/json-reformat") "flymake-diagnostic-at-point" (flymake-diagnostic-at-point :type git :flavor melpa :host github :repo "meqif/flymake-diagnostic-at-point") "magit" (magit :type git :flavor melpa :files ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "magit-pkg.el" (:exclude "lisp/magit-section.el") "magit-pkg.el") :host github :repo "magit/magit") "magit-section" (magit-section :type git :flavor melpa :files ("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el" "magit-section-pkg.el") :host github :repo "magit/magit") "magit-filenotify" (magit-filenotify :type git :flavor melpa :host github :repo "ruediger/magit-filenotify") "sqlite3" (sqlite3 :type git :flavor melpa :files (:defaults "Makefile" "consts.c" "emacs-module.h" "sqlite3-api.c" "sqlite3-pkg.el") :host github :repo "pekingduck/emacs-sqlite3-api") "diff-hl" (diff-hl :type git :flavor melpa :host github :repo "dgutov/diff-hl") "treesit-auto" (treesit-auto :type git :flavor melpa :host github :repo "renzmann/treesit-auto") "js" nil "typescript-ts-mode" nil "eglot" nil "external-completion" nil "track-changes" nil "dape" nil "auto-virtualenv" (auto-virtualenv :type git :flavor melpa :host github :repo "marcwebbie/auto-virtualenv") "pyvenv" (pyvenv :type git :flavor melpa :host github :repo "jorgenschaefer/pyvenv") "lldb-voltron" nil "rust-mode" (rust-mode :type git :flavor melpa :host github :repo "rust-lang/rust-mode") "rustic" (rustic :type git :flavor melpa :host github :repo "emacs-rustic/rustic") "markdown-mode" (markdown-mode :type git :flavor melpa :host github :repo "jrblevin/markdown-mode") "xterm-color" (xterm-color :type git :flavor melpa :host github :repo "atomontage/xterm-color") "flycheck" (flycheck :type git :flavor melpa :host github :repo "flycheck/flycheck") "c-ts-mode" nil "logview" (logview :type git :flavor melpa :host github :repo "doublep/logview") "datetime" (datetime :type git :flavor melpa :files (:defaults "*.extmap" "datetime-pkg.el") :host github :repo "doublep/datetime") "extmap" (extmap :type git :flavor melpa :host github :repo "doublep/extmap") "strace-mode" (strace-mode :type git :flavor melpa :host github :repo "pkmoore/strace-mode") "elf-mode" (elf-mode :type git :flavor melpa :host github :repo "abo-abo/elf-mode") "rgr-kill-dwim" nil "package-lint" (package-lint :type git :flavor melpa :files (:defaults "data" (:exclude "*flymake.el") "package-lint-pkg.el") :host github :repo "purcell/package-lint") "helpful" (helpful :type git :flavor melpa :host github :repo "Wilfred/helpful") "elisp-refs" (elisp-refs :type git :flavor melpa :files (:defaults (:exclude "elisp-refs-bench.el") "elisp-refs-pkg.el") :host github :repo "Wilfred/elisp-refs") "el-docstring-sap" nil "quick-peek" (quick-peek :type git :flavor melpa :host github :repo "cpitclaudel/quick-peek") "edebug-x" (edebug-x :type git :flavor melpa :host github :repo "ScottyB/edebug-x") "elisp-format" (elisp-format :type git :flavor melpa :host github :repo "Yuki-Inoue/elisp-format") "modus-themes" (modus-themes :type git :flavor melpa :host github :repo "protesilaos/modus-themes") "mu4e-alert" (mu4e-alert :type git :flavor melpa :host github :repo "xzz53/mu4e-alert") "mu4e-column-faces" (mu4e-column-faces :type git :flavor melpa :host github :repo "Alexander-Miller/mu4e-column-faces"))) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "nongnu-elpa" nil "emacsmirror-mirror" nil "straight" nil "notifications" nil "compat" (compat :type git :host github :repo "emacs-straight/compat" :files ("*" (:exclude ".git"))) "seq" (seq :type git :host github :repo "emacs-straight/seq" :files ("*" (:exclude ".git"))) "project" (project :type git :host github :repo "emacs-straight/project" :files ("*" (:exclude ".git"))) "xref" (xref :type git :host github :repo "emacs-straight/xref" :files ("*" (:exclude ".git"))) "flymake" (flymake :type git :host github :repo "emacs-straight/flymake" :files ("*" (:exclude ".git"))) "eldoc" (eldoc :type git :host github :repo "emacs-straight/eldoc" :files ("*" (:exclude ".git"))) "lazy-lang-learn" nil "cl-lib" nil "repeat" nil "pulsar" (pulsar :type git :host github :repo "emacs-straight/pulsar" :files ("*" (:exclude ".git"))) "darkroom" (darkroom :type git :host github :repo "emacs-straight/darkroom" :files ("*" (:exclude ".git"))) "tab-bar" nil "bookmark+" nil "hideshow" nil "ffap" nil "dabbrev" nil "llm" (llm :type git :host github :repo "emacs-straight/llm" :files ("*" (:exclude ".git"))) "plz" (plz :type git :host github :repo "emacs-straight/plz" :files ("*" (:exclude ".git"))) "spinner" (spinner :type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git"))) "eww" nil "goldendict" nil "let-alist" (let-alist :type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git"))) "eat" nil "mu4e" nil "erc" (erc :type git :host github :repo "emacs-straight/erc" :files ("*" (:exclude ".git"))) "jsonrpc" (jsonrpc :type git :host github :repo "emacs-straight/jsonrpc" :files ("*" (:exclude ".git"))) "breadcrumb" (breadcrumb :type git :host github :repo "emacs-straight/breadcrumb" :files ("*" (:exclude ".git"))) "compile" nil "js" nil "typescript-ts-mode" nil "eglot" (eglot :type git :host github :repo "emacs-straight/eglot" :files ("*" (:exclude ".git"))) "external-completion" (external-completion :type git :host github :repo "emacs-straight/external-completion" :files ("*" (:exclude ".git"))) "track-changes" (track-changes :type git :host github :repo "emacs-straight/track-changes" :files ("*" (:exclude ".git"))) "dape" (dape :type git :host github :repo "emacs-straight/dape" :files ("*" (:exclude ".git"))) "lldb-voltron" nil "c-ts-mode" nil "rgr-kill-dwim" nil "el-docstring-sap" nil)) nongnu-elpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 4 "emacsmirror-mirror" nil "straight" nil "notifications" nil "lazy-lang-learn" nil "cl-lib" nil "repeat" nil "tab-bar" nil "bookmark+" nil "hideshow" nil "ffap" nil "dabbrev" nil "eww" nil "goldendict" nil "eat" (eat :repo "https://codeberg.org/akib/emacs-eat") "mu4e" nil "compile" nil "js" nil "typescript-ts-mode" nil "lldb-voltron" nil "c-ts-mode" nil "rgr-kill-dwim" nil "el-docstring-sap" nil)) el-get #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "emacsmirror-mirror" nil "straight" nil "notifications" nil "lazy-lang-learn" nil "cl-lib" nil "repeat" nil "tab-bar" nil "bookmark+" `(bookmark+ :type git :host github :repo "emacsmirror/bookmark-plus" :files (:defaults)) "hideshow" nil "ffap" nil "dabbrev" nil "eww" nil "goldendict" nil "mu4e" `(mu4e :type git :host github :repo "djcb/mu" :pre-build ,(cl-letf (((symbol-function #'el-get-package-directory) (lambda (package) (straight--repos-dir (format "%S" package)))) (el-get-install-info (straight--el-get-install-info)) (el-get-emacs (straight--emacs-path)) (el-get-dir (straight--repos-dir))) (pcase system-type (_ `(("./autogen.sh") ("make"))))) :files (:defaults "build/mu4e")) "compile" nil "js" nil "typescript-ts-mode" nil "lldb-voltron" nil "c-ts-mode" nil "rgr-kill-dwim" nil "el-docstring-sap" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "notifications" nil "lazy-lang-learn" nil "cl-lib" nil "repeat" nil "tab-bar" nil "hideshow" nil "ffap" nil "dabbrev" nil "eww" nil "goldendict" (goldendict :type git :host github :repo "emacsmirror/goldendict") "compile" nil "js" nil "typescript-ts-mode" nil "lldb-voltron" nil "c-ts-mode" nil "rgr-kill-dwim" nil "el-docstring-sap" nil))))

("modus-themes" "elisp-format" "edebug-x" "quick-peek" "el-docstring-sap" "elisp-refs" "helpful" "package-lint" "rgr-kill-dwim" "elf-mode" "strace-mode" "extmap" "datetime" "logview" "c-ts-mode" "flycheck" "xterm-color" "markdown-mode" "rustic" "rust-mode" "lldb-voltron" "pyvenv" "auto-virtualenv" "dape" "track-changes" "external-completion" "eglot" "typescript-ts-mode" "js" "treesit-auto" "diff-hl" "sqlite3" "magit-filenotify" "magit-section" "magit" "flymake-diagnostic-at-point" "json-reformat" "yaml-mode" "php-mode" "compile" "parrot" "rmsbolt" "breadcrumb" "duplicate-thing" "cfrs" "lv" "hydra" "pfuture" "treemacs" "jsonrpc" "json-snatcher" "json-mode" "eldoc-box" "erc" "mu4e" "notmuch" "vterm" "eat" "let-alist" "tablist" "pdf-tools" "elfeed" "devdocs-browser" "goldendict" "link" "connection" "dictionary" "go-translate" "eww" "spinner" "plz" "llm" "ellama" "transient" "gptel" "ox-gfm" "ts" "org-super-agenda" "async" "ob-async" "org-contrib" "vertico" "yasnippet-snippets" "yasnippet" "which-key" "cape" "dabbrev" "orderless" "corfu" "all-the-icons-dired" "all-the-icons-completion" "all-the-icons" "marginalia" "consult" "prescient" "ffap" "sudo-edit" "ripgrep" "jinx" "hideshow" "multiple-cursors" "ht" "emojify" "bookmark+" "tab-bar" "darkroom" "dpaste" "boxquote" "blackout" "pulsar" "golden-ratio" "ace-jump-mode" "ace-link" "avy" "ace-window" "posframe" "list-utils" "string-utils" "browse-url-dwim" "repeat" "cl-lib" "log4e" "gntp" "alert" "popup" "google-translate" "lazy-lang-learn" "gif-screencast" "dash" "f" "s" "password-store-otp" "with-editor" "password-store" "pass" "eldoc" "flymake" "xref" "project" "org" "seq" "compat" "no-littering" "notifications" "emacs" "straight" "emacsmirror-mirror" "el-get" "nongnu-elpa" "gnu-elpa-mirror" "melpa" "org-elpa")

t
