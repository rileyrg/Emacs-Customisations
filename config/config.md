- [Project version control, git, .gitignore](#Project-version-control)
- [Building Emacs from source.](#org7bd3bde)
  - [Cloning from github](#org4b9b67b)
  - [compiling and adding symlinks on $PATH](#org81f0480)
- [Setting up emacs as a default editor using a dot desktop file and associated protocol handler](#emacs-default-editor)
    - [php.ini changes e.g /etc/php/7.3/php.ini](#org76106d7)
    - [emacsclient-linenumber script to correctly parse the protocol in order to start emacs frame at correct line](#org0ed0d62)
    - [Gnome protocol handler desktop file](#orgb6c13bd)
    - [Always using emacsclient](#orgc4c8797)
- [package management with straight.el](#package-management)
  - [straight.el bootstrap](#org889838f)
  - [use-package works with straight.el](#org7e39923)
    - [use-package helper](#org5a71c40)
- [Org-Babel and init.el tangling](#org-babel)
- [Elisp library and custom-paths](#libraries-and-paths)
- [Emacs daemon & startup](#emacs-daemon)
- [Music](#org17663f4)
- [Utility functions](#utility-functions)
  - [toggle buffer](#org1aac430)
  - [scratch](#org5421ef1)
  - [external utilities](#orgfa85d3b)
    - [helpful](#orgba463ff)
- [Configure main look and feel](#look-and-feel)
  - [General](#orga7cc244)
  - [Clipboard](#org11d697c)
  - [Ansi colour](#orgbc17d4f)
  - [Point History](#org7b7c8d7)
- [Shells](#shells)
  - [Shell Switcher](#orgab95338)
  - [Eshell](#org082c354)
    - [Eshell functions](#org79062e5)
    - [EShell Aliases](#org1b2ac92)
    - [EShell config with Helm](#orgf8bf05b)
  - [Docker](#org4be43c3)
    - [docker](#org2446355)
- [Helm, helm-mode](#helm-management)
- [Projectile](#projectile-project-management)
- [Buffers and Windows](#buffers-and-windows)
  - [General](#orgfe5fb06)
  - [Auto edit buffer as root](#orgc2853d3)
  - [iBuffer](#org6cbe2e7)
  - [dired - emacs file management](#orge80f300)
    - [Dired Git Info](#org201c2dd)
    - [dired hacks](#org1b1490a)
  - [PosFrame](#orga5159ee)
  - [PopWin,Popwin makes you free from the hell of annoying buffers such like **Help**, **Completions**, **compilation**, and etc.](#orgcc43796)
  - [Transpose windows, transpose-frame](#orgda30346)
  - [Hyperbole](#org93b453f)
  - [Undo utilities](#orgf16ebd5)
    - [undohist](#org5a7fd4f)
    - [undo-tree](#orge02fa82)
    - [undo-fu](#org0ff9ac0)
  - [Navigation](#orgd255350)
    - [Back Button](#org2e0c007)
    - [Window hopping](#orgca93033)
    - [hopping around links](#org790a702)
    - [hopping around in the buffer](#org8649881)
  - [Elscreen](#orga7f2bd9)
- [Centaur Tabs](#orgb97460d)
  - [Darkroom](#org6ce3835)
  - [Outline Mode](#org2c02826)
- [Text tools](#text-tools)
  - [Cursor/Region related](#org0afdd5f)
    - [General](#org17bb1ee)
    - [expand-region](#org66318a1)
    - [easy-kill](#org64123d4)
  - [Folding/Hide Show](#orgb6f1138)
  - [flyspell](#orgf40cc2d)
  - [Completion](#org0d1c936)
    - [Snippets with yasnippet](#org3ac2799)
    - [Company Mode](#org1fca276)
    - [Which Key](#org6fd010c)
    - [Tying it call together](#org0d63351)
  - [Searching non-helm](#org5b111d8)
  - [Abbrev Mode](#org9cf56fb)
  - [Deft - text searching](#orgeaf3885)
  - [Google This](#org8ac6a81)
  - [Reference and dictionary](#orgb460d03)
    - [utility funcs](#orgd873cab)
    - [Dictionary](#orgb28b7d4)
    - [Elisp reference](#org119c3a5)
    - [GoldenDict - external lookup and reference](#org4a74867)
    - [DevDocs](#org6a865fe)
    - [Zeal - Linux Dash](#org0328ea9)
    - [DASH - API documentation for most languages](#orgf0d0ea9)
- [Treemacs](#treemacs)
- [Alerts](#alerts)
- [Web](#web)
  - [helper functions](#orgfe1f15c)
  - [W3M - emacs text based web browser](#orga23a146)
  - [EWW - emacs text based web browser](#org3ff74a7)
    - [open tasks :tangle no](#orgdd8b2ae)
    - [code](#orgf79c4ab)
- [Online Chats](#online-chats)
    - [Only one chat instance](#org75bdb29)
  - [Slackware](#org1cabcdb)
    - [Emacs Slack](#orge5e327b)
    - [Emacs Gitter](#org2f668ef)
- [Org functionality](#org-mode)
  - [Org Mode, org-mode](#org1645101)
    - [config](#org9e31f61)
    - [Journal, org-journal](#org242fc6f)
    - [ROAM note taking, org-roam](#org655d3ce)
    - [Authoring in org-mode](#orgf989e92)
    - [Passwords, org-password-manager](#org452e23c)
  - [Self documenting config file](#orge41f411)
- [Email, gmail, Gnus](#email)
- [Screen recording](#orgf91223e)
  - [Emacs screencasts](#org43b1577)
- [Pomodoro](#pomodoro)
- [Programming related](#programming)
  - [General](#orge3757bf)
  - [Symfony](#org66885b3)
    - [custom](#org23b2d08)
    - [Start a symfony web server when applicable](#orgb68fb5c)
  - [Emacs Lisp, ELisp](#org888053a)
    - [refactoring utlities](#orgc019fea)
    - [query symbol](#orgaa13d3f)
    - [Elisp completion and debugging](#org9648fd4)
    - [Auto-compile](#orge8665d1)
  - [JSON, YAML Configuration files](#org7926a6e)
    - [JSON Editing](#org7065346)
    - [YAML](#orgb72c28d)
  - [Flycheck](#org4fc0ee5)
  - [Version Control](#org0667ea4)
    - [It's [Magit](https://github.com/magit/magit)! A Git porcelain inside Emacs](#orge6d9ed2)
    - [[Forge](https://github.com/magit/forge) ahead with Pull Requests](#orgc3b2358)
  - [Javascript](#orgfd1bbac)
  - [RJSX](#org8d19d86)
  - [Typescript](#orgd4bf2ec)
  - [Tide Mode](#org75ef7ff)
  - [Language Server Protocol (LSP)](#orgef6c34f)
  - [Serial Port](#org480fbea)
  - [PlatformIO](#org6a352f4)
  - [C](#orgaf94b8e)
    - [Clang provides us with some industry standard code prettiers](#orga6b6fcf)
    - [C  modes hooks](#org06bfb4f)
  - [C++](#org8092b7e)
  - [C#](#org7f9875c)
  - [Godot GDScript](#org0924142)
  - [tasks :tangle no](#org6cc5c2b)
  - [PHP Mode](#org98546de)
  - [Web,Symfony and Twig](#org43d4dcd)
    - [The Code](#orga12b7e6)
  - [elf-mode - view the symbol list in a binary](#org88bee53)
  - [EDiff - comparing files in Emacs](#org8a8d9f3)
- [Macros & Utilities](#macros)
  - [move to end of line, add a semi colon and move to next line](#org28367f8)
- [Privacy](#privacy)
- [Host specific setting](#host-specifics)
- [Themes](#screensaver-themes)
  - [Themes](#orgcab3e13)



<a id="Project-version-control"></a>

# Project version control, git, .gitignore

I like to exclude everything and then add in what is important. So the first line of my [gitignore](file:///home/rgr/.emacs.d/.gitignore) is "\*".

```git
*
*.*

!.gitignore
!git_config.backup
!init.el
!init-erc.el
!straight-bootstrap.el
!README.md
!plan.org
!.ignore
!.projectile

!/config
!/config/*

!/editor-config
!/editor-config/*

!/eshell
!/eshell/alias

!/exwm
!/exwm/*

!/elisp
!/elisp/rgr-*.el

!/elisp/hosts
!/elisp/hosts/*.el

!/elisp/lessons
!/elisp/lessons/*.org

!/images
!/images/*

!/straight
!/straight/versions
!/straight/versions/*

!/linux-init
!/linux-init/*
!/linux-init/DotFiles/
!/linux-init/DotFiles/*

!/linux-init/test-files/
!/linux-init/test-files/*
!/linux-init/test-files/subdir
!/linux-init/test-files/subdir/*


!/build
!/build/scripts
!/build/scripts/*
```


<a id="org7bd3bde"></a>

# Building Emacs from source.


<a id="org4b9b67b"></a>

## Cloning from github

You can clone emacs from the the [github emacs mirror](https://github.com/emacs-mirror/emacs). to create an emacs development hierarchy and then checkout (in this instance) emacs 27 (I keep emacs source tree unde my emacs.d in a build directory):

```bash
cd ~/.emacs.d/build
git clone git@github.com:emacs-mirror/emacs.git
git checkout emacs-27
```


<a id="org81f0480"></a>

## compiling and adding symlinks on $PATH

My own build uses the gtk toolkit and stores the build under my $HOME/bin directory.

It goes without saying you need to ensure your $HOME/bin is on the path before any system installed emacs.

```bash
#!/usr/bin/bash
# add this to your path or symlink it from your bin directory
cd ~/bin/thirdparty/emacs
./configure --prefix="/usr/local"  --with-x-toolkit=yes
make && sudo make install
```

More info can be sourced starting from the EmacsWiki [here.](https://www.emacswiki.org/emacs/BuildingEmacs)


<a id="emacs-default-editor"></a>

# Setting up emacs as a default editor using a dot desktop file and associated protocol handler


<a id="org76106d7"></a>

### php.ini changes e.g /etc/php/7.3/php.ini

`xdebug.file_link_format` is used by compliant apps to format a protocol uri. This is handled on my Linux system as a result of [emacsclient.desktop](editor-config/emacsclient.desktop) documented below.

```eshell
xdebug.file_link_format = "emacsclient://%f@%l"

xdebug.remote_enable = 1
xdebug.remote_host = localhost
xdebug.remote_port = 9000
```


<a id="org0ed0d62"></a>

### emacsclient-linenumber script to correctly parse the protocol in order to start emacs frame at correct line

```bash
#!/usr/bin/bash
export GDK_NATIVE_WINDOWS=1
file=`echo "$@" | sed 's/.*://;s/@.*$//'`
line=`echo "$@" | sed 's/.*@//'`
echo $file
echo $line

re='^[0-9]+$'
if ! [[ $line =~ $re ]] ; then
    exec emacsclient -c -a ""  $file
else
    exec emacsclient -c -a "" +$line  $file
fi
```


<a id="orgb6c13bd"></a>

### Gnome protocol handler desktop file

Copy [emacsclient.desktop](editor-config/emacsclient.desktop) to ~/.local/share/applications (Debian & Gnome - your mileage may vary&#x2026;)

```shell
[Desktop Entry]
Name=Emacs (Client)
GenericName=Text Editor
Comment=Edit text
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=emacsclient -c -a "" %F
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;Utility;
StartupWMClass=Emacs
```

Update the desktop database:

```bash
update-desktop-database ~/.local/share/applications
```

now allows emacsclient to be called.

note: symlink the deprecated [~/.local/share/applications/mimeapps.lst](https://wiki.archlinux.org/index.php/XDG_MIME_Applications) to the one in ~/.config

```bash
ln -sf  ~/config/mimeapps.lst ~/.local/share/applications/
```


<a id="orgc4c8797"></a>

### Always using emacsclient

Set up a zshrc alias so that "emacs" actually invokes emacs client. In my .zshrc:

```shell
alias emacs='emacsclient --create-frame --alternate-editor=""'
```


<a id="package-management"></a>

# package management with straight.el

I've started using [straight.el](https://github.com/raxod502/straight.el) which, in it's own words:-

> -   Init-file and version lockfiles as the sole source of truth. No persistent state kept elsewhere.
> -   100% reproducible package management, accounting for changes in packages, recipe repositories, configuration, and the package manager itself.
> -   No support whatsoever for package.el.
> -   Edit packages by editing their code, no extra steps required. Allow for manual version control operations.
> -   Compatibility with MELPA, GNU ELPA, and Emacsmirror.
> -   Trivial to quickly try out a package without permanently installing it.
> -   Good for reproducing an issue with emacs -Q.

A very useful feature is the ability to "freeze" your setup so that later, should newer packages leave your config broken, you can revert to the stable suite. See [Configuration reproducibility](https://github.com/raxod502/straight.el#configuration-reproducibility) for more details.


<a id="org889838f"></a>

## straight.el bootstrap

[straight-bootstrap.el](../straight-bootstrap.el) contains the bootstrap documented [here](https://github.com/raxod502/straight.el#bootstrapping-straightel).

```emacs-lisp
(defvar bootstrap-version)
(setq straight-repository-branch "develop")

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; code below just a test to show use of straight-get-recipe
(straight-use-package '(use-package :type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package"))
(setq straight-use-package-by-default t)

(provide 'straight-bootstrap)
```


<a id="org7e39923"></a>

## use-package works with straight.el

The [use-package](https://github.com/jwiegley/use-package) wrapper still works just fine. In [straight-bootstrap.el](./straight-bootstrap.el) see:-

```emacs-lisp
(straight-use-package
 '(use-package
    :type git
    :flavor melpa
    :files (:defaults (:exclude "bind-key.el"
                                "bind-chord.el"
                                "use-package-chords.el"
                                "use-package-ensure-system-package.el")
                      "use-package-pkg.el")
    :host github
    :repo "jwiegley/use-package"))
(setq straight-use-package-by-default t)
```


<a id="org5a71c40"></a>

### use-package helper

```emacs-lisp
(straight-use-package 'diminish)
```


<a id="org-babel"></a>

# Org-Babel and init.el tangling

The [init.el](./init.el) reads an org-mode config file and extracts the elisp source blocks to create the actual configuration that Emacs executes at startup.

```emacs-lisp
;; add host specific library directores to load path
(load (expand-file-name "straight-bootstrap.el" user-emacs-directory ))
;;
(push (concat user-emacs-directory "elisp") load-path )
(push (concat user-emacs-directory "elisp/hosts") load-path )

;; (use-package straight
;;   :custom
;;   (straight-use-package-by-default t)
;;   (straight-vc-git-default-protocol 'git)
;;   (straight-vc-git-force-protocol t))

(use-package org
  :straight org-plus-contrib)

(defun init-load()
  (interactive)
  (org-babel-load-file (expand-file-name "config/config.org" user-emacs-directory))
  (setq custom-file (expand-file-name "config/custom.el" user-emacs-directory ))
  (load custom-file t)
  (load-host-customisation)
  (message "init.el successfully completed"))

(init-load)

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'init)

(put 'scroll-left 'disabled nil)
```


<a id="libraries-and-paths"></a>

# Elisp library and custom-paths

I have a separate Elisp custom-paths.el file to add paths to stuff that overrides the melpa packages. This file is not in git. My custom-paths is something of the form

```emacs-lisp
;;(add-to-list 'load-path "~/.emacs.d/lisp/dap-mode")
(provide 'custom-paths)
```

It's loaded early so that the load-path is correct prior to package loading.

```emacs-lisp
;; don't complain if custom-paths.el doesn't exist
(require 'custom-paths nil t)
```


<a id="emacs-daemon"></a>

# Emacs daemon & startup

```emacs-lisp

;; start emacs-server if not running
(unless(daemonp)
  (add-hook 'after-init-hook (lambda ()
                               (require 'server)
                               (unless (server-running-p)
                                 (message "Starting EmacsServer from init as not already running.")
                                 (server-start)))))

(defun startHook()
  ;;(desktop-save-mode 1)
  ;;(desktop-read)

  (global-set-key (kbd "S-<f1>") 'describe-face)) ;

(add-hook 'emacs-startup-hook 'startHook)

(defun quit-or-close-emacs(&optional kill)
  (interactive)
  (if (or current-prefix-arg kill)
      (server-shutdown)
    (delete-frame)))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(global-set-key (kbd "C-c x") 'quit-or-close-emacs)
(global-set-key (kbd "C-x C-c") 'nil)
```


<a id="org17663f4"></a>

# Music

[Bongo](https://github.com/dbrock/bongo) is a flexible and usable media player for GNU Emacs. If you store your music collection locally and use structured file names, then Bongo is a great way for you to play music from within Emacs.

```emacs-lisp
(use-package bongo :ensure t)
```


<a id="utility-functions"></a>

# Utility functions


<a id="org1aac430"></a>

## toggle buffer

```emacs-lisp
(defun rgr/toggle-buffer
    (&optional
     n)
  "jump to or from buffer named n else default to *Messages*"
  (interactive)
  (let ((n (or n
               "*Messages*")))
    (switch-to-buffer (if (string= (buffer-name) n)
                          (other-buffer) n))))
```


<a id="org5421ef1"></a>

## scratch

```emacs-lisp
(defun generate-setq-form-function (variable value)
  `(setq ,variable ',(sort (delete-dups (copy-tree value)) #'(lambda (x y)
                                                               (string< (symbol-name x)
                                                                        (symbol-name y))))))

;; (setq x 3)
;; (generate-setq-form-function x '(b e c f a))
```


<a id="orgfa85d3b"></a>

## external utilities

**\*** [help-fns+](https://www.emacswiki.org/emacs/download/help-fns%2b.el) from Drew Adams

<p class="verse">
;;; Commentary:<br />
;;<br />
;;    Extensions to \`help-fns.el'.  Also includes a redefinition of<br />
;;    \`describe-face', which is from \`faces.el'.<br />
;;<br />
;;    Note: As of Emacs 24.4, byte-compiling this file in one Emacs<br />
;;    version and using the compiled file in another Emacs version<br />
;;    does not work.<br />
;;<br />
;;<br />
;;  Keys bound here:<br />
;;<br />
;;    \`C-h B'      \`describe-buffer'<br />
;;    \`C-h c'      \`describe-command'     (replaces \`describe-key-briefly')<br />
;;    \`C-h o'      \`describe-option'<br />
;;    \`C-h C-c'    \`describe-key-briefly' (replaces \`C-h c')<br />
;;    \`C-h C-o'    \`describe-option-of-type'<br />
;;    \`C-h M-c'    \`describe-copying'     (replaces \`C-h C-c')<br />
;;    \`C-h M-f'    \`describe-file'<br />
;;    \`C-h M-k'    \`describe-keymap'<br />
;;    \`C-h M-l'    \`find-function-on-key'<br />
</p>

```emacs-lisp
(use-package help-fns+
  :disabled t
  )
```


<a id="orgba463ff"></a>

### helpful

[Helpful](https://github.com/Wilfred/helpful) is an alternative to the built-in Emacs help that provides much more contextual information.

```emacs-lisp
(use-package helpful

  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

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
  (global-set-key (kbd "C-h C") #'helpful-command)
  )
```


<a id="look-and-feel"></a>

# Configure main look and feel


<a id="orga7cc244"></a>

## General

```emacs-lisp

;; (set-language-environment 'utf-8)
;; (setq default-process-coding-system '(utf-8 . utf-8)) ;; needed this for calling call-process-shell-command
(require 'iso-transl) ;; supposed to cure deadkeys when my external kbd is plugged into my thinkpad T460.  It doesnt.

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(tooltip-mode 1)
(display-time-mode 1)

(global-visual-line-mode 1)

(delete-selection-mode 1)

(save-place-mode 1)
(savehist-mode 1)

(global-set-key (kbd "S-<f10>") #'menu-bar-open)
                                        ;          (global-set-key (kbd "<f10>") #'imenu)


(setq frame-title-format (if (member "-chat" command-line-args)  "Chat: %b" "Emacs: %b")) ;; used to select the window again (frame-list) (selected-frame)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(use-package boxquote
  :straight (:branch "main")
  :bind
  ("C-S-r" . boxquote-region))

(use-package
  browse-url-dwim)

(use-package
  all-the-icons)

(use-package
  webpaste
  :bind ("C-c y" . webpaste-paste-region)
  ("C-c Y" . webpaste-paste-buffer))

;; brings visual feedback to some operations by highlighting portions relating to the operations.
(use-package
  volatile-highlights
  :init (volatile-highlights-mode 1))
;; display dir name when core name clashes
(require 'uniquify)

(global-set-key (kbd "C-c r") 'query-replace-regexp)

```


<a id="org11d697c"></a>

## Clipboard

Allow terminal emacs to interact with the x clipboard.

```emacs-lisp
(use-package xclip
  :demand t
  :config
  (xclip-mode))
```


<a id="orgbc17d4f"></a>

## Ansi colour

[Ansi colour hooks](https://www.emacswiki.org/emacs/AnsiColor) to enable emacs buffers to handle ansi.

```emacs-lisp
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
```


<a id="org7b7c8d7"></a>

## Point History

```emacs-lisp
(require 'pointhistory)
```


<a id="shells"></a>

# Shells


<a id="orgab95338"></a>

## Shell Switcher

[shell-switcher](https://github.com/DamienCassou/shell-switcher) allows easier shell switching.

```emacs-lisp
(use-package
  shell-switcher
  :config (setq shell-switcher-mode t)
  ;;(add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)
  :bind ("<f12>" . projectile-run-eshell)
  ("M-<f12>" . shell-switcher-switch-buffer)
  ("C-<f12>" . shell-switcher-new-shell))
```


<a id="org082c354"></a>

## Eshell

[EShell](https://www.masteringemacs.org/article/complete-guide-mastering-eshell) is, amongst other things, convenient for cat/console debugging in Symfony etc to have all output in easily browsed Emacs buffers via [EShell redirection](https://www.emacswiki.org/emacs/EshellRedirection).


<a id="org79062e5"></a>

### Eshell functions

1.  Bootstrap  clean emacs

    ```emacs-lisp
    (defun eshell/emacs-clean (&rest args)
      "run a clean emacs"
      (interactive)
      (save-window-excursion
        (shell-command "emacs -Q -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el &")))
    ```


<a id="org1b2ac92"></a>

### EShell Aliases

Be sure to check out [Aliases](http://www.howardism.org/Technical/Emacs/eshell.html). Aliases are very powerful allowing you to mix up shell script, elisp raw and elisp library function. My current [alias file](eshell/alias) (subject to change&#x2026;) is currently, at this time of discovery:-

```bash
alias HOME  $*
alias god cd ~/bin/thirdparty/godot
alias in ssh intel-nuc
alias prj cd ~/development/Symfony/the_spacebar/
alias gs git status
alias clconf find ~/Dropbox/ -path "*(*s conflicted copy [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]*" -exec rm -f {} \;
alias sudo eshell/sudo $*
alias twigs grep -i "twig.*"$1 *
alias rgr rg --color=never $* > #<*ripgrep*>; switch-to-buffer "*ripgrep*"
alias resemacshub resgithub.sh "Emacs customisation centered around lsp,helm,dap and projectile."
alias to httplug.client.app.http_methods
alias grep grep --color=always --exclude="*.lock" --exclude-dir=log --exclude-dir=cache -iR $*
alias gg *grep -C 2 -iR $*
alias awg aw|*grep -C 2 -i $*
alias dconp co debug:container  --show-private $*
alias dcon co debug:container  $*
alias dc co debug:config $*
alias cc co cache:clear
alias hg helm-projectile-grep
alias ll ls -l $*
alias aw co debug:autowiring
alias cdu co config:dump $1
alias pg projectile-grep
alias co bin/console --no-ansi $*
alias pff helm-projectile-find-file $*
alias ff helm-projectile-find-file
alias em cd ~/.emacs.d
alias dcg dc $1 |*grep -C 5 -i $2
alias coenv co about
alias R/W multiple sector transfer: Max = 1 Current = 1
alias sp projectile-switch-open-project
alias mlg ag -o -i --no-color -U --smart-case "(?=(?:.|\n)*?$1)(?:.|\n)*?$2" . > #<*mlg*> && switch-to-buffer "*mlg*"
alias csr console server:run
alias dr co debug:router
alias dconparm dcon --parameters
alias cr composer recipes $*
alias property: $*
alias gds cd ~/.emacs.d/straight/repos/emacs-gdscript-mode
```


<a id="orgf8bf05b"></a>

### EShell config with Helm

1.  This setup uses the so called "plan 9" pattern documented [here](https://www.masteringemacs.org/article/complete-guide-mastering-eshell).

    > If smart display is enabled it will also let you review the output of long-running commands by using SPC to move down a page and BACKSPACE to move up a page. If any other key is pressed it will jump the end of the buffer, essentially acting in the same way as if smart display wasn’t enabled.
    >
    > Essentially, if Eshell detects that you want to review the last executed command, it will help you do so; if, on the other hand, you do not then Eshell will jump to the end of the buffer instead. It’s pretty clever about it, and there are switches you can toggle to fine-tune the behavior.

    Added in some pcomplete extensions for git from [Mastering Emacs](https://www.masteringemacs.org/article/pcomplete-context-sensitive-completion-emacs).

    ```emacs-lisp
    (use-package
      eshell
      :init
      (require 'em-hist)
      (require 'em-tramp)
      (require 'em-smart)
      :config
      (defun eshell-mode-hook-func ()
        (setq eshell-path-env (concat "/home/rgr/bin:" eshell-path-env))
        (setenv "PATH" (concat "/home/rgr/bin:" (getenv "PATH")))
         (setq pcomplete-cycle-completions nil))
      (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
      (setq eshell-review-quick-commands nil)
      (setq eshell-smart-space-goes-to-end t)
      (use-package pcomplete-extension
        :config
        (defconst pcmpl-git-commands
          '("add" "bisect" "branch" "checkout" "clone"
            "commit" "diff" "fetch" "grep"
            "init" "log" "merge" "mv" "pull" "push" "rebase"
            "reset" "rm" "show" "status" "tag" )
          "List of `git' commands")

        (defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
          "The `git' command to run to get a list of refs")

        (defun pcmpl-git-get-refs (type)
          "Return a list of `git' refs filtered by TYPE"
          (with-temp-buffer
            (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
            (goto-char (point-min))
            (let ((ref-list))
              (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
                (add-to-list 'ref-list (match-string 1)))
              ref-list)))

        (defun pcomplete/git ()
          "Completion for `git'"
          ;; Completion for the command argument.
          (pcomplete-here* pcmpl-git-commands)
          ;; complete files/dirs forever if the command is `add' or `rm'
          (cond
           ((pcomplete-match (regexp-opt '("add" "rm")) 1)
            (while (pcomplete-here (pcomplete-entries))))
           ;; provide branch completion for the command `checkout'.
           ((pcomplete-match "checkout" 1)
            (pcomplete-here* (pcmpl-git-get-refs "heads")))))    )
      (use-package
        eshell-git-prompt
        :config
        (eshell-git-prompt-use-theme 'powerline)
        (define-advice
            eshell-git-prompt-powerline-dir
            (:override ()
                       short)
          "Show only last directory."
          (file-name-nondirectory (directory-file-name default-directory))))
      :bind (:map eshell-mode-map
                  ;; ([remap eshell-previous-matching-input-from-input] . previous-line)
                  ;; ([remap eshell-next-matching-input-from-input] . next-line)
                  ;;([remap eshell-list-history] . helm-eshell-history)
                  ("C-r" . helm-eshell-history)))
    ```


<a id="org4be43c3"></a>

## Docker


<a id="org2446355"></a>

### docker

A general interface to [docker](https://github.com/Silex/docker.el/tree/a2092b3b170214587127b6c05f386504cae6981b).

```emacs-lisp
(use-package docker
  :ensure t
  :after projectile
  :bind (:map projectile-mode-map ("C-c k" . docker)))
```


<a id="helm-management"></a>

# Helm, helm-mode

Incremental completion/searching and all sorts of wonderful things. [Emacs Incremental Completion.](https://github.com/emacs-helm/helm)

```emacs-lisp
(use-package
  helm
  :custom
  (helm-buffer-max-length 64)
  (helm-candidate-numer 50 t)
  (helm-ff-auto-update-initial-value t)
  ;; (helm-grep-default-command "grep --color=always -a -i -d skip %e -n%cH -e %p %f")
  ;; (helm-grep-default-recurse-command "grep --color=always -i -a -d recurse %e -n%cH -e %p %f")
  ;; (helm-grep-git-grep-command "git --no-pager grep -n%cH --full-name -e %p -- %f")
  (helm-split-window-inside-p t)
  (helm-swoop-move-to-line-cycle t)
  (helm-ff-search-library-in-sexp t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-echo-input-in-header-line t)
  (helm-truncate-lines t)
  (helm-turn-on-recentf t) ;; doesnt work
  ;;      (helm-show-completion-display-function #'helm-show-completion-default-display-function) ; stop using popup frame
  (helm-show-completion-display-function nil) ; stop using popup frame
  :config
  (helm-mode 1)
  ;;(helm-autoresize-mode 1)
  (helm-popup-tip-mode 1)
  (use-package
    helm-ag)
  (use-package
    helm-rg)
  (use-package
    helm-git-grep)
  (use-package
    google-translate)
  (use-package
    helm-swoop
    :config
    (setq helm-swoop-pre-input-function (lambda () "")))
  (use-package
    helm-chrome)
  (use-package
    helm-dictionary)
  (require 'helm-config)
  (global-unset-key (kbd "C-x c"))
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  :bind ("C-c h" . helm-command-prefix)
  ("M-x" . helm-M-x)
  ("C-x b" . helm-mini)
  ("C-s" . helm-swoop)
  ("C-x C-f". helm-find-files)
  ("M-p". helm-show-kill-ring)
  ("C-h SPC" . helm-all-mark-rings)
  ("C-h C-SPC" . helm-global-mark-ring)
  (:map helm-map
        ("C-z" . helm-execute-persistent-action))
  (:map helm-command-map
        (("@" . straight-use-package)
         ("d" . helm-dictionary)
         ("r" . helm-resume)
         ("e" . helm-info-elisp)
         ("g" . helm-google-suggest)
         ("B". helm-chrome-bookmarks)
         ("p". helm-top)
         ("t". google-translate-at-point)
         ("T". google-translate-query-translate)
         ("b". helm-bookmarks)
         ("u". browse-url-dwim)
         ("o" . helm-multi-swoop))))
```


<a id="projectile-project-management"></a>

# Projectile

[Projectile](https://github.com/bbatsov/projectile) is all about being "project aware". Find files, grep and similar are aware of your [project root](https://projectile.readthedocs.io/en/latest/configuration/) making such tasks project local. Can't do without it.

```emacs-lisp
(use-package
  helm-projectile
  :demand
  :custom
  (helm-ag-insert-at-point 'symbol)
  :config
  (use-package helm-ag)
  (defun my-projectile-grep-ag(&optional deepsearch)
    (interactive "P")
  (let((helm-ag--extra-options (if deepsearch "-U" nil)))
    (helm-projectile-ag helm-ag--extra-options)))
  (defun my-projectile-grep-rg(&optional deepsearch)
    (interactive "P")
    (let((helm-rg--extra-args (if deepsearch "--no-ignore-vcs" nil)))
      (helm-projectile-rg)))
  (defun my-projectile-find-file(search)
    (interactive "P")
    (if search
        (projectile-find-file)
      (projectile-find-file-dwim)))
  (helm-projectile-on)
  (projectile-mode 1)
  :bind ("<f2>" . 'projectile-dired)
  ("S-<f3>" . 'helm-do-grep-ag)
  ("<f3>" . 'my-projectile-grep-rg)
  ("<f4>" . 'my-projectile-find-file)
  ("<f5>" . 'helm-projectile-switch-to-buffer)
  (:map projectile-mode-map ("C-c p" . projectile-command-map))
  (:map projectile-command-map ("o"  . helm-multi-swoop-projectile))
  (:map projectile-command-map ("g"  . helm-git-grep)))

```


<a id="buffers-and-windows"></a>

# Buffers and Windows


<a id="orgfe5fb06"></a>

## General

```emacs-lisp
(winner-mode 1)
(recentf-mode 1)
(global-set-key (kbd "C-<f2>") 'rgr/toggle-buffer)
(global-set-key (kbd "C-h d") (lambda()(interactive)(apropos-documentation (symbol-or-region-at-point-as-string-or-prompt))))
(defun kill-next-window ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (if (not (one-window-p))(progn
                            (other-window 1)
                            (kill-this-buffer))
    (message "no next window to kill!")))
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-next-window)
(defun rgr/switch-to-buffer-list (buffer alist)
  (message "in rgr/switch-to-buffer-list")
  (select-window  (display-buffer-use-some-window buffer alist)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
```


<a id="orgc2853d3"></a>

## Auto edit buffer as root

A [package](https://github.com/ncaq/auto-sudoedit) that automatically reopens files with sudo.

```emacs-lisp
(use-package auto-sudoedit
  :demand t
  :config
  (auto-sudoedit-mode 1))

```


<a id="org6cbe2e7"></a>

## iBuffer

[Ibuffer](https://www.emacswiki.org/emacs/IbufferMode) is an advanced replacement for BufferMenu, which lets you operate on buffers much in the same manner as Dired. The most important Ibuffer features are highlighting and various alternate layouts. Ibuffer is part of Emacs since version 22.

```emacs-lisp
(global-set-key (kbd "C-x C-b") 'ibuffer)
```


<a id="orge80f300"></a>

## dired - emacs file management


<a id="org201c2dd"></a>

### Dired Git Info

```emacs-lisp
(use-package dired-git
  :config
  :hook ((dired-mode-hook . dired-git-mode)))
```


<a id="org1b1490a"></a>

### dired hacks

Collection of useful dired additions found on github [here](https://github.com/Fuco1/dired-hacks). Found out about it at the useful emacs resource [**Pragmatic Emacs**](http://pragmaticemacs.com/category/dired/).

1.  dired subtree

    ```emacs-lisp
    (use-package dired-subtree
      :config
      (use-package dash)
      :bind (:map dired-mode-map
                  ("i" . dired-subtree-insert)
                  (";" . dired-subtree-remove)))
    ```

2.  dired filter

    More dired based filtering see [dired-filter-prefix](dired-filter-prefix)

    ```emacs-lisp
    (use-package dired-filter
      :config
      (use-package dash)
      )
    ```

3.  dired quicksort

    Dired sorting popup options from [Pragmatic Emacs](http://pragmaticemacs.com/emacs/speedy-sorting-in-dired-with-dired-quick-sort/). EDIT: cant clone from gitlab

    ```emacs-lisp
    (use-package dired-quick-sort
      :disabled t
      :ensure t
      :config
      (dired-quick-sort-setup))
    ```


<a id="orga5159ee"></a>

## PosFrame

Pop a [posframe](https://melpa.org/#/posframe) (just a frame) at pointPop a posframe (just a frame) at point

```emacs-lisp
(use-package posframe)
```


<a id="orgcc43796"></a>

## PopWin,Popwin makes you free from the hell of annoying buffers such like **Help**, **Completions**, **compilation**, and etc.

```emacs-lisp
(use-package popwin
  :init (popwin-mode 1))
```


<a id="orgda30346"></a>

## Transpose windows, transpose-frame

```emacs-lisp
(use-package transpose-frame
  :config
  (defun window-split-toggle ()
    "Toggle between horizontal and vertical split with two windows."
    (interactive)
    (if (> (length (window-list)) 2)
        (error "Can't toggle with more than 2 windows!")
      (let ((func (if (window-full-height-p)
                      #'split-window-vertically
                    #'split-window-horizontally)))
        (delete-other-windows)
        (funcall func)
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))))
  :bind
  ("C-M-t" . transpose-frame)
  ("C-c T" . window-split-toggle)
  )
```


<a id="org93b453f"></a>

## Hyperbole

[Hyperbole](https://www.emacswiki.org/emacs/Hyperbole) is more a window management system from what I can see. Need to explore it.

```emacs-lisp
(use-package
  hyperbole
  :disabled t)
```


<a id="orgf16ebd5"></a>

## Undo utilities


<a id="org5a7fd4f"></a>

### undohist

[undo-hist](https://melpa.org/#/undohist) provides persistent undo across sessions.

```emacs-lisp
(use-package undohist
  :disabled t
  :config
  (undohist-initialize))
```


<a id="orge02fa82"></a>

### undo-tree

[undo-tree](https://github.com/apchamberlain/undo-tree.el) visualises the sometimes complex undo ring and allow stepping along the timeline

```emacs-lisp
(use-package undo-tree
  :disabled t
  :config
  (global-undo-tree-mode))
```


<a id="org0ff9ac0"></a>

### undo-fu

```emacs-lisp
(use-package undo-fu
  :disabled t
  :init
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))
```


<a id="orgd255350"></a>

## Navigation


<a id="org2e0c007"></a>

### Back Button

[Back-Button](https://github.com/rolandwalker/back-button) provides better navigation on the [local and global mark rings](https://www.gnu.org/software/emacs/manual/html_node/emacs/Mark-Ring.html). The jury is still out on this one.

```emacs-lisp
(use-package back-button
  :config
  (back-button-mode 1)
  :bind
  ("M-<left>" . previous-buffer)
  ("M-<right>" . next-buffer))
```


<a id="orgca93033"></a>

### Window hopping

[Ace-Window](https://github.com/abo-abo/ace-window) provides better window switching.

```emacs-lisp
(use-package ace-window
  :bind*
  ("C-x o" . ace-window)
  ("C-x O" . ace-delete-window))
```


<a id="org790a702"></a>

### hopping around links

Quickly follow [links](https://github.com/abo-abo/ace-link) in Emacs.

```emacs-lisp
(use-package ace-link
  :demand t
  :config
  (ace-link-setup-default)
  :bind*
  (:map emacs-lisp-mode-map
        ("C-c o" . ace-link-addr))
  ("C-c o" . ace-link)
  )
```


<a id="org8649881"></a>

### hopping around in the buffer

Allows word, char and line hopping. The [wiki](https://github.com/winterTTr/ace-jump-mode/wiki) is a food source of info.

```emacs-lisp
(use-package ace-jump-mode
  :bind
  ("C-c j" . ace-jump-mode)
  )
```


<a id="orga7f2bd9"></a>

## Elscreen

[Elscreen](https://github.com/knu/elscreen) provides tabs in Emacs.

```emacs-lisp
(use-package
  elscreen
  :disabled t
  :config (elscreen-start))
```


<a id="orgb97460d"></a>

# Centaur Tabs

[Centaur Tabs](https://github.com/ema2159/centaur-tabs) is an aesthetic, functional and efficient tabs plugin for Emacs.

```emacs-lisp
(use-package centaur-tabs
  :disabled
  :demand
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :custom
  (centaur-tabs-height 32)
  (centaur-tabs-style "alternate")
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-cycle-scope 'default)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (defun centaur-tabs-buffer-groups ()
      "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
      (list
        (cond
         ((or (string-equal "*" (substring (buffer-name) 0 1))
              (memq major-mode '(magit-process-mode
                                 magit-status-mode
                                 magit-diff-mode
                                 magit-log-mode
                                 magit-file-mode
                                 magit-blob-mode
                                 magit-blame-mode
                                 )))
          "Emacs")
         ((derived-mode-p 'prog-mode)
          "Editing")
         ((derived-mode-p 'dired-mode)
          "Dired")
         ((memq major-mode '(helpful-mode
                             help-mode))
          "Help")
         ((memq major-mode '(org-mode
                             org-agenda-clockreport-mode
                             org-src-mode
                             org-agenda-mode
                             org-beamer-mode
                             org-indent-mode
                             org-bullets-mode
                             org-cdlatex-mode
                             org-agenda-log-mode
                             diary-mode))
          "OrgMode")
         (t
          (centaur-tabs-get-group-name (current-buffer))))))

  :hook
  (dired-mode . centaur-tabs-local-mode)
  :bind
  ("C-x B" . centaur-tabs-switch-group)
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))
```


<a id="org6ce3835"></a>

## Darkroom

[Darkroom](https://github.com/joaotavora/darkroom) provides distraction-free editing. Surprisingly nice, especially if someone is leaning over your and you want to point something out.

```emacs-lisp
(use-package
  darkroom
  :config (define-globalized-minor-mode my-global-darkroom-mode darkroom-tentative-mode
            (lambda ()
              (darkroom-tentative-mode 1)))
  :bind ("<f7>" . 'darkroom-tentative-mode))
```


<a id="org2c02826"></a>

## Outline Mode

[Outshine](https://github.com/alphapapa/outshine) attempts to bring the look and feel of Org Mode to the world outside of the Org major-mode. It’s an extension of outline-minor-mode that should act as a replacement of Outline Mode. Just change all your calls to outline-minor-mode into outshine-mode.

```emacs-lisp
(use-package outshine :disabled t)
```


<a id="text-tools"></a>

# Text tools


<a id="org0afdd5f"></a>

## Cursor/Region related


<a id="org17bb1ee"></a>

### General

```emacs-lisp
(defun centreCursorLineOn()
  "set properties to keep current line approx at centre of screen height. Useful for debugging."
  ;; a faster more concise alternative to MELPA's centered-cursor-mode
  (interactive)
  (setq  scroll-preserve-screen-position_t scroll-preserve-screen-position scroll-conservatively_t
         scroll-conservatively maximum-scroll-margin_t maximum-scroll-margin scroll-margin_t
         scroll-margin)
  (setq scroll-preserve-screen-position t scroll-conservatively 0 maximum-scroll-margin 0.5
        scroll-margin 99999))

(defun centreCursorLineOff()
  (interactive)
  (setq  scroll-preserve-screen-position scroll-preserve-screen-position_t scroll-conservatively
         scroll-conservatively_t maximum-scroll-margin maximum-scroll-margin_t scroll-margin
         scroll-margin_t))

```


<a id="org66318a1"></a>

### expand-region

[expand-region](https://github.com/magnars/expand-region.el) is an Emacs extension to increase selected region by semantic units.

```emacs-lisp
(use-package
  expand-region
  :config (defun er/select-call-f(arg)
            (setq current-prefix-arg arg)
            (call-interactively 'er/expand-region)
            (exchange-point-and-mark))
  (defun selectFunctionCall()
    (interactive)
    (er/select-call-f 3))
  :bind ("<C-return>" . selectFunctionCall)
  ("C-c e" . er/expand-region)
  ("C-c c" . er/contract-region))
```


<a id="org64123d4"></a>

### easy-kill

[easy-kill](https://github.com/leoliu/easy-kill) enables you to kill & Mark Things Easily in Emacs

```emacs-lisp
(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))
```


<a id="orgb6f1138"></a>

## Folding/Hide Show

[hs-minor-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html) allows hiding and showing different blocks of text/code (folding).

```emacs-lisp
(add-hook 'prog-mode-hook (lambda()(hs-minor-mode t)))
```

\#+end<sub>src</sub>


<a id="orgf40cc2d"></a>

## flyspell

```emacs-lisp
(use-package flyspell
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word)
    )
  :bind (("C-<f8>" . flyspell-mode)
        ("C-S-<f8>" . flyspell-buffer)
        ("<f8>" . flyspell-check-next-highlighted-word)
        ("S-<f8>" . flyspell-check-previous-highlighted-word)
        ))
```


<a id="org0d1c936"></a>

## Completion


<a id="org3ac2799"></a>

### Snippets with yasnippet

```emacs-lisp
(use-package
  yasnippet
  :init (yas-global-mode 1)
  :config

  (use-package
    php-auto-yasnippets)
  (use-package
    yasnippet-snippets)
  (use-package
    yasnippet-classic-snippets))
```


<a id="org1fca276"></a>

### Company Mode

[company-box](https://github.com/sebastiencs/company-box) provides nice linked help on the highlighted completions when available.

![img](./images/company-box.png "company-box in action for php/lsp mode.")

```emacs-lisp
(use-package
  company
  :demand t
  :config
  ;; (setq company-backends
  ;;       '((company-capf :with company-dabbrev-code :separate)
  ;;         (company-files :with company-dabbrev-code)
  ;;         (company-nxml company-dabbrev-code company-keywords :with company-yasnippet)
  ;;         (company-oddmuse :with company-yasnippet)
  ;;         (company-dabbrev :with company-yasnippet)))
  (use-package
    company-box
    :disabled
    :hook (company-mode . company-box-mode))
  (require 'company-ispell)
  (global-company-mode)
  :bind ("C-<tab>" . company-complete))
```


<a id="org6fd010c"></a>

### Which Key

[which-key](https://github.com/justbur/emacs-which-key) shows you what further key options you have if you pause on a multi key command.

```emacs-lisp
(use-package
  which-key
  :demand t
  :config (which-key-mode))
```


<a id="org0d63351"></a>

### Tying it call together

```emacs-lisp
(defun check-expansion ()
  (save-excursion (if (looking-at "\\_>") t (backward-char 1)
                      (if (looking-at "\\.") t (backward-char 1)
                          (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (yas/create-php-snippet)))))
;;              (indent-for-tab-command)))))
```


<a id="org5b111d8"></a>

## Searching non-helm

```emacs-lisp
(use-package ag)
```


<a id="org9cf56fb"></a>

## Abbrev Mode

[Abbrev Mode](https://www.emacswiki.org/emacs/AbbrevMode#toc4) is very useful for expanding small text snippets

```emacs-lisp
(setq-default abbrev-mode t)

```


<a id="orgeaf3885"></a>

## Deft - text searching

[Deft](https://jblevins.org/projects/deft/) is an Emacs mode for quickly browsing, filtering, and editing directories of plain text notes, inspired by Notational Velocity. It was designed for increased productivity when writing and taking notes by making it fast and simple to find the right file at the right time and by automating many of the usual tasks such as creating new files and saving files.

```emacs-lisp
(use-package deft
  :config
  (setq deft-directory (expand-file-name "orgfiles" user-emacs-directory))
  (setq deft-recursive t))

```


<a id="org8ac6a81"></a>

## Google This

[google-this](https://melpa.org/#/google-this) includes an interface to [google translate](https://translate.google.com/).

```emacs-lisp
(use-package
  google-this
  :config
  (google-this-mode 1)
  :bind ("C-c G" . google-this-search))
```


<a id="orgb460d03"></a>

## Reference and dictionary

The aim here is to link to different reference sources and have a sensible default for different modes. eg elisp mode would use internal doc sources, whereas javascript uses Dash/Zeal or even a straight URL search to lookup help. On top of that provide a list of other sources you can call by prefixing the core lookup-reference-dwim call. But if you lookup internal docs and it doesnt exist then why not farm it out to something like Goldendict which you can configure to look wherever you want? Examples here show Goldendict plugged into google translate amonst other things. The world's your oyster.


<a id="orgd873cab"></a>

### utility funcs

```emacs-lisp

      (defgroup rgr/lookup-reference nil
        "Define functions to be used for lookup"
        :group 'rgr)

      (defcustom mode-lookup-reference-functions-alist '(
                                                         (nil (goldendict-dwim goldendict-dwim))
                                                         (c++-mode  (my-lsp-ui-doc-glance my-dash))
                                                         (gdscript-mode  (my-lsp-ui-doc-glance my-dash))
;;                                                         (gdscript-mode  (my-gdscript-docs-browse-symbol-at-point my-dash))
                                                         (php-mode  (my-lsp-ui-doc-glance my-dash))
                                                         (web-mode  (my-lsp-ui-doc-glance my-devdocs))
                                                         (org-mode (elisp-lookup-reference-dwim))
                                                         (js2-mode (my-dash my-devdocs))
                                                         (js-mode (my-dash my-devdocs))
                                                         (rjsx-mode (my-dash my-devdocs))
                                                         (typescript-mode (my-dash my-devdocs))
                                                         (lisp-interaction-mode (elisp-lookup-reference-dwim my-dash))
                                                         (emacs-lisp-mode (elisp-lookup-reference-dwim my-dash)))
        "mode lookup functions"
        :group 'rgr/lookup-reference)

      (defun get-mode-lookup-reference-functions(&optional m)
        (let* ((m (if m m major-mode))
               (default-funcs (copy-tree(cadr (assoc nil mode-lookup-reference-functions-alist))))
               (mode-funcs (cadr (assoc m mode-lookup-reference-functions-alist))))
          (if mode-funcs (progn
                           (setcar default-funcs (car mode-funcs))
                           (if (cadr mode-funcs)
                               (setcdr default-funcs (cdr mode-funcs)))))
          default-funcs)) ;; (get-mode-lookup-reference-functions 'org-mode)

      (defcustom linguee-url-template "https://www.linguee.com/english-german/search?source=auto&query=%S%"
        "linguee url search template"
        :type 'string
        :group 'rgr/lookup-reference)

      (defcustom php-api-url-template "https://www.google.com/search?q=php[%S%]"
        "php api url search template"
        :type 'string
        :group 'rgr/lookup-reference)

      (defcustom jquery-url-template "https://api.jquery.com/?s=%S%"
        "jquery url search template"
        :type 'string
        :group 'rgr/lookup-reference)

      (defcustom  lookup-reference-functions '(my-describe-symbol goldendict-dwim my-linguee-lookup my-dictionary-search my-jquery-lookup google-this-search)
        "list of functions to be called via C-n prefix call to lookup-reference-dwim"
        :type 'hook
        :group 'rgr/lookup-reference)

      (defun sys-browser-lookup(w template)
        (interactive)
        (browse-url-xdg-open (replace-regexp-in-string "%S%" (if w w (symbol-or-region-at-point-as-string-or-prompt)) template)))

      (defun symbol-or-region-at-point-as-string-or-prompt()
        "if a prefix argument (4)(C-u) read from input, else if we have a region select then return that and deselect the region, else try symbol-at-point and finally fallback to input"
        (let* ((w (if (or  (not current-prefix-arg) (not (listp current-prefix-arg)))
                      (if(use-region-p)
                          (let ((sel-text
                                 (buffer-substring-no-properties
                                  (mark)
                                  (point))))
                            sel-text)
                        (thing-at-point 'symbol)) nil))
               (result (if w w (read-string "lookup:"))))
          result))

      (defun my-describe-symbol(w)
        (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
        (let ((s (if (symbolp w) w (intern-soft w))))
          (if s (describe-symbol s)
            (message "No such symbol: %s" w))))

      (defun my-linguee-lookup(w)
        (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
        (sys-browser-lookup w linguee-url-template))

      (defun my-php-api-lookup(w)
        (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
        (let ((dash-docs-docsets '("PHP")))
          (helm-dash w)))
      ;; (sys-browser-lookup w php-api-url-template))

      (defun my-jquery-lookup(&optional w)
        (interactive(cons (symbol-or-region-at-point-as-string-or-prompt) nil))
        (let (;;(zeal-at-point-docset "jQuery")
              (dash-docs-docsets '("jQuery")))
          (helm-dash w)))
      ;; (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
      ;; (sys-browser-lookup w jquery-url-template))

      (defun my-gdscript-docs-browse-symbol-at-point(&optional w)
        (gdscript-docs-browse-symbol-at-point))

      (defun lookup-reference-dwim(&optional secondary)
        "if we have a numeric prefix then index into lookup-reference functions"
        (interactive)
        (let((w (symbol-or-region-at-point-as-string-or-prompt))
             ;; PREFIX integer including 4... eg C-2 lookup-reference-dwim
             (idx (if (and  current-prefix-arg (not (listp current-prefix-arg)))
                      (- current-prefix-arg 1)
                    nil)))
          (if idx (let((f (nth idx lookup-reference-functions)))
                    (funcall (if f f (car lookup-reference-functions)) w))
            (let* ((funcs (get-mode-lookup-reference-functions))(p (car funcs))(s (cadr funcs)))
              (if (not secondary)
                  (unless (funcall p w)
                    (if s (funcall s w)))
                (if s (funcall s w)))))))

      (defun lookup-reference-dwim-secondary()
        (interactive)
        (lookup-reference-dwim t))

      (bind-key* "C-q" 'lookup-reference-dwim) ;; overrides major mode bindings
      (bind-key* "C-S-q" 'lookup-reference-dwim-secondary)

```


<a id="orgb28b7d4"></a>

### Dictionary

The more emacsy [Dictionary](https://melpa.org/#/dictionary) .

```emacs-lisp
(use-package
  dictionary
  :commands (my-dictionary-search)
  :config
  (defun my-dictionary-search(&optional w)
    (interactive)
    (dictionary-search (if w w (symbol-or-region-at-point-as-string-or-prompt))))
  :bind ("<f6>" . my-dictionary-search) )
```


<a id="org119c3a5"></a>

### Elisp reference

1.  quick help for function etc at point

    If an elisp object is there it brings up the internal docs:

    ![img](./images/lookup-internal-doc.png "lookup using internal docs")

    else it palms it off to goldendict.

    ![img](./images/lookup-goldendict.png "lookup using goldendict")

    ```emacs-lisp
    (defun display-elisp-lookup-reference-popup(sym)
      (interactive)
      (popup+-normal (if (fboundp sym)
                         (popup+-emacs-function sym)
                       (popup+-emacs-variable sym))))

    (defun elisp-lookup-reference-dwim
        (&optional
         w)
      "Checks to see if the 'thing' is known to elisp and, if so, use internal docs else call out to goldendict"
      (interactive)
      (let ((sym (if (symbolp w) w (intern-soft w))))
        (if (and sym
                 (or (fboundp sym)
                     (boundp sym)))
            (display-elisp-lookup-reference-popup sym) nil)))
    ```

2.  constant help as you cursor around

    [Context elisp help](https://emacs.stackexchange.com/questions/22132/help-buffer-on-hover-possible)

    ```emacs-lisp
    (define-minor-mode my-contextual-help-mode
      "Show help for the elisp symbol at point in the current *Help* buffer.

    Advises `eldoc-print-current-symbol-info'."
      :lighter " C-h"
      :global t
      (require 'help-mode) ;; for `help-xref-interned'
      (when (eq this-command 'my-contextual-help-mode)
        (message "Contextual help is %s" (if my-contextual-help-mode "on" "off")))
      (and my-contextual-help-mode
           (eldoc-mode 1)
           (if (fboundp 'eldoc-current-symbol)
               (eldoc-current-symbol)
             (elisp--current-symbol))
           (my-contextual-help :force)))

    (defadvice eldoc-print-current-symbol-info (before my-contextual-help activate)
      "Triggers contextual elisp *Help*. Enabled by `my-contextual-help-mode'."
      (and my-contextual-help-mode
           (derived-mode-p 'emacs-lisp-mode)
           (my-contextual-help)))

    (defvar-local my-contextual-help-last-symbol nil
      ;; Using a buffer-local variable for this means that we can't
      ;; trigger changes to the help buffer simply by switching windows,
      ;; which seems generally preferable to the alternative.
      "The last symbol processed by `my-contextual-help' in this buffer.")

    (defun my-contextual-help (&optional force)
      "Describe function, variable, or face at point, if *Help* buffer is visible."
      (let ((help-visible-p (get-buffer-window (help-buffer))))
        (when (or help-visible-p force)
          (let ((sym (if (fboundp 'eldoc-current-symbol)
                         (eldoc-current-symbol)
                       (elisp--current-symbol))))
            ;; We ignore keyword symbols, as their help is redundant.
            ;; If something else changes the help buffer contents, ensure we
            ;; don't immediately revert back to the current symbol's help.
            (and (not (keywordp sym))
                 (or (not (eq sym my-contextual-help-last-symbol))
                     (and force (not help-visible-p)))
                 (setq my-contextual-help-last-symbol sym)
                 sym
                 (save-selected-window
                   (help-xref-interned sym)))))))

    (defun my-contextual-help-toggle ()
      "Intelligently enable or disable `my-contextual-help-mode'."
      (interactive)
      (if (get-buffer-window (help-buffer))
          (my-contextual-help-mode 'toggle)
        (my-contextual-help-mode 1)))

    (my-contextual-help-mode 1)
    ```


<a id="org4a74867"></a>

### GoldenDict - external lookup and reference

When using goldendict-dwim why not add your program to the wonderful [GoldenDict](http://goldendict.org/)? A call to [trans-shell](https://github.com/soimort/translate-shell) in the dictionary programs tab gives us google translate:-

```bash
trans -e google -s de -t en -show-original y -show-original-phonetics n -show-translation y -no-ansi -show-translation-phonetics n -show-prompt-message n -show-languages y -show-original-dictionary n -show-dictionary n -show-alternatives n "%GDWORD%"
```

```emacs-lisp
(use-package
  goldendict
  :commands (goldendict-dwim)
  :config
  (defun goldendict-dwim
      (&optional
       w)
    "lookup word at region, thing at point or prompt for something, in goldendict. Use a prefix to force prompting."
    (interactive)
    (let ((w (if w w (symbol-or-region-at-point-as-string-or-prompt))))
      (call-process-shell-command (format  "goldendict \"%s\"" w ) nil 0)))
  :bind (("C-c g" . goldendict-dwim)))
```


<a id="org6a865fe"></a>

### DevDocs

```emacs-lisp
(use-package devdocs
  :commands (my-devdocs)
  :config
  (defun my-devdocs (&optional w)
    (interactive)
    (devdocs-search)t)
  :bind* ("C-c v" . 'my-devdocs))
```


<a id="org0328ea9"></a>

### Zeal - Linux Dash

```emacs-lisp
(use-package zeal-at-point
  :disabled t ;;way too buggy
  :commands (my-zeal)
  :config
  (defun my-zeal (&optional w)
    (interactive)
    (zeal-at-point)t)
  :bind* ("C-c z" . 'my-zeal))
```


<a id="orgf0d0ea9"></a>

### DASH - API documentation for most languages

Dash packages docs for many languages.

```emacs-lisp
      (use-package
        helm-dash
        :demand t
        :custom
        (dash-docs-common-docsets '("Emacs Lisp" "Docker"))
        (dash-docs-docsets '())
        :config
           (setq helm-dash-browser-func 'eww-readable-url)
;;         (setq helm-dash-browser-func 'eww)
        (defun my-dash (w)
          (interactive (cons (symbol-or-region-at-point-as-string-or-prompt) nil))
          (message "my-dash: %s" w)
          (message "docsets are: " dash-docs-docsets)
          (helm-dash w)t)
        :bind* ("C-c d" . 'my-dash))
```


<a id="treemacs"></a>

# Treemacs

Excellent [tree based navigation that works really well with projectile.](https://github.com/Alexander-Miller/treemacs)

```emacs-lisp
(use-package
  treemacs
  :config
  (treemacs-git-mode 'deferred)
  (use-package
    treemacs-projectile)

  (use-package
    treemacs-icons-dired
    :config (treemacs-icons-dired-mode))

  (use-package
    treemacs-magit
    :after treemacs
    magit)
  (defun my-treemacs-select-window (close)
    (interactive "P")
    (if close (treemacs)
      (treemacs-select-window)))
  :bind ("M-0"   . my-treemacs-select-window)
  (:map treemacs-mode-map
        ("<right>" . treemacs-peek)))

```


<a id="alerts"></a>

# Alerts

```emacs-lisp
(use-package
  alert
  :commands (alert)
  :custom (alert-default-style 'libnotify))
```


<a id="web"></a>

# Web


<a id="orgfe1f15c"></a>

## helper functions

```emacs-lisp
(defun www-open-current-page-external ()
  "Open the current URL in desktop browser."
  (interactive)
  (let((url (or
             (if(fboundp 'eww-currentl-url)
                 (eww-current-url)
               (if(fboundp 'w3m-currentl-url)
                   (w3m-current-url)
                 nil)))))
    (if url
        (browse-url-xdg-open url)
      (message "No buffer url"))))

(defun www-open-link-external ()
  "Open the current link or image in Firefox."
  (interactive)
  (let((anchor (url-get-url-at-point)))
    (if anchor
        (browse-url-xdg-open anchor)
      (message "No valid anchor found at cursor!"))))
```


<a id="orga23a146"></a>

## W3M - emacs text based web browser

[W3M](https://github.com/emacs-w3m/emacs-w3m) is an in-editor text based web browser. Handy for text based resources, bookmarking etc. Bind a couple of keys to open in the system browser.

```emacs-lisp
(use-package
  w3m
  :disabled t
  :custom (browse-url-browser-function 'w3m-browse-url)
  :config
  (use-package helm-w3m)
  :bind
  ("C-c o" . 'browse-url)
  (:map w3m-mode-map
        ("O" . www-open-current-page-external)
        ("o" . www-open-link-external)))
```


<a id="org3ff74a7"></a>

## EWW - emacs text based web browser

[Emacs-EWW](https://www.gnu.org/software/emacs/manual/html_mono/eww.html) is an in-editor text based web browser. Handy for text based resources, bookmarking etc. Bind a couple of keys to open in the system browser. Added some functions to [open some URLs in 'eww-readable](https://emacs.stackexchange.com/questions/36284/how-to-open-eww-in-readable-mode/47757) so that it skips headers and footers (normally bound to R in eww).


<a id="orgdd8b2ae"></a>

### open tasks :tangle no


<a id="orgf79c4ab"></a>

### code

```emacs-lisp
(use-package
  eww
  ;:disabled t
  :demand t
  :commands (eww-readable-url)
  :config
  (use-package helm-eww)
  (defun make-eww-readable()
    ;; make current eww buffer eww-readable and then remove the hook that called this so normal buffers are not eww-readable.
    (message "in make-eww-reabable")
    (unwind-protect
        ;;(eww-readable)
      (remove-hook 'eww-after-render-hook #'make-eww-readable)))

  (defun eww-readable-url (url)
    ;; when the eww buffer has rendered call a hook function that implements eww-readable and then removes that hook.
    ;; primarily for 'dash-docs-browser-func
    (interactive "sURL:")
    (add-hook 'eww-after-render-hook #'make-eww-readable)
    (message "eww readable browsing: %s, hook is: %s " url eww-after-render-hook)
    (eww url))
  :bind
  ("C-c o" . 'browse-url)
  ("C-c O" . 'eww-readable-url)
  (:map eww-mode-map
        ("O" . www-open-current-page-external)
        ("o" . www-open-link-external)))
```


<a id="online-chats"></a>

# Online Chats

Add a "-chat" [command line switch](https://www.gnu.org/software/emacs/manual/html_node/elisp/Command_002dLine-Arguments.html) to load the [Emacs IRC client](https://www.gnu.org/software/emacs/manual/html_mono/erc.html) and other such of choice. I created this as I tend to hop in and out of Emacs because of severe "[configuration fever](https://alhassy.github.io/init/)". You don't want to be hopping in and out of chat groups too. An [external shell script](./editor-config/oneemacs-chat) checks for the existence of a chat instance (frame name) and switches to it if it exists.

```emacs-lisp
(defun rgr/load-chats(switch)
  (require  'rgr-chat))
(add-to-list 'command-switch-alist '("chat" . rgr/load-chats))
```

The code in [rgr-chat.el](../elisp/rgr-chat.el):

```emacs-lisp
(use-package erc :demand t
  :diminish erc-mode
  :config
  (require 'erc-replace)
  (defun textReadFont ()
    "Set font to a variable width (proportional) fonts in current buffer."
    (interactive)
    ;; (setq buffer-face-mode-face '(:family "arial"))
    ;; (buffer-face-mode)
    )

  (defun rgr/erc-switch-to-channel(&optional channel)
    (when (string= (or channel "#emacs") (buffer-name (current-buffer)))
      (switch-to-buffer (current-buffer))))

  (defun rgr/erc-quit()
    (when (get-buffer "irc.freenode.net:6667")
      (progn
        (switch-to-buffer(get-buffer "irc.freenode.net:6667"))
        (erc-cmd-QUIT "bye"))))

  (defun rgr/erc-start()
    (interactive)
    (unless(get-buffer "irc.freenode.net:6667")
      (progn
        (erc :server "irc.freenode.net"  :port 6667)
        (add-hook 'erc-join-hook 'rgr/erc-switch-to-channel))))

  (defun rgr/erc-format-nick (&optional user _channel-data)
    "Return the nickname of USER.
             See also `erc-format-nick-function'."
    ;; (when user (format "%-024s" (erc-server-user-nickname user))))
    (when user (format "%s" (erc-server-user-nickname user))))
  :hook
  (erc-mode . textReadFont))

(use-package slack :demand t
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (defhydra slack-hydra (:color gold :hint none)
    "
          team                       channel                   message              Entry
          -------------------------------------------------------------------------------------------------
          _c_: change current team   _s_: select channel       _i_: select im       _S_: start slack
          _r_: register team         _l_: channel list update  _u_: im list update  _C_: close connections
                                                                                    _q_: close hydra
          "
    ;; Team
    ("c" slack-change-current-team)
    ("r" slack-register-team)

    ;; Channel
    ("s" slack-channel-select)
    ("l" slack-channel-list-update)

    ;; Message
    ("i" slack-im-select)
    ("u" slack-im-list-update)

    ;; Entry
    ("S" slack-start)
    ("C" slack-ws-close)

    ("q"  nil                    "cancel"     :color orange))

  (slack-register-team
   :name "emacs-slack"
   :default t
   :token slack-api-token
   :subscribed-channels slack-subscribed-channels
   :full-and-display-names t)
  :bind
  ("C-c S" . slack-hydra/body))

(use-package gitter :demand d)


(defgroup rgr/chat-clients nil
  "Chat provides a standalone emacs instance with chat channels open"
  :group 'rgr)

(defcustom rgr/chat-functions nil
  "Start client functions"
  :group 'rgr/chat-clients
  :type '(repeat function))

(defcustom rgr/chat-close-functions nil
  "Close client functions"
  :group 'rgr/chat-clients
  :type '(repeat function))

(defun rgr/start-chats()
  (interactive)
  (dolist (fn rgr/chat-functions)
    (when (fboundp fn)
      (progn
        (message "Calling %s in start-chats" (symbol-name fn))
        (funcall fn))))
  (add-hook 'kill-emacs-hook #'rgr/close-chats))

(defun rgr/close-chats()
  (dolist (fn rgr/chat-close-functions)
    (when (fboundp fn)
      (progn
        (message "Calling %s in close-chats" (symbol-name fn))
        (funcall fn)))))

(recentf-mode -1)
(save-place-mode -1)
(savehist-mode -1)

(global-unset-key (kbd "C-c i"))
(global-set-key (kbd "C-c i") 'rgr/start-chats)

(when(yes-or-no-p "start chats?")
  (rgr/start-chats))

(provide 'rgr-chat)
```


<a id="org75bdb29"></a>

### Only one chat instance

Small external script, [oneemacs-chat](../editor-config/oneemacs-chat), to create an erc instance if there isnt already one.

```shell
#!/bin/bash
WID=`xdotool search --name "Chat:"|head -1`
if [[ -z ${WID} ]]; then
    notify-send "Starting Chats in Emacs..."
    emacs -chat
else
    notify-send "restoring Chat instance..."
    xdotool windowactivate $WID
fi
```

and the eshell func to call it:

```emacs-lisp
(defun eshell/chat-client
    (&rest
     args)
  "run a clean emacs"
  (interactive)
  (save-window-excursion (call-process "oneemacs-chat" nil 0)))

(global-set-key (kbd "C-c i") 'eshell/chat-client)
```


<a id="org1cabcdb"></a>

## Slackware


<a id="orge5e327b"></a>

### Emacs Slack

[Slack](https://slack.com/intl/en-de/) interface for Emacs on [github](https://github.com/yuya373/emacs-slack). See [rgr-chat.el](./lisp/rgr-chat.el).


<a id="org2f668ef"></a>

### Emacs Gitter

[Gitter](https://gitter.im/) interface for Emacs on [github](https://github.com/xuchunyang/gitter.el). See [rgr-chat.el](./lisp/rgr-chat.el).


<a id="org-mode"></a>

# Org functionality


<a id="org1645101"></a>

## Org Mode, org-mode


<a id="org9e31f61"></a>

### config

```emacs-lisp
(use-package org
  :straight org-plus-contrib
  :custom
  (org-directory "~/.emacs.d/orgfiles")
  (org-agenda-files (list org-directory (concat org-directory "/projects" )))
  (org-refile-targets `((,(directory-files-recursively org-directory "^[[:alnum:]].*\\.\\(org\\|gpg\\)\\'") :maxlevel . 9)))
  (org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (org-refile-use-outline-path t)                  ; Show full paths for refiling
  :config
  (require 'org-protocol)
  (require 'org-inlinetask)
  (defun rgr/org-agenda (&optional arg)
    (interactive "P")
    (let ((org-agenda-tag-filter-preset '("-trash")))
      (org-agenda arg "a")))
  :bind
  ("C-c a" . org-agenda)
  ("C-c A" . rgr/org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c C-l" . org-insert-link)
  ("C-c C-s" . org-schedule)
  ("C-c t" . org-todo))

(use-package
  org-bullets
  :config (add-hook 'org-mode-hook (lambda ()
                                     (org-bullets-mode 1)
                                     ;; (org-num-mode 1)
                                     )))

(org-clock-persistence-insinuate)
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(require 'org-crypt)
(org-crypt-use-before-save-magic)

;; The following lines are always needed.  Choose your own keys.

(add-hook 'org-mode-hook 'rgr/elisp-helpers)


(defface org-canceled
  ;; originally copied from font-lock-type-face
  (org-compatible-face nil '((((class color)
                               (min-colors 16)
                               (background light))
                              (:foreground "darkgrey"
                                           :bold t))
                             (((class color)
                               (min-colors 16)
                               (background dark))
                              (:foreground "grey"
                                           :bold t))
                             (((class color)
                               (min-colors 8))
                              (:foreground "grey"))
                             (t
                              (:bold t))))
  "Face used for todo keywords that indicate DONE items."
  :group 'org-faces)

(defface org-wait
  ;; originally copied from font-lock-type-face
  (org-compatible-face nil '((((class color)
                               (min-colors 16)
                               (background light))
                              (:foreground "darkgrey"
                                           :bold t))
                             (((class color)
                               (min-colors 16)
                               (background dark))
                              (:foreground "grey"
                                           :bold t))
                             (((class color)
                               (min-colors 8))
                              (:foreground "grey"))
                             (t
                              (:bold t))))
  "Face used for todo keywords that indicate DONE items."
  :group 'org-faces)
```


<a id="org242fc6f"></a>

### Journal, org-journal

More advanced journalling courtesy of [org-journal](https://github.com/bastibe/org-journal).

```emacs-lisp
(use-package org-journal
  :bind ("C-c J" . org-journal-new-entry)
  :bind ("C-c S" . org-journal-search))
```


<a id="org655d3ce"></a>

### ROAM note taking, org-roam

```emacs-lisp
(use-package org-roam
  :disabled t
  :after org
  :hook (org-mode . org-roam-mode)
  :straight (:host github :repo "jethrokuan/org-roam")
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph))
```


<a id="orgf989e92"></a>

### Authoring in org-mode

Work in progress. Thread related to this is [here](https://www.reddit.com/r/orgmode/comments/6y59r2/using_org_mode_to_write_books/).


<a id="org452e23c"></a>

### Passwords, org-password-manager

```emacs-lisp
(use-package org-password-manager
:config
(add-hook 'org-mode-hook 'org-password-manager-key-bindings))
```

1.  Passwords     :crypt:

    &#x2013;&#x2014;BEGIN PGP MESSAGE&#x2013;&#x2014;

    hQEMA7IjL5SkHG4iAQf/RK74iKGuA+IXecpRBGLUY9oNP7pPtVL23UDqdkKqldH8 SCqwTBbR4lmonJN/d44ezTMjJd4tutxTST37FcGRBgGltaKRrBG4KX3h5BC/WDz1 kKX/nnmxzBNQC0lP9l1gIvBUPIgoKUEMhoXtSY6xaNvg6iGNIhQnW4KoQvhvS5KM TgnlvhlhbFjEcKgR4dOdnAvJDhDGU1ZAB6DPAfraSYICe2qBD1qFyix1hBH+4DYp CBgqOM+Mx2Jbs0MkJGl8/lmsgbYsssbxD8gjuNwgj3ZIckd6xDZofojjylyPWWBb DRsNrF1Qqws2g2boI28LhqOV7KxXCxHxZQ3PpjIGedKqAcqJlk8Igo/MLywxf0hJ H65Vndgdgn4BMJCG/c0KfsvweTRCWataVpuUT8wV34jCCn0U5XSqekCIwHjell0p cXTmIjm4aqwXjGFc927GQ37Rg/QDRG7EEXLy634+PUuiqtAoKVo9kz9fQcFKFlgv /Msuyn36n+Miw1sh1tig5DFdwoIilI2ByKyvjP1POIMMDg9me/trlK+MiQVpxiKw 0gVVrLnozFIRo7U= =R1XD &#x2013;&#x2014;END PGP MESSAGE&#x2013;&#x2014;


<a id="orge41f411"></a>

## Self documenting config file

```emacs-lisp

(use-package ox-gfm
  :defer 3
  :after org
  :config
  (defun config-export-to-markdown()
    (interactive)
    (if (and (eq major-mode 'org-mode) (file-exists-p (concat (file-name-sans-extension (buffer-file-name)) ".md")))
        (org-gfm-export-to-markdown)))

  (add-hook 'after-save-hook 'config-export-to-markdown))

```


<a id="email"></a>

# Email, gmail, Gnus

```emacs-lisp
(use-package
  gnus

  :disabled t
  :config

  (setq smtpmail-smtp-server "smtp.gmail.com" smtpmail-smtp-service 587 gnus-ignored-newsgroups
        "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  (require 'bbdb)
  (require 'bbdb-vcard)
  (bbdb-initialize 'gnus 'message)
  (add-hook 'message-setup-hook 'bbdb-mail-aliases)
  (defun my-gnus-themes()
    (load-theme-buffer-local 'alect-light (current-buffer)))
  (require 'gnus-desktop-notify)
  (gnus-desktop-notify-mode)
  (gnus-demon-add-scanmail)

  (define-key gnus-summary-mode-map (kbd "M-o") 'ace-link-gnus)
  (define-key gnus-article-mode-map (kbd "M-o") 'ace-link-gnus)
  (setq bbdb-use-pop-up nil)
  (use-package
    helm-bbdb)
  :bind	  ("C-c m".  'gnus))
```


<a id="orgf91223e"></a>

# Screen recording


<a id="org43b1577"></a>

## Emacs screencasts

Package [keycast](https://github.com/tarsius/keycast) shows the keys pressed

```emacs-lisp
(use-package keycast
  )

```


<a id="pomodoro"></a>

# Pomodoro

[Pomidor](https://github.com/TatriX/pomidor) is a simple and cool pomodoro technique timer.

```emacs-lisp
(use-package
  pomidor
  :bind (("S-<f7>" . pomidor))
  :custom (pomidor-sound-tick nil)
  (pomidor-sound-tack nil)
  (pomidor-seconds (* 25 60))
  (pomidor-break-seconds (* 5 60))
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer)))))
```


<a id="programming"></a>

# Programming related


<a id="orge3757bf"></a>

## General

```emacs-lisp

(global-set-key (kbd "S-<f2>") 'linum-mode)
(add-hook 'prog-mode-hook (lambda() (linum-mode t)))

(use-package
  smartparens
  :commands (smartparens-mode)
  :config (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  (sp-local-tag '(mhtml-mode html-mode) "b" "<span class=\"bold\">" "</span>")
  (smartparens-global-mode t))

(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))


```


<a id="org66885b3"></a>

## Symfony


<a id="org23b2d08"></a>

### custom

```emacs-lisp
(defgroup rgr/symfony nil
  "Symfony Development"
  :group 'rgr)

(defcustom symfony-server-command "~/.symfony/bin/symfony server:start"
  "Start the symfony web server"
  :type 'string
  :group 'rgr/symfony)
```


<a id="orgb68fb5c"></a>

### Start a symfony web server when applicable

```emacs-lisp
(use-package php-mode
  :config
  (add-to-list 'display-buffer-alist
               (cons "\\*Symfony Web Server\\*.*" (cons #'display-buffer-no-window nil)))
  (defun start-symfony-web-server()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (if (and default-directory (file-exists-p "bin/console") (eq (length (shell-command-to-string "pgrep symfony")) 0) (yes-or-no-p "Start web server?"))
          (async-shell-command symfony-server-command "*Symfony Web Server*"))))
  (defun php-mode-webserver-hook ()
    (interactive)
    (start-symfony-web-server)
    ))
;;:hook (php-mode . php-mode-webserver-hook))
```

We can trigger it using a .dir-locals.el

```emacs-lisp
((php-mode
  (eval php-mode-webserver-hook)))
```


<a id="org888053a"></a>

## Emacs Lisp, ELisp


<a id="orgc019fea"></a>

### refactoring utlities

```emacs-lisp
(use-package
  elisp-format
  :bind
  ("C-c f" . elisp-format-region)
  (:map emacs-lisp-mode-map
        ("C-c f" . elisp-format-region)))
```


<a id="orgaa13d3f"></a>

### query symbol

```emacs-lisp
(use-package popup+
  :config
  (defun show-symbol-details ()
    (interactive)
    (popup-tip (format "intern-soft thing-at-point: %s, symbolp: %s, symbol-name:%s"
                       (setq-local sym (intern-soft (thing-at-point 'symbol)))
                       (symbolp sym)
                       (symbol-name sym))))
  :bind  ("C-M-S-s" . #'show-symbol-details))
```


<a id="org9648fd4"></a>

### Elisp completion and debugging

```emacs-lisp
(use-package
  edebug-x
  :demand t
  :init
  (global-set-key (kbd "C-S-<f9>") 'toggle-debug-on-error)
  ;;:custom
  ;;(edebug-trace nil)
  :config
  (require 'edebug)
  (defun instrumentForDebugging()
    (interactive)
    (eval-defun 0))
  (defun instrumentForDebugging()
    "use the universal prefix arg (C-u) to remove instrumentation"
    (interactive)
    (if current-prefix-arg (eval-defun nil) (eval-defun 0)))
  (defun rgr/elisp-helpers()
    ;; Meh, use C-u C-M-x
    ;; (define-key (current-local-map)
    ;;   (kbd "C-<f9>") #'instrumentForDebugging)
    )
  (add-hook 'emacs-lisp-mode-hook #'rgr/elisp-helpers)
  (add-hook 'lisp-interaction-mode-hook #'rgr/elisp-helpers)
  (add-hook 'help-mode-hook #'rgr/elisp-helpers)
  )
```


<a id="orge8665d1"></a>

### Auto-compile

```emacs-lisp
(use-package
  auto-compile
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))
```


<a id="org7926a6e"></a>

## JSON, YAML Configuration files


<a id="org7065346"></a>

### JSON Editing

JSON editing using [json-mode](https://github.com/joshwnj/json-mode)

```emacs-lisp
(use-package json-mode)
```


<a id="orgb72c28d"></a>

### YAML

1.  Modes

    ```emacs-lisp
    (use-package
      yaml-mode
      :config
      (add-to-list 'auto-mode-alist '("\\.yml\\.yaml\\'" . yaml-mode))
      )
    ```


<a id="org4fc0ee5"></a>

## Flycheck

On the fly [syntax checking](https://github.com/flycheck/flycheck) for GNU Emacs

```emacs-lisp
(use-package
  flycheck
  :config (use-package
            flycheck-pos-tip)
  (flycheck-pos-tip-mode)
  (global-flycheck-mode))
```


<a id="org0667ea4"></a>

## Version Control


<a id="orge6d9ed2"></a>

### It's [Magit](https://github.com/magit/magit)! A Git porcelain inside Emacs

```emacs-lisp
(use-package
  diff-hl
  :init (global-diff-hl-mode 1))
(use-package
  magit
  :demand t
  :config (add-hook 'magit-post-commit-hook 'magit-mode-bury-buffer)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind* ("C-x g" . magit-status)
  )
```


<a id="orgc3b2358"></a>

### [Forge](https://github.com/magit/forge) ahead with Pull Requests

```emacs-lisp
(use-package forge
  :after magit)
```


<a id="orgfd1bbac"></a>

## Javascript

```emacs-lisp

(use-package rjsx-mode
  :config
  (use-package npm-mode)
  (defun my-js2-mode-hook ()
    ;;         (setq-local zeal-at-point-docset '("JavaScript" "jQuery"))
    (npm-mode t)
    (setq-local dash-docs-docsets '("React" "JavaScript" "jQuery")))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook)
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
  )

(use-package prettier-js
  :custom
  (prettier-js-args '(
                      "--trailing-comma" "all"
                      "--bracket-spacing" "false"
                      "--print-width" "80"
                      )))

(defun my-js-mode-hook ()
  (setq-local js-indent-level 2)
  (prettier-js-mode t)
  (setq-local dash-docs-docsets '("React" "JavaScript" "jQuery")))

(add-hook 'js-mode-hook 'my-js-mode-hook)



;;(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))
```


<a id="org8d19d86"></a>

## RJSX

[rjsx-mode](https://github.com/felipeochoa/rjsx-mode) extends js2-mode to include jsx parsing.

```emacs-lisp
(use-package rjsx-mode
  :disabled t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
  )
```


<a id="orgd4bf2ec"></a>

## Typescript

```emacs-lisp
(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-mode))
  (defun my-ts-mode-hook ()
    (setq-local dash-docs-docsets '("React" "JavaScript")))
  (add-hook 'typescript-mode-hook 'my-ts-mode-hook))
```


<a id="org75ef7ff"></a>

## Tide Mode

```emacs-lisp
(use-package tide
  :disabled t
  :config
  (defun setup-tide-mode ()
    "Setup function for tide."
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  (setq company-tooltip-align-annotations t)
  :init
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  )
```


<a id="orgef6c34f"></a>

## Language Server Protocol (LSP)

[Emacs-lsp](https://github.com/emacs-lsp) : Language Server Protocol client for Emacs

```emacs-lisp
;; if you want to change prefix for lsp-mode keybindings.
(use-package lsp-mode
  :custom
  (lsp-enable-file-watchers . nil)
  :config
  (use-package
    lsp-ui
    :custom
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-delay 3)
    (lsp-ui-doc-delay 1.7)
    :config
    (use-package lsp-treemacs
      :config
      (lsp-treemacs-sync-mode 1))
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

    (defun my-lsp-ui-doc-glance (&optional w)
      "Trigger display hover information popup and hide it on next typing."
      (interactive)
      (lsp-describe-thing-at-point)
      ;; (message "lsp-ui-doc--displayed:%s" lsp-ui-doc--displayed)
      )
    (defun my-lsp-ui-mode-hook()
      (lsp-ui-sideline-mode -1)
      (lsp-ui-doc-mode -1)
      )

    (defun my-lsp-ui-imenu-view()
      (interactive)
      (lsp-ui-imenu--view)
      )


    :bind ((:map lsp-ui-mode-map
                 ;; rather use helm's imenu< C-c h i>             ("C-c m"   . lsp-ui-imenu)
                 ;;("C-q"   . lsp-ui-doc-show)
                 ("C-S-<f10>" . lsp-ui-imenu)
                 ("M-S-<f9>" . myDapDebugOn)
                 ("C-c S"   . toggle-lsp-ui-sideline)
                 ("C-c D"   . toggle-lsp-ui-doc))
           (:map lsp-ui-imenu-mode-map
                 ("<RET>" . my-lsp-ui-imenu-view)
                 ))

    :hook ((lsp-ui-mode . my-lsp-ui-mode-hook)))

  (use-package dap-mode
    :demand t
    :commands (myDapDebugOn)
    :config
    (setq dap-ui-buffer-configurations
          `((,"*dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.50))) ;; changed this to 0.50
            (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.50)))
            (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.50)))
            (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
            (,"*debug-window*" . ((side . bottom) (slot . 3) (window-width . 0.20)))))


    (setq dap-auto-configure-features '(locals  tooltip))
    (require 'dap-gdb-lldb)
    ;;          (dap-gdb-lldb-setup)
    ;; (add-hook 'dap-stopped-hook (lambda (arg)
    ;;                               (call-interactively #'dap-hydra)))

    (require 'dap-chrome)

    ;; (dap-register-debug-template "Chrome Browse my-project"
    ;;   (list :type "chrome"
    ;;         :cwd nil
    ;;         :mode "url"
    ;;         :request "launch"
    ;;         :webRoot "/home/rgr/Dropbox/homefiles/development/projects/react/my-app/"
    ;;         :url "http://localhost:3000"
    ;;         :sourceMap "true"
    ;;         :name "Chrome Browse my-project"))



    (defun myDapDebugOn(p)
      "turn on dap modes"
      (interactive "P")
      (centreCursorLineOn)
      (if p
          (dap-debug nil)
        (dap-debug-last))
      ;;    (dap-hydra)
      )
    (defun my-lsp-mode-hook()
      (lsp-enable-which-key-integration)
      )

    :bind (:map dap-mode-map
                ("<f8>" . dap-continue)
                ("S-<f8>" . dap-disconnect)
                ("C-<f8>" . dap-debug-last)
                ("<f9>" . dap-hydra)
                ("<f10>" . dap-next)
                ("<f11>" . dap-step-in)
                ("S-<f11>" . dap-step-out)
                ))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ((c-mode c++-mode js-mode php-mode gdscript-mode). lsp)
         ;; if you want which-key integration
         (lsp-mode . my-lsp-mode-hook)))
```

1.  [.dir-local.el](file:///home/rgr/development/thirdparty/godot/bin) config for a debug template

    ```emacs-lisp
    ((c++-mode . ((dap-debug-template-configurations . (("Godot LLDB"
                                                         :type "lldb"
                                                         :request "launch"
                                                         :target "/home/rgr/bin/godot")
                                                        ("Godot GDB"
                                                         :type "gdb"
                                                         :request "launch"
                                                         :target "/home/rgr/bin/godot"))))))
    ```


<a id="org480fbea"></a>

## Serial Port

```emacs-lisp
(defgroup rgr/serial-ports nil
  "serial port customization"
  :group 'rgr)

(defcustom rgr/serialIOPort "/dev/ttyACM0"
  "Serial device for emacs to display"
  :type 'string
  :group 'rgr/serial-ports)

(defcustom rgr/serialIOPortBaud 9600
  "Default serial baud rate"
  :type 'integer
  :group 'rgr/serial-ports)

(defun selectSerialPortBuffer()
  (setq ser (get-buffer rgr/serialIOPort))
  (if ser (switch-to-buffer ser)
    (serial-term rgr/serialIOPort rgr/serialIOPortBaud)))

(global-set-key (kbd "C-c s")
                (lambda()
                  (interactive)
                  (selectSerialPortBuffer)))
```


<a id="org6a352f4"></a>

## PlatformIO

[platformio-mode](https://github.com/emacsmirror/platformio-mode) is an Emacs minor mode which allows quick building and uploading of PlatformIO projects with a few short key sequences. The build and install process id documented [here](https://docs.platformio.org/en/latest/ide/emacs.html).

```emacs-lisp
(straight-use-package 'platformio-mode)
```


<a id="orgaf94b8e"></a>

## C


<a id="orga6b6fcf"></a>

### Clang provides us with some industry standard code prettiers

```emacs-lisp
(straight-use-package 'clang-format)
(setq clang-format-style-option "llvm")
(fset 'c-indent-region 'clang-format-region)
(fset 'c-indent-buffer 'clang-format-buffer)
```


<a id="org06bfb4f"></a>

### C  modes hooks

```emacs-lisp
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(use-package
  ccls
  :hook ((c++-mode c-mode objc-mode) . (lambda ()
                                         (ccls-use-default-rainbow-sem-highlight)
                                         ;;                 (ccls-code-lens-mode)
                                         )))
(defun my-c-setup ()
  "Set up my C mode."
  (platformio-mode))
;;        (define-key c-mode-map (kbd "C-c s") #'selectSerialPort
(add-hook 'c-mode-hook #'my-c-setup)
(add-hook 'objc-mode-hook #'my-c-setup)

```


<a id="org8092b7e"></a>

## C++

```emacs-lisp
(defun my-c++-mode-hook ()
  (setq-local dash-docs-docsets '("C++")))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
```


<a id="org7f9875c"></a>

## C#

1.  Loading CSharp support

    ```emacs-lisp
    (use-package csharp-mode)
    (use-package omnisharp)
    ```


<a id="org0924142"></a>

## Godot GDScript

This [package](https://github.com/GDQuest/emacs-gdscript-mode) adds support for the GDScript programming language from the Godot game engine in Emacs. It gives syntax highlighting and indentations

```emacs-lisp
(use-package gdscript-mode
  ;;       :disabled t
  :straight (gdscript-mode
             :type git
             :host github
             :repo "rileyrg/emacs-gdscript-mode")
  :init
  (defun franco/godot-gdscript-lsp-ignore-error (original-function &rest args)
    "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
    (if (string-equal major-mode "gdscript-mode")
        (let ((json-data (nth 0 args)))
          (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                   (not (gethash "id" json-data nil))
                   (not (gethash "method" json-data nil)))
              nil ; (message "Method not found")
            (apply original-function args)))
      (apply original-function args)))
  (advice-add #'lsp--get-message-type :around #'franco/godot-gdscript-lsp-ignore-error)
  )
```


<a id="org6cc5c2b"></a>

## tasks :tangle no


<a id="org98546de"></a>

## PHP Mode

```emacs-lisp

(use-package composer :disabled t)

(defun rgr/debug-php()
  (interactive)
  (dap-debug (cdr (car (cdr dap-debug-template-configurations)))))

(use-package
  php-mode
  :disabled t
  :commands (php-mode)
  :config
  (defun my-php-mode-hook()
    (setq-local dash-docs-docsets '("Symfony" "PHP")));test
  (add-hook 'php-mode-hook 'my-php-mode-hook)
  (use-package
    transient)
  (use-package
    phpstan)
  (use-package
    flycheck-phpstan
    :after phpstan
    )
  (define-transient-command
    php-transient-menu
    ()
    "Php"
    [["Class" ("cc" "Copy" phpactor-copy-class)
      ("cn" "New" phpactor-create-new-class)
      ("cr" "Move" phpactor-move-class)
      ("ci" "Inflect" phpactor-inflect-class)
      ("n"  "Namespace" phpactor-fix-namespace)]
     ["Properties" ("a"  "Accessor" phpactor-generate-accessors)
      ("pc" "Constructor" phpactor-complete-constructor)
      ("pm" "Add missing props" phpactor-complete-properties)
      ("r" "Rename var locally" phpactor-rename-variable-local)
      ("R" "Rename var in file" phpactor-rename-variable-file)]
     ["Extract" ("ec" "constant" phpactor-extract-constant)
      ("ee" "expression" phpactor-extract-expression)
      ("em"  "method" phpactor-extract-method)]
     ["Methods" ("i" "Implement Contracts" phpactor-implement-contracts)
      ("m"  "Generate method" phpactor-generate-method)]
     ["Navigate" ("x" "List refs" phpactor-list-references)
      ("X" "Replace refs" phpactor-replace-references)
      ("."  "Goto def" phpactor-goto-definition)]
     ["Phpactor" ("s" "Status" phpactor-status)
      ("u" "Install" phpactor-install-or-update)
      ("q" "Quit" transient-quit-all)]])
  (use-package
    phpactor
    :config (use-package
              company-phpactor))
  :bind (:map php-mode-map
              ( "C-c C-y" .  yas/create-php-snippet)
              ( "C-S-<return>" . c-complete-line)
              ( "C-<return>" . c-newline-below)
              ( "C-S-y" . c-insert-previous-line)
              ( "M-<return>" . php-transient-menu)))

(use-package
  php-scratch
  :bind (:map php-mode-map
              ("C-x s" . php-scratch)))
```


<a id="org43d4dcd"></a>

## Web,Symfony and Twig


<a id="orga12b7e6"></a>

### The Code

```emacs-lisp
(use-package
  web-mode
  ;;:disabled t
  :demand t
  :config
  (defun my-web-mode-hook()
    (setq-local dash-docs-docsets '("Twig" "CSS" "HTML"))
    )
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  ;; (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))
```


<a id="org88bee53"></a>

## elf-mode - view the symbol list in a binary

[https://oremacs.com/2016/08/28/elf-mode/](https://oremacs.com/2016/08/28/elf-mode/)

```emacs-lisp
(use-package elf-mode
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(?:a\\|so\\)\\'" . elf-mode)))
```


<a id="org8a8d9f3"></a>

## EDiff - comparing files in Emacs

```emacs-lisp
(use-package ediff+
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :config
  (when (fboundp 'winnder-undo)
    (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
  :bind (:map prog-mode-map ("C-c C-d" . 'ediff-files)))
```


<a id="macros"></a>

# Macros & Utilities


<a id="org28367f8"></a>

## move to end of line, add a semi colon and move to next line

```emacs-lisp


(defun c-complete-line()
  (interactive)
  (end-of-line)
  (unless (eql ?\; (char-after (- (point-at-eol) 1)))
    (progn (insert ";")))
  (newline-and-indent))
(defun c-insert-previous-line()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  (insert (string-trim (current-kill 0))))
(defun c-newline-below()
  (interactive)
  (end-of-line)
  (newline-and-indent))
```


<a id="privacy"></a>

# Privacy

```emacs-lisp

(load (expand-file-name "flowers.el.gpg" user-emacs-directory ))
(eval-after-load "gnus" '(progn
                           (setq circe-default-nick (nth 0 (auth-source-user-and-password "circe")))
                           (setq circe-default-user (nth 1 (auth-source-user-and-password "circe")))
                           (setq twit-user (nth 0 (auth-source-user-and-password "twitter")))
                           (setq twit-pass (nth 1 (auth-source-user-and-password "twitter")))
                           (setq twittering-username (nth 0 (auth-source-user-and-password
                                                             "twitter")))
                           (setq twittering-password (nth 1 (auth-source-user-and-password
                                                             "twitter")))
                           (setq twitter-username (nth 0 (auth-source-user-and-password "twitter")))
                           (setq twitter-password (nth 1(auth-source-user-and-password "twitter")))
                           (setq emms-player-mpd-server-password (nth 1
                                                                      (auth-source-user-and-password
                                                                       "emmsmpd"))          )
                           (setq org-mobile-encryption-password (nth 1(auth-source-user-and-password
                                                                       "org-mobile")))))
```


<a id="host-specifics"></a>

# Host specific setting

Sometimes needs machine specific settings eg a larger font on a bigger display.

```emacs-lisp

(defun load-host-customisation()
  "allow the requiring of a custom-$HOSTNAME library"
  (message (concat "Attempting to require host specific settings: custom-" (system-name)))
  (let ((init-host-feature (intern (concat "custom-" (system-name)))))
    (require init-host-feature nil 'noerror)))
```


<a id="screensaver-themes"></a>

# Themes


<a id="orgcab3e13"></a>

## Themes

```emacs-lisp
(use-package
  helm-themes
  :config (use-package
            panda-theme)
  (use-package
    dracula-theme)

  (use-package modus-operandi-theme
    :ensure t)

  (use-package modus-vivendi-theme
    :ensure t)

  (load-theme (if (daemonp) 'modus-vivendi 'panda)  t)
  )
```
