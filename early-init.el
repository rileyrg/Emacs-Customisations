;;; early-init.el --- early bird  -*- no-byte-compile: t; lexical-binding: t; -*-
;; Maintained in emacs-config.org
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache "var/eln-cache"))
(setq max-specpdl-size 13000)
(setq warning-suppress-log-types '((files missing-lexbind-cookie)))

(setq package-enable-at-startup nil)

(defcustom rgr/elisp-dir (expand-file-name "etc/elisp" user-emacs-directory)
  "Where user elisp files should be stored."
  :type 'directory
  :group 'rgr)

(defun rgr/user-elisp-file (f)
  "Return the full path to a user Emacs Lisp file F."
  (expand-file-name f rgr/elisp-dir))

;; Ensure the directory exists before adding it to load-path
(make-directory rgr/elisp-dir t)

;; Add the directory to the load-path, preferring to the front and avoiding duplicates.
(add-to-list 'load-path rgr/elisp-dir)
