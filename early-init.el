;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
;; Maintained in emacs-config.org
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache "var/eln-cache"))
(setq max-specpdl-size 13000)

(setq package-enable-at-startup nil)

(defcustom rgr/elisp-dir (expand-file-name  "etc/elisp" user-emacs-directory) "Where user elisp files should be stored")
(defun rgr/user-elisp-file(f)
  (expand-file-name f rgr/elisp-dir))
(setq load-path (cons rgr/elisp-dir load-path))
