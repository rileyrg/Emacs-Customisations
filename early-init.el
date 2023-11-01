;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
;; Maintained in emacs-config.org
(setq package-enabled-at-startup nil)
(setq max-specpdl-size 13000)
;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "var/eln-cache" user-emacs-directory)))
;;; early-init.el ends here
