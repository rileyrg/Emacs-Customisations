;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
;; Maintained in emacs-config.org
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache "var/eln-cache"))
(setq max-specpdl-size 13000)

(setq package-enable-at-startup nil)
