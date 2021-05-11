;;; rgr-early-init.el  -*- no-byte-compile: t -*-
(use-package
  auto-compile
  :init
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))
(provide 'rgr/early-init)
