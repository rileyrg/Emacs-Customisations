;;; rgr-early-init.el  -*- no-byte-compile: t -*-
(use-package
  auto-compile
  :init
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

(defun load-el-gpg (load-dir)
  (message "attempting mass load from %s." load-dir)
  (when (file-exists-p load-dir)
    (dolist (f (directory-files-recursively load-dir "\.[el|gpg]$"))
      (condition-case nil
          (progn
            (message "load-el-gpg loading %s" f)
            (load f 'no-error))
        (error nil)))))

(load-el-gpg (no-littering-expand-etc-file-name "early-load"))

(load-el-gpg (expand-file-name (system-name)  (no-littering-expand-etc-file-name "hosts")))

(provide 'rgr/early-init)
