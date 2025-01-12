(use-package vterm)
(use-package  multi-vterm
  :after vterm
  :init
  (add-to-list 'display-buffer-alist  '((or (major-mode . vterm-mode))
                                        (display-buffer-reuse-mode-window
                                         display-buffer-in-direction)
                                        (direction . below)
                                        (window-height . 0.3)))
  :bind
  ("M-g t" . multi-vterm-project))

(provide 'rgr/shells)
