(use-package
  emms
  :disabled t
  :custom
  (emms-source-file-default-directory "~/Music" emms-info-asynchronously t emms-show-format "♪ %s")
  (emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  (emms-history-start-playing nil)
  :config
  (defun rgr/emms-play-url()
    (interactive)
    (let* ((url (thing-at-point-url-at-point))
           (url (if (and (not current-prefix-arg)
                         url) url (read-string (format "URL to play %s: " (if url url "")) nil
                         nil url))))
      (message "Playing: %s" url)
      (kill-new url)
      (emms-play-url url)))
  (defun rgr/emms-play-playlist()
    (interactive)
    (let(( emms-source-file-default-directory (expand-file-name "Playlists/" emms-source-file-default-directory)))
      (call-interactively 'emms-play-playlist)))
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (require 'emms-history)
  (emms-history-load)
  :bind ("C-c e e" . #'emms-smart-browse)
  ("C-c e j" . #'emms-seek-backward)
  ("C-c e l" . #'emms-seek-forward)
  ("C-c e p" . #'rgr/emms-play-playlist)
  ;;        ("C-c e p" . #'emms-play-playlist)
  ("C-c e <SPC>" . #'emms-pause)
  ("C-c e o" . #'rgr/emms-play-url)
  (:map emms-playlist-mode-map
        ("<SPC>" . #'emms-pause)
        ("j" . #'emms-seek-backward)
        ("l" . #'emms-seek-forward)
        ("k" . #'emms-pause)))
(provide 'rgr/emms)
