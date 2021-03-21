(use-package
  alert
  :demand
  :init
  (defvar rgr/alert-learn-history  nil "list of learns in rgr/alert-learn")
  (defgroup rgr/alert-learn nil "Options to control the Alert based popup learning" :group 'rgr)
  (defcustom rgr/alert-learn-on t "Whether pop up learning is on" :type 'boolean :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-command "fortune de" "Shell command to run to generate a learn" :type 'boolean :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-period 120 "How many seconds before another learn is displayed when idle" :type 'integer :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-fade-time 30 "How many seconds before the learn fades out" :type 'integer :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-history-auto-save  t "Auto save learn history?" :type 'boolean :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-history-file  (if (featurep 'no-littering) (no-littering-expand-var-file-name "alert-learn/alert-learn.txt" ) (expand-file-name "alert-learn.alist" user-emacs-directory)) "filename in which to save learns" :type '() :group 'rgr/alert-learn)
  (defcustom rgr/alert-learn-history-length 200 "How many learns to store in history" :type 'integer :group 'rgr/alert-learn)
  :config

  (defvar rgr/alert-learn-timer nil "timer object for alert-learn prompt")

  (let ((dir (file-name-directory rgr/alert-learn-history-file)))
    (unless (file-exists-p dir)
      (make-directory dir)))

  (defun rgr/alert-learn-history-save()
    (when rgr/alert-learn-history-auto-save
      (rgr/elisp-write-var rgr/alert-learn-history-file (butlast rgr/alert-learn-history (- (length rgr/alert-learn-history) rgr/alert-learn-history-length)))))

  (defun rgr/alert-learn-history-load()
    (when rgr/alert-learn-history-auto-save
      (setq rgr/alert-learn-history (rgr/elisp-read-var rgr/alert-learn-history-file))))

  (defun rgr/alert-learn-select-from-history()
    (interactive)
    (if rgr/alert-learn-history ;; select from old ones by bubbling one to top
        (let ((learn (completing-read "Select learn:" rgr/alert-learn-history)))
          (when learn
            (setq rgr/alert-learn-history (remove learn rgr/alert-learn-history))
            (add-to-list 'rgr/alert-learn-history learn)
            (rgr/alert-learn-history-save)))
      (message "No previous learns."))
    (car rgr/alert-learn-history))

  (defun rgr/alert-fortune(&optional title id)
    ;; alert a fortune and return it. (rgr/alert-fortune)
    (let ((f (shell-command-to-string rgr/alert-learn-command)))
      (alert  f :title (or title "Fortune favours the brave...") :id (or id 'emacs))
      f))

  (defun rgr/alert-learn()
    ;; alert a learn and store it. (rgr/alert-learn)
    (interactive)
    (let ((alert-fade-time rgr/alert-learn-fade-time)
          (learn (rgr/alert-fortune "Learn Now..." "learn")))
      (add-to-list 'rgr/alert-learn-history learn)
      (rgr/alert-learn-history-save)
      ;;
      )
    nil)

  (defun rgr/alert-learn-timer()
    "start or remove learn timer as appropriate."
    (if rgr/alert-learn-on
        (unless rgr/alert-learn-timer
          (setq rgr/alert-learn-timer
                (run-with-idle-timer rgr/alert-learn-period t 'rgr/alert-learn)))
      (when rgr/alert-learn-timer
        (cancel-timer rgr/alert-learn-timer)
        (setq rgr/alert-learn-timer nil))))

  (defun rgr/google-translate-learn(&optional prefix)
    "prefix 1 to toggle on/off, 2 to create a new learn, anything else to swap languages"
    (interactive "P")
    (when prefix
      (if (eq prefix 0) ;; C-u 0 to toggle on/off
          (progn
            (setq rgr/alert-learn-on (if rgr/alert-learn-on nil t))
            (rgr/alert-learn-timer))
        (if (eq prefix 1) ;; C-u 1 to create new learn
            (rgr/alert-learn)
          (if (eq prefix 2) ;; C-u 2 to select an old learn and relearn it
              (rgr/alert-learn-select-from-history)
            (google-translate-swap-default-languages)))))
    (when rgr/alert-learn-on
      (let ((learn (car rgr/alert-learn-history)))
        (when learn
          (google-translate-translate google-translate-default-source-language google-translate-default-target-language (car rgr/alert-learn-history))
          (if google-translate-pop-up-buffer-set-focus
              (select-window (display-buffer "*Google Translate*")))))))

  (rgr/alert-learn-history-load)

  (rgr/alert-learn-timer)

  :bind
  ("C-c L" . rgr/google-translate-learn))

(provide 'rgr/alert-learn)
