(require 'auth-source)
(defun get-auth-info (host user &optional port)
  "Interface to `auth-source-search' to fetch a secret for the HOST and USER."
  (let* ((info (nth 0 (auth-source-search
                       :host host
                       :user user
                       :port port
                       :require '(:user :secret)
                       :create nil)))
         (secret (plist-get info :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(use-package pass :disabled)

(provide 'rgr/security)
