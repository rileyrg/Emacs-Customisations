(use-package modus-themes
                                        ;:disabled
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  ;; (modus-themes-load-themes)
  :config
  (load-theme 'modus-operandi :no-confirm))
;; (modus-themes-load-vivendi))

(use-package ef-themes
  :disabled
  :demand t
  :config
  (ef-themes-select 'ef-duo-light))

(provide 'rgr/themes)
