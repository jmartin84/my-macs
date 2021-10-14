;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--ui-theme ()
  (message "Boostrap: ui-theme")

	(use-package page-break-lines)

	(use-package rainbow-delimiters
		:hook (prog-mode . rainbow-delimiters-mode))

	(use-package dashboard
		:ensure t
		:custom
			(dashboard-set-init-info t)
			(dashboard-set-navigator nil)
			(dashboard-set-heading-icons t)
			(dashboard-set-file-icons t)
			(dashboard-center-content t)
			(dashboard-startup-banner 'logo)
			(dashboard-show-shortcuts t)

			(dashboard-navigator-buttons
				`(;; line1
					((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
					"Homepage"
					"Browse homepage"
					(lambda (&rest _) (browse-url "homepage")))
					("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
					("?" "" "?/h" #'show-help nil "<" ">"))
					;; line 2
					((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
					"Linkedin"
					""
					(lambda (&rest _) (browse-url "homepage")))
					("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))

			(dashboard-items '((recents  . 5)
				(projects . 5)
				))
		:config
			(dashboard-setup-startup-hook))

 (use-package rainbow-mode)

  (use-package doom-themes
	 :ensure t
	:after (all-the-icons )
	:custom
		(doom-themes-enable-bold t)
		(doom-themes-enable-italic t)
		(doom-themes-treemacs-file-icons t)
	:config
	  ;;(load-theme 'doom-snazzy t)
	  ;;(load-theme 'doom-ephemeral t)
		;;(load-theme 'doom-nord t)
		;;(load-theme 'doom-city-lights t)
		;;(load-theme 'doom-dark+)
		;;(load-theme 'doom-vibrant t)
		;; (load-theme 'doom-spacegrey t)
		 (load-theme 'doom-one t)
		(setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
	  (doom-themes-treemacs-config)
	  (require 'doom-themes-ext-treemacs)

	  (doom-themes-visual-bell-config))

(use-package solaire-mode
  :after (doom-themes)
  :config (solaire-global-mode +1)
  )
)
(provide 'ui-theme)
;;; theme.el ends here
