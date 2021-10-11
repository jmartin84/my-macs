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
		:config
		(dashboard-setup-startup-hook)

		(setq dashboard-items '((recents  . 5)
			(projects . 5)
			(agenda . 5)
			(registers . 5))))

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
