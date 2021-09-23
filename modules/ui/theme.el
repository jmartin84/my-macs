;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--ui-theme ()
  (message "Boostrap: ui-theme")

	(use-package page-break-lines)


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
	:after (all-the-icons neotree)
	:custom
		(doom-themes-enable-bold t)
		(doom-themes-enable-italic t)
		(doom-themes-neotree-file-icons t)
	:config
	  ;;(load-theme 'doom-snazzy t)
	  ;;(load-theme 'doom-ephemeral t)
		;;(load-theme 'doom-nord t)
		;;(load-theme 'doom-city-lights t)
		;;(load-theme 'doom-dark+)
		;;(load-theme 'doom-vibrant t)
		;; (load-theme 'doom-spacegrey t)
		 (load-theme 'doom-one t)
	  (doom-themes-neotree-config)
	  (require 'doom-themes-ext-neotree)
	  (doom-themes-visual-bell-config))

	(use-package solaire-mode
		:hook (doom-load-theme . solaire-global-mode)
			(which-key-init-buffer . solaire-mode)
	)

(use-package solaire-mode
  :after (doom-themes)
  :config (solaire-global-mode +1)
  )
)
(provide 'ui-theme)
;;; theme.el ends here
