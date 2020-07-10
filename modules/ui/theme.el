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

  (use-package doom-themes
	:after (all-the-icons neotree)
	:custom
		(doom-themes-enable-bold t)
		(doom-themes-enable-italic t)
		(doom-themes-neotree-file-icons t)
	:config
		;;(load-theme 'doom-nord t)
		;;(load-theme 'doom-city-lights t)
		(load-theme 'doom-vibrant t)
		;; (load-theme 'doom-spacegrey t)
		;; (load-theme 'doom-one t)
	  (doom-themes-neotree-config)
	  (require 'doom-themes-ext-neotree)
	  (doom-themes-visual-bell-config))

	(use-package solaire-mode
		:hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
		:config
		(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
		(solaire-mode-swap-bg)))

(provide 'ui-theme)
;;; theme.el ends here
