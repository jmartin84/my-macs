;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--ui-theme ()
	(message "Boostrap: ui-theme")
  (use-package doom-themes
	:custom
		(doom-themes-enable-bold t)
		(doom-themes-enable-italic t)
	:config
		;;(load-theme 'doom-nord t)
		;;(load-theme 'doom-city-lights t)
		;;(load-theme 'doom-spacegrey t)
		(load-theme 'doom-one t)
	  (doom-themes-visual-bell-config))

	(use-package solaire-mode
		:hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
		:config
		(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
		(solaire-mode-swap-bg)))

(provide 'ui-theme)
;;; theme.el ends here
