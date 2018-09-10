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
		;;(load-theme 'doom-city-lights t)
		(load-theme 'doom-spacegrey t)
		(doom-themes-visual-bell-config)))

(provide 'ui-theme)
;;; theme.el ends here
