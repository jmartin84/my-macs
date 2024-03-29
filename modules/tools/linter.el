;;; package --- Summary:
;;; Commentary:
;;; Code:

(declare-function flycheck-add-mode "flycheck" (checker mode))

;;;###autoload
(defun my/bootstrap--tools-flycheck ()
	(message "Boostrap: tools-flycheck")

	(use-package flycheck
		:after (fringe-helper)
		:hook (after-init . global-flycheck-mode)
			(flycheck-mode . add-node-modules-path)
		:config
			(flycheck-add-mode 'javascript-eslint 'typescript-mode)
			(setq flycheck-indication-mode 'left-fringe)
			(fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
				".......X...."
				".......XX..."
				".......XXX.."
				"XXXXXXXXXXX."
				"XXXXXXXXXXXX"
				"XXXXXXXXXXX."
				".......XXX.."
				".......XX..."
				".......X....")
		)
	)
(provide 'linter)
;;; linter.el ends here
