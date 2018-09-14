;;; package --- Summary:
;;; Commentary:
;;; Code:

; Externals
(defvar modules-dir)

(defun my/init-exec-path () ()
    (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))


;;;###autoload
(defun my/bootstrap ()
	(message "Bootstrap")
	(setq create-lockfiles nil)

	; import autoloads
	(load (concat modules-dir "ui/.autoload"))
	(load (concat modules-dir "tools/.autoload"))

	(use-package exec-path-from-shell
		:ensure t
		:config (my/init-exec-path))

	; bootstrap submodules
	(my/bootstrap--ui)
	(my/bootstrap--tools))



(provide 'core)
;;; core.el ends here
