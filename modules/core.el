;;; package --- Summary:
;;; Commentary:
;;; Code:

; Externals
(defvar modules-dir)

;;;###autoload
(defun my/delete-buffer-and-window (buffer)()
	(progn (delete-windows-on buffer)
			(kill-buffer buffer)))

;;;###autoload
(defun my/bootstrap ()
	(message "Bootstrap")
	(setq create-lockfiles nil)

	; import autoloads
	(load (concat modules-dir "ui/.autoload"))
	(load (concat modules-dir "tools/.autoload"))
	(load (concat modules-dir "lang/.autoload"))

	(use-package exec-path-from-shell
		:ensure t
		:config (when (memq window-system '(mac ns x))
					(exec-path-from-shell-initialize)))

	; bootstrap submodules
	(my/bootstrap--ui)
	(my/bootstrap--tools)
	(my/bootstrap--lang))



(provide 'core)
;;; core.el ends here
