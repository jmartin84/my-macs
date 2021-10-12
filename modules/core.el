;;; package --- Summary:
;;; Commentary:
;;; Code:

; Externals
(defvar modules-dir)
(declare-function my/bootstrap--ui "ui/ui.el" ())
(declare-function my/bootstrap--tools "tools/tools.el" ())
(declare-function my/bootstrap--lang "lang/lang.el" ())

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
