
;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--lang-elixir ()
	(message "Boostrap: lang-elixir")

	(use-package elixir-mode)
	(use-package alchemist
		:after (elixir-mode)
		:custom
		(alchemist-mix-command "/usr/local/bin/mix")
		(alchemist-iex-program-name "/usr/local/bin/iex")
		(alchemist-execute-command "/usr/local/bin/elixir")
		(alchemist-compile-command "/usr/local/bin/elixirc")
		)

	)
(provide 'elixir)
;;; elixir.el ends here
