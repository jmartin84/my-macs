;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--lang-elixir ()
	(message "Boostrap: lang-elixir")

	(use-package elixir-mode
		:init
		(which-key-add-major-mode-key-based-replacements 'elixir-mode
			"<SPC> mf" "find"
			"<SPC> mfd" "definitions"
			"<SPC> mfi" "implementations"
			"<SPC> mfs" "symbols"
			"<SPC> mfu" "usages"
			"<SPC> mr" "refactor"
			"<SPC> mrr" "rename"
			"<SPC> m=" "format"
			"<SPC> m=f" "file"
			"<SPC> m=r" "region"
			)

		(use-package flycheck-dialyxir
			:after (flycheck)
			:init
			(flycheck-dialyxir-setup))

		(use-package flycheck-credo
			:after (flycheck)
			:init
			(flycheck-credo-setup)
			(add-hook 'elixir-mode-hook 'flycheck-mode))

	(evil-leader/set-key-for-mode 'elixir-mode
		"mfd" 'lsp-ui-peek-find-definitions
		"mfi" 'lsp-ui-peek-find-implementation
		"mfs" 'lsp-ui-peek-find-workspace-symbol
		"mfu" 'lsp-ui-peek-find-references

		"mrr" 'lsp-rename

		"m=f" 'lsp-format-buffer
		"m=r" 'lsp-format-region))

	;; (use-package alchemist
	;; 	:after (elixir-mode)
	;; 	:disabled t
	;; 	:custom
	;; 	(alchemist-mix-command "/usr/local/bin/mix")
	;; 	(alchemist-iex-program-name "/usr/local/bin/iex")
	;; 	(alchemist-execute-command "/usr/local/bin/elixir")
	;; 	(alchemist-compile-command "/usr/local/bin/elixirc")
	;; 	)

	 )
(provide 'elixir)
;;; elixir.el ends here
