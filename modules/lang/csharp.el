;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--lang-csharp ()
	(message "Boostrap: lang-csharp")

	(use-package omnisharp
		:after(flycheck company)
		:hook (csharp-mode . omnisharp-mode)
		:custom
			(omnisharp-debug t)
			(omnisharp-server-executable-path "/usr/local/bin/omnisharp")
		:init (add-to-list 'company-backends #'company-omnisharp)
			(which-key-add-major-mode-key-based-replacements 'csharp-mode
				"<SPC> m" "dotnet"
				"<SPC> mc" "compile project"
				"<SPC> me" "solution errors"
				"<SPC> mf" "find"
				"<SPC> mfd" "definitions"
				"<SPC> mfi" "implementations"
				"<SPC> mfs" "symbols"
				"<SPC> mfu" "usages"
				"<SPC> mr" "refactor"
				"<SPC> mrr" "rename"
				"<SPC> ms" "server"
				"<SPC> msc" "check status"
				"<SPC> msi" "install"
				"<SPC> msr" "reload solution"
				"<SPC> mss" "start"
				"<SPC> msS" "stop"
				"<SPC> msv" "version info"
				"<SPC> m=" "format"
				"<SPC> m=f" "file"
				"<SPC> m=r" "region"
				)

		(evil-leader/set-key-for-mode 'csharp-mode
			"mc" 'recompile
			"me" 'omnisharp-solution-errors
			"mfd" 'omnisharp-go-to-definition
			"mfi" 'omnisharp-find-implementations
			"mfs" 'omnisharp-helm-find-symbols
			"mfu" 'omnisharp-helm-find-usages

			"mrr" 'omnisharp-rename-interactively

			"msc" 'omnisharp-check-alive-status
			"msi" 'omnisharp-install-server
			"msr" 'omnisharp-reload-solution
			"mss" 'omnisharp-start-omnisharp-server
			"msS" 'omnisharp-stop-server

			"m=f" 'omnisharp-code-format-file
			"m=r" 'omnisharp-code-format-region
			)
		))
(provide 'csharp)
;;; csharp.el ends here
