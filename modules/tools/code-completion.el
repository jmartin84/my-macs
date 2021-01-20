;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--tools-completion ()
	(message "Boostrap: tools-completion")

	(use-package company
		:hook
			(after-init . global-company-mode)
		:bind (("C-<SPC>" . company-complete))
		:custom
			(company-begin-commands '(self-insert-command))
			(company-idle-delay 0.0)
			(company-minimum-prefix-length 1)
			(company-show-numbers nil)
			(company-tooltip-visible-p t)
			(company-tooltip-align-annotations 't)
			(company-require-match 'never)
			(company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)))

	(use-package company-box
		:after (company all-the-icons)
		:hook (company-mode . company-box-mode)
		:custom
			(company-box-doc t)
			(company-box-icons-alist 'company-box-icons-all-the-icons))


	(use-package lsp-mode
		:ensure t
		:commands lsp
		:hook (prog-mode . lsp)
			  (js2-mode . lsp-headerline-breadcrumb-mode)
			  (js-jsx-mode . lsp-headerline-breadcrumb-mode)
			  (typescript-mode . lsp-headerline-breadcrumb-mode)
		:custom
			(lsp-modeline-code-actions-segments `(count icon name))
			(lsp-response-timeout 30)
;;			(lsp-clients-typescript-server "typescript-language-server")
	;;		(lsp-clients-typescript-server-args '("--stdio"))
			;;(lsp-prefer-flymake :none)
			(lsp-clients-elixir-server-executable "~/code/elixir/elixir-ls/language_server.sh")
			(lsp-diagnostics-provider :flycheck)
			(lsp-auto-guess-root t)
			(lsp-enable-xref t)
			(lsp-enable-eldoc t)
			;;(lsp-flycheck-enable t)
			(lsp-hover t)
			(lsp-prefer-capf t)
		(lsp-enable-indentation nil)
		)

	(use-package lsp-treemacs
		:commands lsp-treemacs-errors-list
		:after (lsp-mode))

	(use-package dap-mode
		:after (lsp-mode))

	(use-package company-lsp
		:disabled t
		:commands company-lsp
		:custom
			(company-lsp-async t)
			(company-lsp-cache-candidates 'auto))

	(use-package lsp-ui
		:commands lsp-ui-mode
		:custom
			(lsp-ui-doc-enable t)
			(lsp-ui-peek-enable t)
			(lsp-ui-sideline-enable nil)
			(lsp-ui-imenu-enable nil)
			;;(lsp-ui-flycheck-enable t)
			(lsp-ui-sideline-show-hover nil))

	(setq lsp-eslint-server-command
	'("node"
		"/Users/justen/code/opensource/vscode-eslint/server/out/eslintServer.js"
		"--stdio"))

	(use-package yasnippet
		:init (yas-global-mode 1))

	(setq gc-cons-threshold 100000000)
	(functionp 'json-serialize)
	(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; (setq lsp-prefer-capf t)

	)

(provide 'code-completion)
;;; code-completion.el ends here
