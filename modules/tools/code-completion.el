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
			(company-idle-delay nil)
			(company-minimum-prefix-length 2)
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

	(use-package company-quickhelp
		:after (company)
		:hook (company-mode . company-quickhelp-mode))


	(use-package lsp-mode
		:ensure t
		:commands lsp
		:hook (prog-mode . lsp)
		:custom
			(lsp-response-timeout 30)
			(lsp-clients-typescript-server "typescript-language-server")
			(lsp-clients-typescript-server-args '("--stdio"))
			(lsp-prefer-flymake :none)
			(lsp-auto-guess-root t)
			(lsp-enable-xref t)
			(lsp-enable-eldoc t)
			(lsp-ui-flycheck-enable nil)
			(lsp-enable-indentation nil))

	(use-package lsp-java)

	(use-package company-lsp
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
			(lsp-ui-flycheck-enable nil)
			(lsp-ui-sideline-show-hover nil))

	(use-package yasnippet
		:init (yas-global-mode 1))

	(setq gc-cons-threshold 100000000)
	(functionp 'json-serialize)
	(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; (setq lsp-prefer-capf t)

	)

(provide 'code-completion)
;;; code-completion.el ends here
