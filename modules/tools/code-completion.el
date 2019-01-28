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
		:after (company)
		:hook (company-mode . company-box-mode)
		:custom
		(company-box-backends-colors
			'((company-dabbrev-code . "yellow")
			  (company-keywords . "red")
			  (company-etags . "red")
			  (company-gtags . "red")
			  (company-tern . "light blue")
				 ))
			(company-box-icons-unknown 'fa_question_circle)
			(company-box-icons-elisp
				'((fa_tag :face font-lock-function-name-face) ;; Function
					(fa_cog :face font-lock-variable-name-face) ;; Variable
					(fa_cube :face font-lock-constant-face) ;; Feature
					(md_color_lens :face font-lock-doc-face))) ;; Face

			(company-box-icons-yasnippet 'fa_bookmark)

			(company-box-icons-lsp
				'((1 . fa_text_height) ;; Text
					(2 . (fa_tags :face font-lock-function-name-face)) ;; Method
					(3 . (fa_tag :face font-lock-function-name-face)) ;; Function
					(4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
					(5 . (fa_cog :foreground "#FF9800")) ;; Field
					(6 . (fa_cog :foreground "#FF9800")) ;; Variable
					(7 . (fa_cube :foreground "#7C4DFF")) ;; Class
					(8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
					(9 . (fa_cube :foreground "#7C4DFF")) ;; Module
					(10 . (fa_cog :foreground "#FF9800")) ;; Property
					(11 . md_settings_system_daydream) ;; Unit
					(12 . (fa_cog :foreground "#FF9800")) ;; Value
					(13 . (md_storage :face font-lock-type-face)) ;; Enum
					(14 . (md_closed_caption :foreground "#009688")) ;; Keyword
					(15 . md_closed_caption) ;; Snippet
					(16 . (md_color_lens :face font-lock-doc-face)) ;; Color
					(17 . fa_file_text_o) ;; File
					(18 . md_refresh) ;; Reference
					(19 . fa_folder_open) ;; Folder
					(20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
					(21 . (fa_square :face font-lock-constant-face)) ;; Constant
					(22 . (fa_cube :face font-lock-type-face)) ;; Struct
					(23 . fa_calendar) ;; Event
					(24 . fa_square_o) ;; Operator
					(25 . fa_arrows)) ;; TypeParameter
				)
		)

	(use-package company-quickhelp
		:disabled t
		:after (company)
		:hook (company-mode . company-quickhelp-mode))


	(use-package lsp-mode
		:ensure t
		:commands lsp
		:hook (prog-mode . lsp)
		:custom
			(lsp-response-timeout 30)
			;; (lsp-clients-typescript-server "typescript-language-server")
			;; (lsp-clients-typescript-server-args '("--stdio"))
			(lsp-auto-guess-root t)
			(lsp-enable-xref t)
			(lsp-enable-eldoc t)
			(lsp-enable-indentation nil))

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

	)

(provide 'code-completion)
;;; code-completion.el ends here
