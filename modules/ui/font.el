;;; package --- Summary:
;;; Commentary:
;;; Code:
;;;###autoload
(defun my/bootstrap--ui-font ()
	(message "Bootstrap: ui-font")

	(load (concat modules-dir "vendor/font-lock+"))

	(setq whitespace-style
		(quote (face
				trailing
				tabs
;;				empty

				indention
				;; spaces
				;; space-mark
	;;			space-after-tab
	;;			space-before-tab
				tab-mark)))

	(setq whitespace-space-regexp "\\(^ +\\| +$\\)")
	(setq whitespace-hspace-regexp "\\(^\xA0+\\|\xA0+$\\)")

	(setq whitespace-display-mappings
		'((face)
			(space-mark 32 [183] [46])
			(tab-mark 9 [8594 9] [92 9])))


	(setq whitespace-display-mappings
		'(
			(face)
			(space-mark 32 [?·]) ; middle-dot
			(tab-mark 9 [8594 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
		))
	(add-hook 'find-file-hook #'whitespace-mode)

	(defun remove-dos-eol ()
		"Do not show ^M in files containing mixed UNIX and DOS line endings."
		(interactive)
		(setq buffer-display-table (make-display-table))
		(aset buffer-display-table ?\^M []))

	(add-hook 'prog-mode-hook #'remove-dos-eol)
	(add-hook 'text-mode-hook #'remove-dos-eol)


	(when (window-system)
		(add-hook 'helm-major-mode-hook
			(lambda ()
			(setq auto-composition-mode nil)))

		;; Set default font
		(set-face-attribute 'default nil
							:family "Fira Code"
							:height 135
							:weight 'normal
							:width 'normal))

  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
				(35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
				(36 . ".\\(?:>\\)")
				(37 . ".\\(?:\\(?:%%\\)\\|%\\)")
				(38 . ".\\(?:\\(?:&&\\)\\|&\\)")
				(42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
				(43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
				(45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
			   ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
				(47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
				(48 . ".\\(?:x[a-zA-Z]\\)")
				(58 . ".\\(?:::\\|[:=]\\)")
				(59 . ".\\(?:;;\\|;\\)")
				(60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
				(61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
				(62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
				(63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
				(91 . ".\\(?:]\\)")
				(92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
				(94 . ".\\(?:=\\)")
				(119 . ".\\(?:ww\\)")
				(123 . ".\\(?:-\\)")
				(124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
				(126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
				)
			  ))
	(dolist (char-regexp alist)
		(set-char-table-range composition-function-table (car char-regexp)
				`([,(cdr char-regexp) 0 font-shape-gstring]))))

)

(provide 'ui-font)
;;; font.el ends here
