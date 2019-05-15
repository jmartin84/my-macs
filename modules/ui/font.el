;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--ui-font ()
  (message "Bootstrap: ui-font")
  (setq whitespace-style (quote (
                                 face
                                 trailing
                                 tabs
								empty
								indention
                                 tab-mark
                                 )))

  (setq whitespace-display-mappings
        '(
          (face)
          (newline)
          (tab-mark 9 [8594 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
          ))
  (add-hook 'find-file-hook #'whitespace-mode)
  (when (window-system)
    (add-hook 'helm-major-mode-hook
	    (lambda ()
	      (setq auto-composition-mode nil)))
    (set-default-font "Fira Code 14"))
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
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))
(provide 'ui-font)
;;; font.el ends here
