;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--tools-vcs ()
	(message "Boostrap: tools-vcs")

	(use-package git-gutter+
		:hook (after-init . global-git-gutter+-mode))

	(use-package git-gutter-fringe+
		:after (git-gutter+ fringe-helper)
		:init
			(setq git-gutter-fr+-side 'right-fringe)
			(setq-default right-fringe-width 8)
			(setq-default fringes-outside-margins t)
		:config
			(set-face-foreground 'git-gutter+-modified "blue")
			(set-face-foreground 'git-gutter+-added    "green")
			(set-face-foreground 'git-gutter+-deleted  "red")
			(fringe-helper-define 'git-gutter-fr+-added '(center repeated) "XXXX....")

			(fringe-helper-define 'git-gutter-fr+-deleted '(center repeated) "XXXX....")
			(fringe-helper-define 'git-gutter-fr+-modified '(center repeated) "XXXX...."))

	)
(provide 'vcs)
;;; vcs.el ends here
