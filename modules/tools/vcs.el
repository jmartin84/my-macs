;;; package --- Summary:
;;; Commentary:
;;; Code:

(defun my/configure-magit-hydra ()
	(defhydra hydra-magit-menu (:color blue
								:hint nil
							    :foreign-keys run
								:body-pre (progn
										 (unless (bound-and-true-p magit-blame-mode)
											 (magit-blame)))
								:post (progn
										(if (bound-and-true-p magit-blame-mode)
											(hydra-magit-menu/body)
											(progn
												(hydra-keyboard-quit)
												(magit-blame-mode nil)
												)))) "

Press [_b_] again to blame further in the history, [_q_] to go up or quit."
		("b" magit-blame :exit nil)
		("q" (progn
				 (when (bound-and-true-p magit-blame-mode)
					 (magit-blame-quit)))
			:exit t)))

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

	(use-package magit
		:ensure t
		:init (my/configure-magit-hydra))
	)
(provide 'vcs)
;;; vcs.el ends here
