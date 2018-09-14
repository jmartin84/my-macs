
;;; project.el --- Summary
;;; Commentary:
;;; Code:

;; from https://github.com/jaypei/emacs-neotree/issues/149
;;;###autoload
(defun my/open-neotree-project-root-or-current-dir ()
  "Open NeoTree using the project root, using projectile, or the current buffer directory."
  (interactive)
  (let ((project-dir (ignore-errors (projectile-project-root)))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))


;;;###autoload
(defun my/bootstrap--tools-project ()
	(message "Boostrap: tools-project")

	;;projectile
	(use-package projectile
		:pin melpa-stable
		:hook (after-init . projectile-mode))

	;; helm-projectile
	(use-package helm-projectile
		:pin melpa-stable
		:requires (helm projectile))

	;; neo-tree
	(use-package neotree
		:after (doom-themes)
		:custom
			;;(setq neotree-switch-project-action 'neotree-projectile-action)
			(neo-window-width 32)
			(neo-create-file-auto-open nil)
			(neo-banner-message "Press ? for neotree help")
			(neo-show-updir-line nil)
			(neo-mode-line-type 'neotree)
			(neo-smart-open nil)
			(neo-dont-be-alone t)
			(neo-persist-show nil)
			(neo-auto-indent-point t)
			(neo-vc-integration t)
			(neo-autorefresh nil)
			(neo-theme 'icons)
			(doom-neotree-file-icons t)
		:after (evil doom-themes projectile all-the-icons)
		:config
			(doom-themes-neotree-config)
		))

(provide 'projects)

;;; projects.el ends here
