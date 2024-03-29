
;;; project.el --- Summary
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/toggle-treemacs ()
  "Toggle Treemacs"
  (interactive)
	(let ((treemacs-buffer (try-completion " *Treemacs-" (mapcar #'buffer-name (buffer-list)))))
		(if (eq treemacs-buffer nil)
			(treemacs-display-current-project-exclusively)
			(my/delete-buffer-and-window treemacs-buffer))))

;; from https://github.com/jaypei/emacs-neotree/issues/149
;;;###autoload
(defun my/open-neotree-project-root-or-current-dir ()
  "Toggle Neotree"
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
		:ensure t
		:hook (after-init . projectile-mode))

	;; helm-projectile
	(use-package helm-projectile
		:after (helm projectile))

	;; neo-tree
	;; (use-package neotree
	;; 	:custom
	;; 		;;(setq neotree-switch-project-action 'neotree-projectile-action)
	;; 		(neo-window-width 32)
	;; 		(neo-create-file-auto-open nil)
	;; 		(neo-banner-message "Press ? for neotree help")
	;; 		(neo-show-updir-line nil)
	;; 		(neo-mode-line-type 'neotree)
	;; 		(neo-smart-open nil)
	;; 		(neo-dont-be-alone t)
	;; 		(neo-persist-show nil)
	;; 		(neo-auto-indent-point t)
	;; 		(neo-vc-integration t)
	;; 		(neo-autorefresh t)
	;; 		(neo-theme 'icon)
	;; 	:after (evil projectile))
(use-package treemacs
  :ensure t
  ;;:defer t
  ;; :init
  ;; (with-eval-after-load 'winum
  ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          t
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-width                           35
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; (use-package treemacs-icons-dired
;;   :after (treemacs dired)
;;   :ensure t
;;   :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)
	)
(provide 'projects)

;;; projects.el ends here
