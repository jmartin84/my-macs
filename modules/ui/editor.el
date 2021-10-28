;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--ui-editor ()
	(message "Boostrap: ui-editor")

	(setq evil-want-keybinding nil)

	;; evil mode config
	(use-package evil-leader
		:ensure t
		:init
			(setq evil-want-keybinding nil)
			(global-evil-leader-mode)
			(evil-leader/set-leader "<SPC>"))


	(use-package evil
		:ensure t
		:init
			(setq evil-want-keybinding nil)
		:custom
			(evil-insert-state-cursor 'bar)
			(evil-undo-system 'undo-fu)
		:config
			(define-key evil-visual-state-map ">" (lambda ()
				(interactive)
				(when (> (mark) (point))
					(exchange-point-and-mark)
				)
				(evil-normal-state)
				(evil-shift-right (mark) (point))
				(evil-visual-restore)
			))

			(define-key evil-visual-state-map "<" (lambda ()
				(interactive)
				(when (> (mark) (point))
					(exchange-point-and-mark)
				)
				(evil-normal-state)
				(evil-shift-left (mark) (point))
				(evil-visual-restore)))
			(evil-mode 1))

	(use-package undo-fu
		:config
			(global-unset-key (kbd "C-z"))
			(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
			(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

	(use-package evil-collection
		:after evil
		:init (setq evil-want-keybinding nil)
		:ensure t
		:config
		(evil-collection-init)))
(provide 'ui-editor)
;;; editor.el ends here
