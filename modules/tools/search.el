;;; search.el --- Summary
;;; Commentary:
;;; Code:


(defun my/ahs-move-forward () (interactive)
	(ahs-highlight-now)
	(evil-set-jump)
	(ahs-forward))

(defun my/ahs-move-backwards () (interactive)
	(ahs-highlight-now)
	(evil-set-jump)
	(ahs-backward))

;;;###autoload
(defun my/ahs () (interactive)
	(ahs-highlight-now)
	(doom-modeline-def-segment matches
		(propertize
			(let* ((stats (cdr (ahs-stat)))
					(total (first stats))
					(previous-results (second stats)))
				(format " %s/%d "
					(if (eq 0 total)
						0
						(+ 1 previous-results))
					total))
			'face 'doom-modeline-panel))
	(hydra-ahs-menu/body))

(defun my/configure-ahs-hydra ()
	(defhydra hydra-ahs-menu (:color blue
									:hint nil
									:timeout 5
									:post (progn
											(doom-modeline-def-segment matches (propertize "" 'face 'doom-modeline-panel))
											(ahs-clear))) "
		[_n_] next  [_N_] previous   [_e_] iedit"

				("n" my/ahs-move-forward :exit nil)
				("N" my/ahs-move-backwards :exit nil)
				("e" my/not-implemented)))

;;;###autoload
(defun my/bootstrap--tools-search ()
	(message "Boostrap: tools-search")

	;; helm
	(use-package helm
	:pin melpa-stable
	:custom
		(helm-split-window-inside-p t)
		(helm-mode-fuzzy-match t)
		(helm-completion-in-region-fuzzy-match t))

	;; helm-ag
	(use-package helm-ag
	:pin melpa-stable
		:after (helm))

	(use-package auto-highlight-symbol
		:after (hydra)
		:custom
			(ahs-case-fold-search t)
			(ahs-default-range 'ahs-range-whole-buffer)
			(ahs-idle-timer 0)
			(ahs-idle-interval 0.25)
			(ahs-inhibit-face-list nil)
			(auto-highlight-symbol-mode-map (make-sparse-keymap))
		:init (global-auto-highlight-symbol-mode)
		:config (my/configure-ahs-hydra))


	)

(provide 'search)

;;; search.el ends here
