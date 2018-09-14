;;; init.el --- Summary
;;; Commentary:
;;; Code:
(message "****Initializing Emacs****")
;; package config
(message "--Configuring package management--")
(require 'package)
(setq package-enable-at-startup nil)
(setq use-package-always-ensure t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar emacs-dir (file-truename user-emacs-directory) "Path to Emacs dir.")
(defvar modules-dir (concat emacs-dir "modules/") "Path to modules dir.")

(eval-when-compile
  (require 'use-package))

(let ((default-directory  modules-dir))
  (normal-top-level-add-subdirs-to-load-path))

(load (concat modules-dir "core"))

(defun my/log-package-init (packageName) (message (format "Initializing package: %s" packageName)))

(defun my/not-implemented () (message "**NOT YET IMPLEMENTED**"))

(defun my/open-messages-buffer () (interactive) (switch-to-buffer "*Messages*"))

(defun my/restart-and-debug () (interactive) (restart-emacs '("--debug-init")))

(defun my/kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun my/reload-dot-file () (interactive)
       (load-file "~/.emacs.d/init.el"))
(defun my/find-dot-file () (interactive)
       (find-file "~/.emacs.d/init.el"))



;;variables
(setq my/which-key-map-prefixes '(
    ("<SPC> <SPC>" "M-x")
    ("<SPC> a" "applications")
    ("<SPC> ad" "dired")
    ("<SPC> b" "buffer")
    ("<SPC> bb" "buffer list")
    ("<SPC> bd" "kill buffer")
    ("<SPC> bm" "open message buffer")
    ("<SPC> bo" "kill other buffers")
    ("<SPC> bs" "open scratch buffer")
    ("<SPC> bY" "yank whole buffer")
    ("<SPC> f" "files")
    ("<SPC> fe" "emacs")
    ("<SPC> fed" "find dotfile")
    ("<SPC> fer" "reload dotfile")
    ("<SPC> p" "project")
    ("<SPC> pb" "buffers")
    ("<SPC> pf" "find file")
    ("<SPC> ps" "special files")
    ("<SPC> pse" "open editorconfig")
    ("<SPC> po" "open project")
    ("<SPC> pt" "project explorer")
    ("<SPC> s" "search")
    ("<SPC> sA" "search all files")
    ("<SPC> sb" "search open buffers")
    ("<SPC> sf" "search this file")
    ("<SPC> sh" "symbol highlight")
    ("<SPC> sp" "search current project")
    ("<SPC> w" "window")
    ("<SPC> wd" "delete window")
    ("<SPC> wh" "focus window left")
    ("<SPC> wH" "move window left")
    ("<SPC> wj" "focus window below")
    ("<SPC> wJ" "move window down")
    ("<SPC> wk" "focus window above")
    ("<SPC> wK" "move window up")
    ("<SPC> wl" "focus window right")
    ("<SPC> wL" "move window right")
    ("<SPC> wS" "split window horizontally")
    ("<SPC> wV" "split window vertically")
    ("<SPC> q" "quit")
    ("<SPC> qd" "quit, restart, and debug")
    ("<SPC> qq" "quit and save")
    ("<SPC> qr" "quit and restart")))


;; evil mode config
(use-package evil-leader
  :ensure t
  :pin melpa-stable
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :init
    (setq evil-insert-state-cursor 'bar)
   :config
    (evil-mode 1)

	;; from https://emacs.stackexchange.com/questions/20151/how-to-rebind-evil-key-mappings-for-delete-and-friends
	;; reselects visualmode selection after indent with ><
	(define-key evil-visual-state-map ">" (lambda ()
		(interactive)
		; ensure mark is less than point
		(when (> (mark) (point))
			(exchange-point-and-mark)
		)
		(evil-normal-state)
		(evil-shift-right (mark) (point))
		(evil-visual-restore) ; re-select last visual-mode selection
	))

	(define-key evil-visual-state-map "<" (lambda ()
		(interactive)
		; ensure mark is less than point
		(when (> (mark) (point))
			(exchange-point-and-mark)
		)
		(evil-normal-state)
		(evil-shift-left (mark) (point))
		(evil-visual-restore) ; re-select last visual-mode selection
	))
  :pin melpa-stable
  :ensure t
  :after (evil-leader))

;; which-key
(use-package which-key
  :pin melpa-stable
  :config
    ;; set menu text
    (dolist (prefix my/which-key-map-prefixes) (apply 'which-key-add-key-based-replacements prefix))
    ;; keybinds - general
    (evil-leader/set-key "<SPC>" 'helm-M-x)

	;; keybinds - applications
	(evil-leader/set-key "ad" 'dired)

    ;; keybings - buffer
    (evil-leader/set-key "bb" 'helm-mini)
    (evil-leader/set-key "bd" 'kill-this-buffer)
    (evil-leader/set-key "bm" 'my/open-messages-buffer)
    (evil-leader/set-key "bo" 'my/kill-other-buffers)
    (evil-leader/set-key "bs" '(lambda () (interactive)(switch-to-buffer "*scratch*")))
    (evil-leader/set-key "bY" 'my/not-implemented)


    ;; keybinds - file
    (evil-leader/set-key "fed" 'my/find-dot-file)
    (evil-leader/set-key "fer" 'my/reload-dot-file)

    ;;project management
    (evil-leader/set-key "pb" 'helm-projectile-switch-to-buffer)
    (evil-leader/set-key "pf" 'helm-projectile-find-file)
    (evil-leader/set-key "pse" 'editorconfig-find-current-editorconfig)
    (evil-leader/set-key "po" 'helm-projectile-switch-project)
    (evil-leader/set-key "pt" 'my/open-neotree-project-root-or-current-dir)

    ;; search
    (evil-leader/set-key "sA" 'helm-do-ag)
    (evil-leader/set-key "sb" 'helm-ag-buffers)
    (evil-leader/set-key "sf" 'helm-ag-this-file)
    (evil-leader/set-key "sh" 'my/ahs)
    (evil-leader/set-key "sp" 'helm-projectile-ag)

    ;; keybinds - window
    (evil-leader/set-key "wd" 'delete-window)
    (evil-leader/set-key "wh" 'evil-window-left)
    (evil-leader/set-key "wH" 'evil-window-move-far-left)
    (evil-leader/set-key "wj" 'evil-window-down)
    (evil-leader/set-key "wJ" 'evil-window-move-very-bottom)
    (evil-leader/set-key "wk" 'evil-window-up)
    (evil-leader/set-key "wK" 'evil-window-move-far-top)
    (evil-leader/set-key "wl" 'evil-window-right)
    (evil-leader/set-key "wL" 'evil-window-move-far-right)
    (evil-leader/set-key "wV" 'split-window-horizontally)
    (evil-leader/set-key "wS" 'split-window-vertically)
    (evil-leader/set-key "qd" 'my/restart-and-debug)
    (evil-leader/set-key "qq" 'save-buffers-kill-terminal)
    (evil-leader/set-key "qr" 'restart-emacs)


	;;neotree
	(evil-define-key 'normal neotree-mode-map (kbd "k") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "j") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "l") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "s") 'neotree-hidden-file-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-create-node)
    (evil-define-key 'normal neotree-mode-map (kbd "C") 'neotree-copy-node)
    (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-rename-node)
    (evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node)
    (evil-define-key 'normal neotree-mode-map (kbd "gr") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "?") 'my/not-implemented)


    (which-key-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "gray22")
	'(package-selected-packages
		 (quote
			 (hydra auto-highlight-symbol all-the-icons-dired "epl" "epm" company-terraform terraform-mode omnisharp omnisharp-mode yaml-mode prettier-js add-node-modules-path protobuf-mode rjsx-mode json-mode lsp-ui lsp-javascript-typescript js2-mode company-lsp lsp-mode company-next rainbow-delimiters flycheck git-gutter+ git-gutter-fringe+ fringe-helper git-gutter editorconfig evil-anzu doom-modeline exec-path-from-shell helm-projectile restart-emacs autopair frame-local ov s projectile company-quickhelp icons-in-terminal string-trim all-the-icons company-box company company-mode jbeans jbeans-theme which-key use-package helm evil-leader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-neotree-media-file-face ((t (:inherit doom-neotree-hidden-file-face :foreground "dark gray")))))



(defun my/company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun my/company-js-hook nil
  (make-local-variable 'company-transformers)
  (push 'my/company-transformer company-transformers))

(use-package company
  :defer 2
  :diminish
  :hook
	(js-mode . my/company-js-hook)
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

(load (concat modules-dir "vendor/font-lock+"))

(use-package company-box
	:disabled t
	:hook (company-mode . company-box-mode)
	:init
	(setq company-box-icons-unknown 'fa_question_circle)

	(setq company-box-icons-elisp
	'((fa_tag :face font-lock-function-name-face) ;; Function
		(fa_cog :face font-lock-variable-name-face) ;; Variable
		(fa_cube :face font-lock-constant-face) ;; Feature
		(md_color_lens :face font-lock-doc-face))) ;; Face

	(setq company-box-icons-yasnippet 'fa_bookmark)

	(setq company-box-icons-lsp
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
  :pin melpa-stable
  :after (company)
  :commands (company-quickhelp-mode)
  :hook (company-mode . company-quickhelp-mode))

;; restart emacs
(use-package restart-emacs :pin melpa-stable)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package git-gutter+
	:hook (after-init . global-git-gutter+-mode))

(use-package git-gutter-fringe+
	:ensure t
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

(use-package add-node-modules-path
	:defer t)


(use-package rainbow-delimiters
	:pin melpa-stable
	:hook (prog-mode . rainbow-delimiters-mode))

(use-package lsp-mode
	:ensure t
	:custom
	(lsp-enable-eldoc nil)
	(lsp-enable-indentation nil))

(use-package company-lsp
	:after (company lsp-mode)
	:init (push 'company-lsp company-backends)
	:custom
		(company-lsp-async t)
		(company-lsp-cache-candidates nil))

(use-package lsp-ui
	:disabled t
	:hook (lsp-mode . lsp-ui-mode)
	:after (lsp-mode)
	:custom
		(lsp-ui-doc-enable nil)
		(lsp-ui-peek-enable t)
		(lsp-ui-sideline-enable t)
		(lsp-ui-imenu-enable nil)
		(lsp-ui-flycheck-enable t)
		(lsp-ui-sideline-show-hover nil))


(use-package js2-mode
	:init
	(setq js2-mode-show-strict-warnings nil)
	(setq js2-mode-show-parse-errors nil)
	(setq js2-highlight-external-variables nil)
	(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package rjsx-mode
	:defer t)

(use-package lsp-javascript-typescript
	:hook
	(rjsx-mode . lsp-javascript-typescript-enable)
	(js-mode . lsp-javascript-typescript-enable))

(use-package json-mode
	:init
	(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(use-package protobuf-mode
	:defer t)

(use-package yaml-mode
	:init (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package omnisharp
	:after(flycheck company)
	:hook (csharp-mode . omnisharp-mode)
	:custom (omnisharp-debug t)
	:init (add-to-list 'company-backends #'company-omnisharp)
		(which-key-add-major-mode-key-based-replacements 'csharp-mode
			"<SPC> m" "dotnet"
			"<SPC> mc" "compile project"
			"<SPC> me" "solution errors"
			"<SPC> mf" "find"
			"<SPC> mfd" "definitions"
			"<SPC> mfi" "implementations"
			"<SPC> mfs" "symbols"
			"<SPC> mfu" "usages"
			"<SPC> mr" "refactor"
			"<SPC> mrr" "rename"
			"<SPC> ms" "server"
			"<SPC> msc" "check status"
			"<SPC> msi" "install"
			"<SPC> msr" "reload solution"
			"<SPC> mss" "start"
			"<SPC> msS" "stop"
			"<SPC> msv" "version info"
			"<SPC> m=" "format"
			"<SPC> m=f" "file"
			"<SPC> m=r" "region"
			)

	(evil-leader/set-key-for-mode 'csharp-mode
		"mc" 'recompile
		"me" 'omnisharp-solution-errors
		"mfd" 'omnisharp-go-to-definition
		"mfi" 'omnisharp-find-implementations
		"mfs" 'omnisharp-helm-find-symbols
		"mfu" 'omnisharp-helm-find-usages

		"mrr" 'omnisharp-rename-interactively

		"msc" 'omnisharp-check-alive-status
		"msi" 'omnisharp-install-server
		"msr" 'omnisharp-reload-solution
		"mss" 'omnisharp-start-omnisharp-server
		"msS" 'omnisharp-stop-server

		"m=f" 'omnisharp-code-format-file
		"m=r" 'omnisharp-code-format-region
		)
	)

(use-package terraform-mode)

(use-package company-terraform
	:init (company-terraform-init))

(use-package hydra)

(my/bootstrap)


(provide 'init)
;;; init.el ends here
