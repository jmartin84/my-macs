;;; init.el --- Summary
;;; Commentary:
;;; Code:
(message "****Initializing Emacs****")
;; package config
(message "--Configuring package management--")
(require 'package)
;;(setq package-enable-at-startup nil)
(package-initialize)
(setq package-check-signature nil)
(setq use-package-always-ensure t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
	  backup-by-copying t               ; don't clobber symlinks
	  version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar emacs-dir (file-truename user-emacs-directory) "Path to Emacs dir.")
(defvar modules-dir (concat emacs-dir "modules/") "Path to modules dir.")

(eval-when-compile
  (require 'use-package))

(let ((default-directory  modules-dir))
	(normal-top-level-add-subdirs-to-load-path))

;; (add-to-list 'default-frame-alist '(font . "Fira Code" ))
;; (set-face-attribute 'default t :font "Fira Code" )

(load (concat modules-dir "core"))

(use-package esup)
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
    ("<SPC> ;" "toggle comment")
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
    ("<SPC> e" "errors")
    ("<SPC> el" "list treemacs errors")
    ("<SPC> eL" "list flycheck errors")
    ("<SPC> en" "next error")
    ("<SPC> ep" "previous error")
    ("<SPC> eV" "verify setup")
    ("<SPC> g" "git")
    ("<SPC> gb" "blame")
    ("<SPC> gs" "status")
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
    ("<SPC> t" "terminal")
    ("<SPC> tc" "char mode")
    ("<SPC> th" "new term here")
    ("<SPC> tl" "line mode")
    ("<SPC> tn" "new terminal")
    ("<SPC> w" "window")
    ("<SPC> wd" "delete window")
    ("<SPC> wD" "delete other windows")
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


(use-package evil
  :ensure t
	:init
	(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
  :custom
    (evil-insert-state-cursor 'bar)
	(evil-undo-system 'undo-fu)
  :config
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
	)

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


;; evil mode config
(use-package evil-leader
	:ensure t
	:pin melpa-stable
	:after (evil)
	:init
		(global-evil-leader-mode)
		(evil-leader/set-leader "<SPC>"))
	:config
		(evil-mode 1)

(use-package evil-goggles
	:disabled t
	:ensure t
	:config (evil-goggles-mode)
	(evil-goggles-use-diff-faces))

;; which-key
(use-package which-key
  :pin melpa-stable
  :config
    ;; set menu text
    (dolist (prefix my/which-key-map-prefixes) (apply 'which-key-add-key-based-replacements prefix))
    ;; keybinds - general
    (evil-leader/set-key "<SPC>" 'helm-M-x)
    (evil-leader/set-key ";" 'comment-or-uncomment-region)

	;; keybinds - applications
	(evil-leader/set-key "ad" 'dired)

    ;; keybings - buffer
    (evil-leader/set-key "bb" 'helm-mini)
    (evil-leader/set-key "bd" 'kill-this-buffer)
    (evil-leader/set-key "bm" 'my/open-messages-buffer)
    (evil-leader/set-key "bo" 'my/kill-other-buffers)
    (evil-leader/set-key "bs" '(lambda () (interactive)(switch-to-buffer "*scratch*")))
    (evil-leader/set-key "bY" 'my/not-implemented)

    (evil-leader/set-key "el" 'lsp-treemacs-errors-list)
    (evil-leader/set-key "eL" 'flycheck-list-errors)
    (evil-leader/set-key "en" 'flycheck-next-error)
    (evil-leader/set-key "ep" 'flycheck-previous-error)
    (evil-leader/set-key "eV" 'flycheck-verify-setup)

    ;; keybinds - file
    (evil-leader/set-key "fed" 'my/find-dot-file)
    (evil-leader/set-key "fer" 'my/reload-dot-file)

	;; git
    (evil-leader/set-key "gb" 'hydra-magit-menu/body)
    (evil-leader/set-key "gs" 'magit-status)

    ;;project management
    (evil-leader/set-key "pb" 'helm-projectile-switch-to-buffer)
    (evil-leader/set-key "pf" 'helm-projectile-find-file)
    (evil-leader/set-key "pse" 'editorconfig-find-current-editorconfig)
    (evil-leader/set-key "po" 'helm-projectile-switch-project)
    (evil-leader/set-key "pt" 'treemacs-display-current-project-exclusively)

    ;; search
    (evil-leader/set-key "sA" 'helm-do-ag)
    (evil-leader/set-key "sb" 'helm-ag-buffers)
    (evil-leader/set-key "sf" 'helm-ag-this-file)
    (evil-leader/set-key "sh" 'my/ahs)
    (evil-leader/set-key "sp" 'helm-projectile-ag)

	;; terminal
    (evil-leader/set-key "tc" 'term-char-mode)
    (evil-leader/set-key "tl" 'term-line-mode)
    (evil-leader/set-key "tn" 'ansi-term)

    ;; keybinds - window
    (evil-leader/set-key "wd" 'delete-window)
    (evil-leader/set-key "wD" 'delete-other-windows)
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
    (evil-define-key 'normal neotree-mode-map (kbd ".") 'neotree-hidden-file-toggle)
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
	'(company-box-backends-colors
		 '((company-dabbrev-code . "yellow")
			  (company-keywords . "red")
			  (company-etags . "red")
			  (company-gtags . "red")
			  (company-tern . "light blue")) t)
	'(company-box-icons-elisp
		 '((fa_tag :face font-lock-function-name-face)
			  (fa_cog :face font-lock-variable-name-face)
			  (fa_cube :face font-lock-constant-face)
			  (md_color_lens :face font-lock-doc-face)) t)
	'(company-box-icons-lsp
		 '((1 . fa_text_height)
			  (2 fa_tags :face font-lock-function-name-face)
			  (3 fa_tag :face font-lock-function-name-face)
			  (4 fa_tag :face font-lock-function-name-face)
			  (5 fa_cog :foreground "#FF9800")
			  (6 fa_cog :foreground "#FF9800")
			  (7 fa_cube :foreground "#7C4DFF")
			  (8 fa_cube :foreground "#7C4DFF")
			  (9 fa_cube :foreground "#7C4DFF")
			  (10 fa_cog :foreground "#FF9800")
			  (11 . md_settings_system_daydream)
			  (12 fa_cog :foreground "#FF9800")
			  (13 md_storage :face font-lock-type-face)
			  (14 md_closed_caption :foreground "#009688")
			  (15 . md_closed_caption)
			  (16 md_color_lens :face font-lock-doc-face)
			  (17 . fa_file_text_o)
			  (18 . md_refresh)
			  (19 . fa_folder_open)
			  (20 md_closed_caption :foreground "#009688")
			  (21 fa_square :face font-lock-constant-face)
			  (22 fa_cube :face font-lock-type-face)
			  (23 . fa_calendar)
			  (24 . fa_square_o)
			  (25 . fa_arrows)) t)
 '(company-box-icons-unknown 'fa_question_circle t)
 '(company-box-icons-yasnippet 'fa_bookmark t)
 '(company-lsp-async t t)
 '(company-lsp-cache-candidates 'auto t)
 '(company-quickhelp-color-background "gray22")
	'(custom-safe-themes
		 '("f302eb9c73ead648aecdc1236952b1ceb02a3e7fcd064073fb391c840ef84bca" default))
 '(evil-insert-state-cursor 'bar t)
 '(helm-minibuffer-history-key "M-p")
 '(lsp-clients-typescript-server "typescript-language-server" t)
 '(lsp-clients-typescript-server-args '("--stdio"))
 '(lsp-prefer-flymake :none t)
 '(lsp-ui-flycheck-enable nil t)
	'(package-selected-packages
		 '(treemacs-magit treemacs-projectile treemacs-evil rustic tide js-mode web-mode lsp-treemacs gnu-elpa-keyring-update flycheck-dogma flycheck-dialyxir flycheck-credo flycheck-elixir flycheck-elixir-credo graphql-mode typescript-mode mocha mmm-mode vue-mode dap-go dap-node lsp-java dap-mode yasnippet solaire-mode lsp-clients lsp dockerfile-mode evil-magit go go-mode alchemist elixir-mode magit company-tern lsp-typescript helm-ag neotree hydra auto-highlight-symbol all-the-icons-dired "epl" "epm" company-terraform terraform-mode omnisharp omnisharp-mode yaml-mode prettier-js add-node-modules-path rjsx-mode json-mode lsp-ui lsp-javascript-typescript js2-mode company-lsp lsp-mode company-next rainbow-delimiters flycheck git-gutter+ git-gutter-fringe+ fringe-helper git-gutter editorconfig evil-anzu doom-modeline exec-path-from-shell helm-projectile restart-emacs autopair frame-local ov s projectile company-quickhelp icons-in-terminal string-trim all-the-icons company-box company company-mode jbeans jbeans-theme which-key use-package helm evil-leader)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-neotree-media-file-face ((t (:inherit doom-neotree-hidden-file-face :foreground "dark gray")))))

(load (concat modules-dir "vendor/font-lock+"))

(use-package rainbow-delimiters
	:pin melpa-stable
	:hook (prog-mode . rainbow-delimiters-mode))


(use-package json-mode
	:init
	(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(use-package yaml-mode
	:init (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package terraform-mode)

(use-package company-terraform
	:init (company-terraform-init))

(use-package hydra)

(use-package dockerfile-mode
	:init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package csv-mode
	:init
	(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode)))

(my/bootstrap)


(provide 'init)
;;; init.el ends here
