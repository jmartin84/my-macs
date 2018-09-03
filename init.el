(message "****Initializing Emacs****")
;; package config
(message "--Configuring package management--")
(require 'package)
(setq package-enable-at-startup nil)
(setq use-package-always-ensure t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; helper functions
(defun my/log-package-init (packageName) (message (format "Initializing package: %s" packageName)))

(defun my/not-implemented () (interactive) (message "**NOT YET IMPLEMENTED**"))

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

(defun my/init-exec-path () ()
    (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

;; from https://github.com/jaypei/emacs-neotree/issues/149
(defun my/open-neotree-project-root-or-current-dir ()
  "Open NeoTree using the project root, using projectile, or the
current buffer directory."
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


(defun my/bootstrap-font ()
  (add-to-list 'load-path "~/.local/share/icons-in-terminal/")
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

;;variables
(setq my/which-key-map-prefixes '(
    ("<SPC> <SPC>" "M-x")
    ("<SPC> b" "buffer")
    ("<SPC> bb" "buffer list")
    ("<SPC> bd" "kill buffer")
    ("<SPC> bm" "open message buffer")
    ("<SPC> bo" "kill other buffers")
    ("<SPC> bY" "yank whole buffer")
    ("<SPC> f" "files")
    ("<SPC> fe" "emacs")
    ("<SPC> fed" "find dotfile")
    ("<SPC> fer" "reload dotfile")
    ("<SPC> p" "project")
    ("<SPC> pb" "buffers")
    ("<SPC> pf" "special files")
    ("<SPC> pfe" "open editorconfig")
    ("<SPC> po" "open project")
    ("<SPC> pt" "project explorer")
    ("<SPC> s" "search")
    ("<SPC> sA" "search all files")
    ("<SPC> sb" "search open buffers")
    ("<SPC> sf" "search this file")
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

;; general config
(message "--Executing general config--")
(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)))
(tool-bar-mode -1)
(set-default 'truncate-lines t)
(scroll-bar-mode -1)
(my/bootstrap-font)


(use-package exec-path-from-shell
  :ensure t
  :config (my/init-exec-path))

;; theme
(use-package doom-themes
  :init
    (my/log-package-init "theme")
    (setq doom-themes-enable-bold t
	  doom-thmes-enable-italic t)
  :config
    (load-theme 'doom-city-lights t)
    (doom-themes-visual-bell-config))


;; evil mode config
(use-package evil-leader
  :ensure t
  :pin melpa-stable
  :init (my/log-package-init "evil-leader")
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :init
    (my/log-package-init "evil")
    (setq evil-insert-state-cursor 'bar)
   :config
    (evil-mode 1)
    ;;(evil-map visual "<" "<gv")
  :pin melpa-stable
  :ensure t
  :after (evil-leader))

;; nlinum
(use-package nlinum-relative
  :ensure t
  :init
    (my/log-package-init "nlinum-relative")
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  :commands (nlinum-relative-mode)
  :config (nlinum-relative-setup-evil))

;; neo-tree
(use-package neotree
  :init
    (my/log-package-init "neotree")
    (setq neo-window-width 32
	  neo-create-file-auto-open t
	  neo-banner-message "Press ? for neotree help"
	  neo-show-updir-line nil
	  neo-mode-line-type 'neotree
	  neo-smart-open t
	  neo-dont-be-alone t
	  neo-persist-show nil
	  neo-auto-indent-point t
	  neo-vc-integration t)
  :after (doom-themes projectile)
  :config
    (doom-themes-neotree-config)
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
  )

;; which-key
(use-package which-key
  :pin melpa-stable
  :init
    (my/log-package-init "which-key")
  :config
    ;; set menu text
    (dolist (prefix my/which-key-map-prefixes) (apply 'which-key-add-key-based-replacements prefix))
    ;; keybinds - general
    (evil-leader/set-key "<SPC>" 'helm-M-x)

    ;; keybings - buffer
    (evil-leader/set-key "bb" 'helm-mini)
    (evil-leader/set-key "bd" 'kill-this-buffer)
    (evil-leader/set-key "bm" 'my/open-messages-buffer)
    (evil-leader/set-key "bo" 'my/kill-other-buffers)
    (evil-leader/set-key "bY" 'my/not-implemented)


    ;; keybinds - file
    (evil-leader/set-key "fed" 'my/find-dot-file)
    (evil-leader/set-key "fer" 'my/reload-dot-file)

    ;;project management
    (evil-leader/set-key "pb" 'helm-projectile-switch-to-buffer)
    (evil-leader/set-key "pfe" 'editorconfig-find-current-editorconfig)
    (evil-leader/set-key "po" 'helm-projectile-switch-project)
    (evil-leader/set-key "pt" 'my/open-neotree-project-root-or-current-dir)

    ;; search
    (evil-leader/set-key "sA" 'helm-do-ag)
    (evil-leader/set-key "sb" 'helm-ag-buffers)
    (evil-leader/set-key "sf" 'helm-ag-this-file)
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
    (evil-leader/set-key "wS" 'split-window-horizontally)
    (evil-leader/set-key "wV" 'split-window-vertically)
    (evil-leader/set-key "qd" 'my/restart-and-debug)
    (evil-leader/set-key "qq" 'save-buffers-kill-terminal)
    (evil-leader/set-key "qr" 'restart-emacs)

    (which-key-mode))

;; helm
(use-package helm
  :pin melpa-stable
  :init
    (my/log-package-init "helm")
    (setq
     helm-mode-fuzzy-match t
     helm-completion-in-region-fuzzy-match t))

;; helm-ag
(use-package helm-ag
  :pin melpa-stable
  :requires (helm)
  :init
    (my/log-package-init "helm-ag"))

;; helm-projectile
(use-package helm-projectile
  :pin melpa-stable
  :requires (helm)
  :init
    (my/log-package-init "helm-projectile")
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (git-gutter+ git-gutter-fringe+ fringe-helper git-gutter editorconfig evil-anzu doom-modeline exec-path-from-shell helm-projectile restart-emacs autopair frame-local ov s projectile company-quickhelp icons-in-terminal string-trim all-the-icons company-box company company-mode jbeans jbeans-theme which-key use-package helm evil-leader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; all-the-icons
(use-package all-the-icons)

;; company
(use-package company
  :pin melpa
  :demand t
  :init
    (my/log-package-init "company")
    (setq company-idle-delay nil)
  :commands (company-complete global-company-mode)
  :config
    (global-company-mode)
    (evil-declare-change-repeat 'company-complete)
  :bind (("C-<SPC>" . company-complete)))

(use-package company-box
  :after (company)
  :commands (company-box-mode)
  :init
    (my/log-package-init "company-box")
    (setq company-box-icons-unknown '(all-the-icons-faicon "question-circle"))
    (setq company-box-icons-elisp
	'((all-the-icons-faicon "tag") ;; Function
	    (all-the-icons-faicon "cog") ;; Variable
	    (all-the-icons-faicon "cube") ;; Feature
	    ))

    (add-hook 'prog-mode-hook 'company-box-mode))

(use-package company-quickhelp
  :pin melpa-stable
  :after (company)
  :commands (company-quickhelp-mode)
  :init
    (my/log-package-init "company-quickhelp")
    (add-hook 'after-init-hook 'company-quickhelp-mode))

(use-package projectile
  :pin melpa-stable
  :commands (projectile-mode)
  :init
  (my/log-package-init "projectile")
  (add-hook 'after-init-hook 'projectile-mode))
;; restart emacs
(use-package restart-emacs :pin melpa-stable)

(use-package doom-modeline
    :ensure t
    :init (my/log-package-init "doom-modeline")
    :defer t
    :hook (after-init . doom-modeline-init))

(use-package evil-anzu
  :hook (after-init . global-anzu-mode)
  :init (my/log-package-init "evil-anzu"))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package fringe-helper :ensure t)

(use-package git-gutter+
	:hook (after-init . global-git-gutter+-mode)
	:init
		(my/log-package-init "git-gutter+")
	)
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
		(fringe-helper-define 'git-gutter-fr+-added '(center repeated) "....XXXX")

		(fringe-helper-define 'git-gutter-fr+-deleted '(center repeated) "....XXXX")
		(fringe-helper-define 'git-gutter-fr+-modified '(center repeated) "....XXXX"))
