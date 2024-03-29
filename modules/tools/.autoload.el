;;; .autoload.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "code-completion" "code-completion.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from code-completion.el

(autoload 'my/bootstrap--tools-completion "code-completion" nil nil nil)

;;;***

;;;### (autoloads nil "linter" "linter.el" (0 0 0 0))
;;; Generated autoloads from linter.el

(autoload 'my/bootstrap--tools-flycheck "linter" nil nil nil)

;;;***

;;;### (autoloads nil "projects" "projects.el" (0 0 0 0))
;;; Generated autoloads from projects.el

(autoload 'my/toggle-treemacs "projects" "\
Open NeoTree using the project root, using projectile, or the current buffer directory." t nil)

(autoload 'my/open-neotree-project-root-or-current-dir "projects" "\
Open NeoTree using the project root, using projectile, or the current buffer directory." t nil)

(autoload 'my/bootstrap--tools-project "projects" nil nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "projects" '("matches-a-buffer-name?")))

;;;***

;;;### (autoloads nil "search" "search.el" (0 0 0 0))
;;; Generated autoloads from search.el

(autoload 'my/ahs "search" nil t nil)

(autoload 'my/bootstrap--tools-search "search" nil nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "search" '("my/")))

;;;***

;;;### (autoloads nil "tools" "tools.el" (0 0 0 0))
;;; Generated autoloads from tools.el

(autoload 'my/bootstrap--tools "tools" nil nil nil)

;;;***

;;;### (autoloads nil "utils" "utils.el" (0 0 0 0))
;;; Generated autoloads from utils.el

(autoload 'my/bootstrap--tools-utils "utils" nil nil nil)

;;;***

;;;### (autoloads nil "vcs" "vcs.el" (0 0 0 0))
;;; Generated autoloads from vcs.el

(autoload 'my/bootstrap--tools-vcs "vcs" nil nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vcs" '("my/configure-magit-hydra")))

;;;***

(provide '.autoload)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .autoload.el ends here
