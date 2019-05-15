;;; .autoload.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "core" "core.el" (0 0 0 0))
;;; Generated autoloads from core.el

(autoload 'my/bootstrap "core" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "core" '(#("my/init-exec-path" 0 7 (face font-lock-function-name-face fontified nil) 7 17 (face font-lock-function-name-face fontified nil)))))

;;;***

;;;### (autoloads nil "helpers" "helpers.el" (0 0 0 0))
;;; Generated autoloads from helpers.el

(autoload 'my/log-package-init "helpers" "\


\(fn PACKAGENAME)" nil nil)

(autoload 'my/not-implemented "helpers" "\


\(fn)" nil nil)

(autoload 'my/open-messages-buffer "helpers" "\


\(fn)" t nil)

(autoload 'my/restart-and-debug "helpers" "\


\(fn)" t nil)

;;;***

(provide '.autoload)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .autoload.el ends here
