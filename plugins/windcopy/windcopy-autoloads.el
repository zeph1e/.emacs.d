;;; windcopy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "windcopy" "windcopy.el" (0 0 0 0))
;;; Generated autoloads from windcopy.el

(autoload 'windcopy-default-keybindings "windcopy" "\
Set up keybindgs for `windcopy'

\(fn &rest MODIFIERS)" t nil)

(autoload 'windcopy-left "windcopy" "\
Copy buffer to right window

\(fn &optional ARG)" t nil)

(autoload 'windcopy-right "windcopy" "\
Copy buffer to right window

\(fn &optional ARG)" t nil)

(autoload 'windcopy-up "windcopy" "\
Copy buffer to right window

\(fn &optional ARG)" t nil)

(autoload 'windcopy-down "windcopy" "\
Copy buffer to right window

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "windcopy" '("windcopy--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; windcopy-autoloads.el ends here
