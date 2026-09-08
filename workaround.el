;;-*- mode: emacs-lisp; -*-
;; To fix issue in dictionaries-common:
;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=968955
(setq ispell-menu-map-needed t)

(defun my:normalize-nil-face-attributes (orig-fun face frame &rest args)
  "Replace :foreground nil and :background nil with 'unspecified."
  (let ((plist (copy-sequence args)))
    (let ((tail plist))
      (while tail
        (when (or (eq (car tail) :foreground)
                  (eq (car tail) :background))
          (when (null (cadr tail))
            (setcar (cdr tail) 'unspecified)))
        (setq tail (cddr tail))))
    (apply orig-fun face frame plist)))

(advice-add 'set-face-attribute :around #'my:normalize-nil-face-attributes)
