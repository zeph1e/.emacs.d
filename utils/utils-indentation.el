;;; utils-indentation.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(require 'utils-macros)

(define-c-style "webkit" "google" 4 nil
  :innamespace 0
  :access-label -
  :case-label 0
  :member-init-intro +
  :topmost-intro 0
  :arglist-cont-nonempty +
  )

(define-c-style "default" "webkit" 4 nil
  :case-label +
  )

(setq-default
 c-default-style '((java-mode . "java") (awk-mode . "awk") (python-mode . "python")
                   (other . "default"))
 c-basic-offset 4
 tab-width 4 ; tab width 4
 indent-tabs-mode nil ; don't insert tabs in indent
 tab-always-indent nil)

(provide 'utils-indentation)
