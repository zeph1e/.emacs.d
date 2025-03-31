;;-*- mode: emacs-lisp; -*-
(use-package groovy-mode
  :mode "\\.groovy\\'"
  :ensure-system-package
  (groovysh . "sudo install apt -y groovy")
  :commands groovy-mode
  :custom
  (groovysh "groovysh"))
