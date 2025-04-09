;;-*- mode: emacs-lisp; -*-
(use-package groovy-mode
  :mode "\\.groovy\\'"
  :ensure-system-package
  (groovysh . "sudo apt install -y groovy")
  :commands groovy-mode
  :custom
  (groovysh "groovysh"))
