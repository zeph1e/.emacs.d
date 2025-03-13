;;-*- mode: emacs-lisp; -*-
(use-package groovy-mode
  :ensure-system-package
  (groovysh . "sudo install apt -y groovy")
  :custom
  (groovysh "groovysh"))
