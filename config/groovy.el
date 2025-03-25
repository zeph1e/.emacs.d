;;-*- mode: emacs-lisp; -*-
(use-package groovy-mode
  :ensure-system-package
  (groovysh . "sudo install apt -y groovy")
  :commands groovy-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
  :custom
  (groovysh "groovysh"))
