;;; groovy.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package groovy-mode
  :mode "\\.groovy\\'"
  :ensure-system-package (groovysh . "sudo apt install -y groovy")
  :commands groovy-mode
  :custom
  (groovysh "groovysh"))
