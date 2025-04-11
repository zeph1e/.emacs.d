;;; yaml.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package yaml-mode
  :mode
  ("\\.ya?ml\\'" . yaml-mode)
  :bind
  ((:map yaml-mode-map
         ("C-m" . 'newline-and-indent)))
  :custom
  (yaml-indent-offset 4))
