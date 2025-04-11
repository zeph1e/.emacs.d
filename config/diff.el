;;; diff.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package diff
  :pin manual
  :hook
  (diff-mode . (lambda ()
                 (setq-local whitespace-style
                             '(face trailing tabs tab-mark))
                 (whitespace-mode 1))))
