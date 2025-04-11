;;; cc-mode.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :custom
  (indent-tabs-mode nil))
