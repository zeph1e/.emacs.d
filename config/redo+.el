;;; redo+.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package redo+
  :ensure nil
  :bind
  (:map my:global-key-map
   ("C-_" . undo)
   ("M-_" . redo)))
