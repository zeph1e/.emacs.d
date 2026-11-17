;;; flycheck.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :custom
  (global-flycheck-annotate-mode t)
  (flycheck-keymap-prefix (kbd "C-M-!")))
