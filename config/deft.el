;;; deft.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package deft
  :config
  (setq deft-default-extension "org")
  :bind
  (:map my:global-key-map
   ("M-g d" . deft)
   ("C-x RET C-d" . deft))
  :custom
  ((deft-directory "~/Documents/deft")
   (deft-recursive t)))
