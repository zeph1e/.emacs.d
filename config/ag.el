;;; ag.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package ag
  :ensure-system-package (ag . "sudo apt install -y silversearcher-ag")
  :bind
  (:map my:global-key-map
   ("C-M-R" . ag)))
