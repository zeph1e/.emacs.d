;;; xcscope.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.


(use-package xcscope
  :ensure-system-package (cscope . "sudo apt install -y cscope")
  :init
  (cscope-setup))
