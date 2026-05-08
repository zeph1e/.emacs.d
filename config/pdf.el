;;; pdf.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package pdf-tools
  :pin melpa
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :ensure-system-package
  (epdfinfo . "sudo apt install -y elpa-pdf-tools-server")
  :custom
  (pdf-info-epdfinfo-program `,(executable-find "epdfinfo")))
