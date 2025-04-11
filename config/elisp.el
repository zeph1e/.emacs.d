;;; elisp.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package elisp-mode
  :ensure nil
  :pin manual
  :hook
  ((emacs-lisp-mode       . turn-on-eldoc-mode)
   (lisp-interaction-mode . turn-on-eldoc-mode)))
