;;; compile.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package compile
  :pin manual
  :config
  (defun my:compile ()
    (interactive)
    (if (bufferp compilation-last-buffer)
        (recompile)
      (call-interactively #'compile)))
  :bind
  (:map my:global-key-map
   ("<f7>" . my:compile)
   ("C-<f7>" . compile)))
