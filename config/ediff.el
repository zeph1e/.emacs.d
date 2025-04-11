;;; ediff.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package ediff
  :ensure nil
  :pin manual
  :init
  ;; ediff help functions
  (defun my:ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents
       ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents
       ediff-current-difference 'B ediff-control-buffer))))
  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
              (define-key ediff-mode-map "c" 'my:ediff-copy-both-to-C)))
  :custom
  ((ediff-window-setup-function 'ediff-setup-windows-plain)
   (ediff-split-window-function 'split-window-horizontally)))
