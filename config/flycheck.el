;;; flycheck.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-keymap-prefix (kbd "C-M-!")))


(use-package flycheck-pos-tip
  :ensure t
  :commands flycheck-pos-tip-mode
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  :custom
  (flycheck-pos-tip-timeout -1))
