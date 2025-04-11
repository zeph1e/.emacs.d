;;; python.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package python-mode
  :bind
  (:map python-mode-map
   ("C-c C-." . python-indent-shift-right)
   ("C-c C-," . python-indent-shift-left)
   ;; in terminal, C-,/C-. will not be delivered
   ("C-c ." . python-indent-shift-right)
   ("C-c ," . python-indent-shift-left))
  :custom
  ((python-indent-offset 2)
   (python-shell-interpreter (or (executable-find "python3")
                                 (executable-find "python")))))

(use-package anaconda-mode
  :hook
  ((python-mode . anaconda-mode)
   (python-mode . anaconda-eldoc-mode))
  :custom
  (anaconda-mode-installation-directory
   (concat (file-name-directory user-init-file) ".anaconda-mode")))

(use-package company-anaconda)
