(use-package python
  :bind
  (:map python-mode-map
   ("C-c C-." . python-indent-shift-right)
   ("C-c C-," . python-indent-shift-left)
   ;; in terminal, C-,/C-. will not be delivered
   ("C-c ." . python-indent-shift-right)
   ("C-c ," . python-indent-shift-left))
  :custom
  ((python-indent-offset 2)
  (python-shell-interpreter "python3")))

(use-package anaconda-mode
  :hook
  ((python-mode . anaconda-mode)
   (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda)
