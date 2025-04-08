;;-*- mode: emacs-lisp; -*-
(use-package yaml-mode
  :mode
  ("\\.ya?ml\\'" . yaml-mode)
  :bind
  ((:map yaml-mode-map
         ("C-m" . 'newline-and-indent)))
  :custom
  (yaml-indent-offset 4))
