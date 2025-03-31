;;-*- mode: emacs-lisp; -*-
(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :custom
  (indent-tabs-mode nil))
