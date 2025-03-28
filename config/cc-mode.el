;;-*- mode: emacs-lisp; -*-
(use-package cc-mode
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  :custom
  (indent-tabs-mode nil))
