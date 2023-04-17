;;-*- mode: emacs-lisp; -*-
(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (enable-remote-dir-locals t))
