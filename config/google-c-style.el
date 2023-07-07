;;-*- mode: emacs-lisp; -*-
(use-package google-c-style
  :hook
  (c-mode-common-hook . google-c-style)
  :init
  (setq-default
   c-default-style '((java-mode . "java")
		     (awk-mode . "awk")
		     (python-mode . "python"))
   indent-tabs-mode nil ; don't insert tabs in indent
   tab-always-indent nil))
