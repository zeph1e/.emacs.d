;;-*- mode: emacs-lisp; -*-
(use-package google-c-style
  :init
  (setq-default
   c-default-style '((java-mode . "java")
		     (awk-mode . "awk")
		     (python-mode . "python")
		     (other . "google"))
   indent-tabs-mode nil ; don't insert tabs in indent
   tab-always-indent nil))
