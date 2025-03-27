;;-*- mode: emacs-lisp; -*-
(use-package xcscope
  :ensure-system-package
  (cscope . "sudo apt install -y cscope")
  :init
  (cscope-setup))
