;;-*- mode: emacs-lisp; -*-
(use-package ag
  :ensure-system-package (ag . "sudo apt install -y silversearcher-ag")
  :bind
  (:map my:global-key-map
   ("C-M-R" . ag)))
