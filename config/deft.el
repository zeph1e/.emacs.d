;;-*- mode: emacs-lisp; -*-
(use-package deft
  :config
  (setq deft-default-extension "org")
  :bind
  (:map my:global-key-map
   ("M-g d" . deft)
   ("C-x RET C-d" . deft))
  :custom
  ((deft-directory "~/Documents/deft")
   (deft-recursive t)))
