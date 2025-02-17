;;-*- mode: emacs-lisp; -*-
(use-package magit
  :bind
  (:map my:global-key-map
   ("C-x RET C-s" . magit)
   ("C-x RET C-h" . magit-log-head)
   ("C-x RET C-b" . magit-blame)))
