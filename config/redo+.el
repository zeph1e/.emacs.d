;;-*- mode: emacs-lisp; -*-
(use-package redo+
  :ensure nil
  :bind
  (:map my:global-key-map
   ("M-_" . redo)))
