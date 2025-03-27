;;-*- mode: emacs-lisp; -*-
(use-package elisp-mode
  :ensure nil
  :pin manual
  :hook
  ((emacs-lisp-mode       . turn-on-eldoc-mode)
   (lisp-interaction-mode . turn-on-eldoc-mode)))
