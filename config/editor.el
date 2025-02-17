;;-*- mode: emacs-lisp; -*-
(use-package iedit
  :bind
  (:map my:global-key-map
   ("C-M-#" . iedit-mode)))

(use-package multiple-cursors
  :bind
  (:map my:global-key-map
   ("M-?" . mc/edit-lines)
   ("M-." . mc/mark-next-like-this)
   ("M-," . mc/mark-previous-like-this)
   ("M-/" . mc/mark-all-like-this)))
