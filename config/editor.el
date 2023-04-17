;;-*- mode: emacs-lisp; -*-
(use-package iedit
  :bind
  (("C-M-#" . iedit-mode)))

(use-package multiple-cursors
  :bind
  (("M-?" . mc/edit-lines)
   ("M-." . mc/mark-next-like-this)
   ("M-," . mc/mark-previous-like-this)
   ("M-/" . mc/mark-all-like-this)))

;; another editor key bindings
(global-set-key (kbd "<f12>") 'read-only-mode)
