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

(use-package simple
  :ensure nil
  :pin manual
  :config
  (defun my:forward-to-indentation ()
    "Move forward to the first nonblank characther at the current line."
    (interactive)
    (forward-to-indentation 0))
  :bind
  (:map my:global-key-map
   ("M-SPC" . my:forward-to-indentation)
   ("M-S-SPC" . just-one-space)))
