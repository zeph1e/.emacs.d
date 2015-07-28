(defun my:erc-setup ()
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs"))
	erc-autojoin-mode t)
  (set-face-foreground 'erc-input-face "ivory"))

(if (boundp 'with-eval-after-load)
    (with-eval-after-load (my:erc-setup))
  (add-hook 'erc-mode-hook 'my:erc-setup))
