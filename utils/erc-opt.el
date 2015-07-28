(with-eval-after-load 'erc
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs"))
        erc-autojoin-mode t)
  (set-face-foreground 'erc-input-face "ivory"))
