;;-*- mode: emacs-lisp; -*-
(use-package magit
  :bind
  (:map my:global-key-map
   ("C-x RET C-s" . magit)
   ("C-x RET C-h" . magit-log-head)
   ("C-x RET C-b" . magit-blame)
   ("C-x RET C-f" . magit-find-file)
   ("C-x RET C-l" . magit-log-buffer-file)))

(use-package magit-gerrit
  :ensure nil
  :pin manual
  :config
  (setq-default magit-gerrit-push-to "for")
  :custom
  (magit-gerrit-known-hosts '("[a-zA-Z0-9]+.lge.com"
                              "[a-zA-Z0-9]+.lgsvl.com"))
  (magit-gerrit-popup-prefix (kbd "`")))
