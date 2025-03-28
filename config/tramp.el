;;-*- mode: emacs-lisp; -*-
(use-package tramp
  :config
  (add-to-list 'tramp-remote-path (concat (getenv "HOME") "/.local/bin"))
  :custom
  (tramp-default-method
   (ignore-errors (file-name-nondirectory (executable-find "ssh"))))
  (enable-remote-dir-locals t))
