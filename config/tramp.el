;;-*- mode: emacs-lisp; -*-
(use-package tramp
  :config
  (defun my:tramp-cleanup-all-connections ()
    (interactive)
    (when (y-or-n-p "Cleanup all tramp connections? ")
      (tramp-cleanup-all-connections)
      (message "Cleaned all tramp connections up")))
  (defun my:tramp-cleanup-all-buffers ()
    (interactive)
    (when (y-or-n-p "Cleanup all tramp buffers? ")
      (tramp-cleanup-all-buffers)
      (message "Cleaned all tramp buffers up")))
  (add-to-list 'tramp-remote-path (concat (getenv "HOME") "/.local/bin"))
  :bind
  (:map my:global-key-map
   ("<f6>" . my:tramp-cleanup-all-connections)
   ("C-<f6>" . my:tramp-cleanup-all-buffers))
  :custom
  (tramp-default-method
   (ignore-errors (file-name-nondirectory (executable-find "ssh"))))
  (enable-remote-dir-locals t))
