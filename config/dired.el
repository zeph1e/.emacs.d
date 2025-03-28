;;-*- mode: emacs-lisp; -*-
(use-package dired
  :ensure nil
  :config
  (defvar my:view-file-opener
    (eval-when-compile
      (or (executable-find "explorer.exe") (executable-find "xdg-open"))))

  (defun my:view-file-external (file)
    (interactive (list (convert-standard-filename
                        (expand-file-name (dired-file-name-at-point)))))
    (when (file-remote-p file)
      (let ((target-file
             (make-temp-file "view-file" nil
                             (concat "-" (file-name-nondirectory file)))))
        (tramp-compat-copy-file file target-file t)
        (setq file target-file)))
    (and my:view-file-opener (call-process my:view-file-opener nil 0 nil file)))
  :bind
  (:map my:global-key-map
   ("C-x C-j" . dired-jump)
   :map dired-mode-map
   ("V" . my:view-file-external)))
