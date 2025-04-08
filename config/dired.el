;;-*- mode: emacs-lisp; -*-
(use-package dired
  :ensure nil
  :pin manual
  :config
  ;; http://superuser.com/q/1728902
  ;; If there's an issue in opening files with wslview, it would be from a bug
  ;; in wslu, WSL Utilities. You may update wslu by:
  ;; sudo add-apt-repository ppa:wslutilities/wslu
  ;; sudo apt update
  ;; sudo apt upgrade
  (defvar my:view-file-opener (or (executable-find "wslview")  ; for WSL
                                  (executable-find "xdg-open")
                                  (executable-find "gnome-open")
                                  (executable-find "kde-open")
                                  (executable-find "open"))
    "The executable which opens file in external viewer")

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
