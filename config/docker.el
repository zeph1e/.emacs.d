;; -*- mode: emacs-lisp; -*-
(use-package docker)

(use-package docker-compose-mode
  :mode
  ("\\`docker-compose.*\\.ya?ml\\'" . docker-compose-mode))
