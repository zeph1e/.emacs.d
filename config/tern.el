;;-*- mode: emacs-lisp; -*-
(use-package tern
  :ensure-system-package
  ((tern .  "npm i -g tern")
   ("~/.local/lib/node_modules" . "npm config set prefix ~/.local")
   (npm . "sudo apt install -y npm")))
