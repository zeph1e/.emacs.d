;;-*- mode: emacs-lisp; -*-
(use-package tern
  :ensure-system-package
  ((npm . "sudo apt install -y npm")
   ("~/.local/lib/node_modules" . "npm config set prefix ~/.local")
   (tern .  "npm i -g tern")))
