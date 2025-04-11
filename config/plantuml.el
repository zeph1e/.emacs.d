;;; plantuml.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package plantuml-mode
  :ensure-system-package (plantuml . "sudo apt install -y plantuml")
  :mode "\\.uml\\'"
  :config
  (setq-default plantuml-output-type "png")
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (plantuml-indent-level 4))
