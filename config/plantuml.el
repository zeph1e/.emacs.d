;;-*- mode: emacs-lisp; -*-
(use-package plantuml-mode
  :mode "\\.uml\\'"
  :ensure-system-package
  (plantuml . "sudo apt install -y plantuml")
  :config
  (setq-default plantuml-output-type "png")
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (plantuml-indent-level 4))
