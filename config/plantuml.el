(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  :config
  (setq-default plantuml-output-type "png")
  :init
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode)))
