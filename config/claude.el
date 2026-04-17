(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :config
  (claude-code-ide-emacs-tools-setup)
  :bind
  (:map my:global-key-map
   ("C-c C-'" . claude-code-ide-menu)))
