(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :ensure-system-package
  (claude . "npm install -g @anthropic-ai/claude-code")
  :config
  (claude-code-ide-emacs-tools-setup)
  :bind
  (:map my:global-key-map
   ("C-c C-_" . claude-code-ide-menu) ; C-c C-/ in terminal
   ("C-c C-/" . claude-code-ide-menu)))
