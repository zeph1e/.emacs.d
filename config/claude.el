(use-package inheritenv
  :straight (:type git :host github :repo "purcell/inheritenv"))

(use-package monet
  :straight (:type git :host github :repo "stevemolitor/monet")
  :demand t)

;; install claude-code.el, using :depth 1 to reduce download size:
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :ensure-system-package
  (claude . "npm install -g @anthropic-ai/claude-code")
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :hook
  (claude-code-start-hook . (lambda () (setq-local line-spacing 0.1)))
  :custom
  (claude-code-terminal-backend 'vterm)
  (claude-code-display-window-fn
   #'(lambda (buffer)
       (select-window (display-buffer buffer
                                      '((display-buffer-in-side-window)
                                        (side . right)
                                        (window-width . 82))))))
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode))
