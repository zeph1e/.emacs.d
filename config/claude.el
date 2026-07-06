;;; claude.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.


(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest)
  :demand t)
  ;; :custom
  ;; (monet-diff-tool monet-ediff-tool)
  ;; (monet-diff-cleanup-tool monet-ediff-cleanup-tool))

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el"
       :branch "main" :rev :newest)
  :ensure-system-package
  (claude . "npm install -g @anthropic-ai/claude-code")
  :bind-keymap
  ("C-'" . claude-code-command-map) ; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude
  ;; auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
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
  (add-hook 'claude-code-process-environment-functions
            #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode))

(use-package agent-shell
  :ensure t
  :pin melpa
  :ensure-system-package
  ;; Add agent installation configs here
  ((claude . "npm install -g @anthropic-ai/claude-code")
   (claude-agent-acp . "npm install -g @agentclientprotocol/claude-agent-acp")))
