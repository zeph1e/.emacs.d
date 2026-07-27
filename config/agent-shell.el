;;; agent-shell.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package agent-shell
  :ensure t
  :pin melpa
  :ensure-system-package
  ;; Add agent installation configs here
  ((claude . "npm install -g @anthropic-ai/claude-code")
   (codex-acp . "npm install -g @agentclientprotocol/codex-acp")
   (gemini . "npm install -g @google/gemini-cli")
   (claude-agent-acp . "npm install -g @agentclientprotocol/claude-agent-acp"))
  :bind
  (:map my:global-key-map
   ("C-\"" . agent-shell)))

(use-package agent-shell-tramp
  :vc (:url "https://github.com/junyi-hou/agent-shell-tramp" :rev :newest)
  :after agent-shell
  :config
  (agent-shell-tramp-mode 1))
