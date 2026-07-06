;;; lsp.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ;; (c-mode . lsp)
  ;; (c++-mode . lsp)
  (js-mode . lsp-deferred)
  ;; Install kotlin-language-server/debug-adapter like below:
  ;; $ gh release download -R fwcd/kotlin-language-server -D /tmp -p server.zip \
  ;;   && unzip -o -d $HOME/.kotlin/ /tmp/server.zip && rm -f /tmp/server.zip
  ;; $ gh release download -R fwcd/kotlin-debug-adapter -D /tmp -p adapter.zip \
  ;;   && unzip -o -d $HOME/.kotlin/ /tmp/adapter.zip && rm -f /tmp/adapter.zip
  ;; add there bin/ directory to $PATH environment variable
  (kotlin-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  (typescript-mode . lsp-deferred)
  (web-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :ensure-system-package
  ((pylsp . "sudo apt install -y python3-pylsp")
   (tsc . "npm -g install typescript")
   (typescript-language-server . "npm -g install typescript-language-server")
   (vscode-html-language-server . "npm -g install vscode-langservers-extracted")
   (vscode-json-language-server . "npm -g install vscode-langservers-extracted")
   (vscode-css-language-server . "npm -g install vscode-langservers-extracted"))

  :custom
  ((lsp-keymap-prefix "C-c C-l")
   (lsp-completion-provider :capf)))

(use-package lsp-ui)
