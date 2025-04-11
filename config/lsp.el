;;; lsp.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (js-mode . lsp-deferred)
  (typescript-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  (web-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :ensure-system-package
  ((tsc . "npm -g install typescript")
   (typescript-language-server . "npm -g install typescript-language-server")
   (vscode-html-language-server . "npm -g install vscode-langservers-extracted")
   (vscode-json-language-server . "npm -g install vscode-langservers-extracted")
   (vscode-css-language-server . "npm -g install vscode-langservers-extracted")
   (pylsp . "sudo apt install -y python3-pylsp"))

  :custom
  ((lsp-keymap-prefix "C-c C-l")
   ;; to get lsp-mode going with xtensa
   (lsp-clients-clangd-executable (executable-find "clangd"))
   (lsp-clients-clangd-args
    '("--query-driver=/**/bin/xtensa-esp32-elf-*"
      "--background-index"
      "--header-insertion=iwyu"
      "-j=4"))))

(use-package lsp-ui)
