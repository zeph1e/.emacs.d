;;; lsp.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (defun my:lsp-rust-setup-conditional-project ()
    "Dynamically configure rust-analyzer based on whether a project root exists."
    (let ((has-project-file
           (or (locate-dominating-file default-directory "Cargo.toml")
               (locate-dominating-file default-directory "rust-project.json"))))
      (if has-project-file
          (setq-local lsp-rust-analyzer-linked-projects nil)
        (setq-local lsp-rust-analyzer-linked-projects
                    `[(:roots ["."]
                       :crates [(:root_module ,(buffer-file-name)
                                 :edition "2021"
                                 :deps []
                                 :is_workspace_member t)]
                       :sysroot_src
                       ,(concat (string-trim-right
                                 (shell-command-to-string
                                  "rustup run stable rustc --print sysroot"))
                                 "/lib/rustlib/src/rust/library"))]))))
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
  (rust-mode . lsp-deferred)
  (rust-mode . my:lsp-rust-setup-conditional-project)
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
   (lsp-completion-provider :capf)
   (lsp-auto-guess-root nil)
   (lsp-ui-sideline-show-diagnostics nil)))

(use-package lsp-ui)
