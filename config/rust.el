;;; rust.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package rust-mode
  :config
  (require 'cl-macs)
  (cl-flet ((which #'(lambda (bin)
                   (string-trim-right
                    (shell-command-to-string
                     (concat "rustup which " bin))))))
    (setq-default rust-rustfmt-bin (which "rustfmt")
                  rust-cargo-bin (which "cargo")))
  :ensure-system-package
  ((rustup . "sudo apt install -y rustup")
   (rustc . "rustup default --stable")
   (rust-analyzer . "rustup component add rust-analyzer rust-src"))
  :custom
  (lsp-rust-analyzer-diagnostics-enable-experimental t)
  :after (lsp))
