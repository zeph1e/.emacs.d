(defconst my:lsp-path (concat (file-name-directory user-init-file) "lsp/"))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  ;; to get lsp-mode going with xtensa
  (setq lsp-clients-clangd-executable "clangd")
  (setq lsp_clients-clangd-args
        '("--query-driver=/**/bin/xtensa-esp32-elf-*"
          "--background-index"
          "--header-insertion=iwyu"
          "-j=4"))
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration))
(use-package lsp-ui)
