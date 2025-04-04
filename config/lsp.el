;;-*- mode: emacs-lisp; -*-
;; https://www.amoradi.org/20211123173900.html
(defconst my:lsp-path (concat (file-name-directory user-init-file) "lsp/"))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (js-mode . lsp-deferred)
  (typescript-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
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
