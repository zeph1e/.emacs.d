;; utils-lsp.el

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(require 'python)
(require 'kotlin-mode)

(defconst my:lsp-path (concat (file-name-directory user-init-file) "lsp/"))
(when (file-directory-p (concat my:lsp-path "kotlin-language-server/bin"))
  (add-to-list 'exec-path (concat my:lsp-path "kotlin-language-server/bin")))
(when (file-directory-p (concat my:lsp-path "kotlin-debug-adapter/bin"))
  (add-to-list 'exec-path (concat my:lsp-path "kotlin-debug-adapter/bin")))

;; add lsp to hooks
(when (executable-find "pylsp")
  (add-hook 'python-mode-hook 'lsp))
(when (executable-find "kotlin-language-server")
  (add-hook 'kotlin-mode-hook 'lsp))

(provide 'utils-lsp)
