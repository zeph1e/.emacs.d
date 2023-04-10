;; utils-lsp.el

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(require 'python)
(require 'kotlin-mode)

(defconst my:lsp-path (concat (file-name-directory user-init-file) "lsp/"))
;; 1. Download server.zip from https://github.com/fwcd/kotlin-language-server.
;; 2. Extract server.zip into ~/.emacs.d/lsp/
;; 3. Change directory name to 'kotlin-language-server' from 'server'.
(when (file-directory-p (concat my:lsp-path "kotlin-language-server/bin"))
  (add-to-list 'exec-path (concat my:lsp-path "kotlin-language-server/bin")))
;; 1. Download adpater.zip from https://github.com/fwcd/kotlin-debug-adapter
;; 2. Extract adapter.zip into ~/.emacs.d/lsp/
;; 3. Change directory name to 'kotlin-debug-adapter' from 'adapter'.
(when (file-directory-p (concat my:lsp-path "kotlin-debug-adapter/bin"))
  (add-to-list 'exec-path (concat my:lsp-path "kotlin-debug-adapter/bin")))

;;; add lsp to hooks
(when (executable-find "pylsp")
  (add-hook 'python-mode-hook 'lsp))
(when (executable-find "kotlin-language-server")
  (add-hook 'kotlin-mode-hook 'lsp))

(provide 'utils-lsp)
