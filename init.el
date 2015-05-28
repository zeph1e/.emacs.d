;; init.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;; el-get initialization
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; required packages to me
(el-get-bundle! ascope)
(el-get-bundle! linum+)
(el-get-bundle magit) (setq magit-last-seen-setup-instructions "1.4.0")
(el-get-bundle markdown-mode)
(el-get-bundle! markdown-preview-mode)
(el-get-bundle color-theme) (color-theme-initialize) (color-theme-dark-blue2) (color-theme-arjen)


;; local packages
;;(el-get-bundle zeph1e/ascope-ext)

;; basic options
(setq make-backup-files nil) ;; no backup files

;; global keybindings
(global-set-key (kbd "C-c l") 'linum-mode) ;; line-number

(global-set-key (kbd "S-<left>")  'windmove-left) ;; wind-move keymap
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>")    'windmove-up)
(global-set-key (kbd "S-<down>")  'windmove-down)

;; global (internal) minor modes
(require 'ido) (ido-mode t) ;; ido
(column-number-mode)
(show-paren-mode)

;; other internal modes initialization
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode) ;; el-doc
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; enable linum for code-editors
(add-hook 'c-mode-hook 'linum-mode)
(add-hook 'cc-mode-hook 'linum-mode)
(add-hook 'objc-mode-hook 'linum-mode)
(add-hook 'java-mode-hook 'linum-mode)
(add-hook 'idl-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'sh-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
