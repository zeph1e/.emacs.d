;; init.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package initializations

;; global (internal) minor modes
(require 'ido) (ido-mode t) ; ido
(column-number-mode)
(show-paren-mode)

;; el-get initialization
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes")
(el-get 'sync)

(el-get-bundle  ascope)
(el-get-bundle  ascope-ext)
(el-get-bundle! linum+)
(el-get-bundle! redo+)
(el-get-bundle  iman)
(el-get-bundle  magit) (setq magit-last-seen-setup-instructions "1.4.0")
                       (setq magit-auto-revert-mode nil)
(el-get-bundle  markdown-mode)
(el-get-bundle! markdown-preview-mode)
(el-get-bundle color-theme) (color-theme-initialize)
(el-get-bundle color-theme-tomorrow)
(if (string-match "256color" (getenv "TERM")) (color-theme-tomorrow-night-eighties))

;; load files in utils/
(add-to-list 'load-path "~/.emacs.d/utils")
(if (file-exists-p "~/.emacs.d/utils")
  (dolist (filename (directory-files "~/.emacs.d/utils"))
    (when (string-match "\\([^.]+\\).el\\'" filename)
      (load-library (match-string 1 filename)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic options
(setq make-backup-files nil) ; no backup files

;; global keybindings
(global-set-key (kbd "C-c l") 'linum-mode) ; line-number

(global-set-key (kbd "S-<left>")  'windmove-left) ; windmove keymap
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>")    'windmove-up)
(global-set-key (kbd "S-<down>")  'windmove-down)

(global-set-key (kbd "C-_") 'undo) ; undo & redo
(global-set-key (kbd "M-_") 'redo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other internal modes initialization

;; el-doc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; enable linum for code-editors
(add-hook 'c-mode-hook 'linum-mode)
(add-hook 'c++-mode-hook 'linum-mode)
(add-hook 'objc-mode-hook 'linum-mode)
(add-hook 'java-mode-hook 'linum-mode)
(add-hook 'idl-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'sh-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customized options
