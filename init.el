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
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")
(el-get 'sync)

(el-get-bundle  ascope)
(el-get-bundle  ascope-ext)
(el-get-bundle  http-post-simple)
(el-get-bundle  iman)
(el-get-bundle  json)
(el-get-bundle  json-mode)
(el-get-bundle! linum+)
(el-get-bundle  magit) (setq magit-last-seen-setup-instructions "1.4.0")
                       (setq magit-auto-revert-mode nil)
(el-get-bundle  magit-gerrit)
(el-get-bundle  magit-gh-pulls)
(el-get-bundle  markdown-mode)
(el-get-bundle! markdown-preview-mode)
(unless (functionp 'org-mode) (el-get-bundle org-mode)) ; can be installed with linux-dist-package
(el-get-bundle  org-publish)
(el-get-bundle  org-readme)
(el-get-bundle! redo+)
(el-get-bundle  windcycle)
(el-get-bundle  color-theme) (color-theme-initialize)
(el-get-bundle  color-theme-tomorrow) (if (or (string-match "256color" (getenv "TERM"))
					      (display-graphic-p))
					  (if (and (stringp (getenv "EMACS_THEME"))
						   (string-match "\\`color-theme-" (getenv "EMACS_THEME"))
						   (functionp (intern (getenv "EMACS_THEME"))))
					      (funcall (intern (getenv "EMACS_THEME")))
					    (color-theme-tomorrow-night-eighties)))
(when (>= emacs-major-version 24) ;; >= 24
    (el-get-bundle  smex)
)

;; load files in utils/
(add-to-list 'load-path "~/.emacs.d/utils")
(if (file-exists-p "~/.emacs.d/utils")
  (dolist (filename (directory-files "~/.emacs.d/utils"))
    (when (string-match "\\([^.]+\\).el\\'" filename)
      (load-library (match-string 1 filename)))))


;; temporary
(when (file-exists-p "~/.emacs.d/slack/")
  (add-to-list 'load-path "~/.emacs.d/slack/")
  (if (file-exists-p "~/.emacs.d/slack/slack.el") (require 'slack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic options
(setq make-backup-files nil) ; no backup files

;; global keybindings
(defvar my-keys-mode-keymap
  (let ((map (make-sparse-keymap)))
    ;; windmove
    (define-key map (kbd "S-<left>")  'windmove-left)
    (define-key map (kbd "S-<right>") 'windmove-right)
    (define-key map (kbd "<select>")  'windmove-up) ; sometime, shift-up is sent as <select> to remote
    (define-key map (kbd "S-<up>")    'windmove-up)
    (define-key map (kbd "S-<down>")  'windmove-down)

    ;; windcycle
    (define-key map (kbd "M-<up>")    'buffer-up-swap) ; Switch window keybindings
    (define-key map (kbd "M-<down>")  'buffer-down-swap)
    (define-key map (kbd "M-<right>") 'buffer-right-swap)
    (define-key map (kbd "M-<left>")  'buffer-left-swap)

    (define-key map (kbd "M-S-<left>")  'shrink-window-horizontally) ; Window Resizing keybindings
    (define-key map (kbd "M-S-<right>") 'enlarge-window-horizontally)
    (define-key map (kbd "M-S-<down>")  'shrink-window)
    (define-key map (kbd "M-S-<up>")    'enlarge-window)

    (define-key map (kbd "C-x -")     'split-window-vertically) ; Window Split keybindings
    (define-key map (kbd "C-x |")     'split-window-horizontally)

    (define-key map (kbd "C-x x")     'delete-window) ; Window Close keybindings

    ;; frame
    (define-key map (kbd "C-<left>")  (lambda (p) (interactive "p")(select-frame (next-frame))))
    (define-key map (kbd "C-<right>") (lambda (p) (interactive "p")(select-frame (previous-frame))))
    (define-key map (kbd "C-x +")     (lambda (p) (interactive "p")
					(if (yes-or-no-p "Create a new frame? ") (make-frame))))
    ;; undo+
    (define-key map (kbd "C-_") 'undo)
    (define-key map (kbd "M-_") 'redo)

    ;; smex
    (when (functionp 'smex)
      (define-key map (kbd "M-x") 'smex)
      (define-key map (kbd "M-X") 'smex-major-mode-commands)
    ;;(define-key map (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x
    )
    map)
  "global key mode keymap")

(define-minor-mode my-keys-mode
"My global key map to prevent annoying overriding of major modes

Key bindings:
\\{my-keys-mode-keymap}"
  t nil my-keys-mode-keymap)
(my-keys-mode t)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-mode))
      (let ((mykeys (assq 'my-keys-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes initialization

;; el-doc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; simple minor modes
(define-minor-mode trailing-whitespace-mode
"Shows trailing whitespaces."
  nil nil nil
 (setq show-trailing-whitespace t))


;; Minor modes to apply
(setq prog-minor-mode-list '(linum-mode trailing-whitespace-mode))
(setq text-minor-mode-list '(linum-mode trailing-whitespace-mode))

;; enable minor modes for prog-mode(there's a case of that prog-mode is nil)
(let (value)
  (dolist (mode prog-minor-mode-list value)
    (if (functionp 'prog-mode) (add-hook 'prog-mode-hook mode)
      (progn (add-hook 'c-mode-hook mode)
	     (add-hook 'c++-mode-hook mode)
	     (add-hook 'emacs-lisp-mode-hook mode)
	     (add-hook 'python-mode-hook mode)
	     (add-hook 'ruby-mode-hook mode)
	     (add-hook 'perl-mode-hook mode)
	     (add-hook 'idl-mode-hook mode)
	     (add-hook 'java-mode-hook mode)
	     (add-hook 'js-mode-hook mode)
	     (add-hook 'sh-mode-hook mode)
))))
;; enable minor modes for text-mode
(let (value)
  (dolist (mode text-minor-mode-list value)
    (if (functionp 'text-mode) (add-hook 'text-mode-hook mode)
      (progn (add-hook 'org-mode-hook mode)
	     (add-hook 'markdown-mode-hook mode)
))))

;; enable redspace for editors
;;(add-hook 'prog-mode-hook 'redspace-mode)
;;(add-hook 'text-mode-hook 'redspace-mode)
;;(define-globalized-minor-mode global-redspace-mode redspace-mode redspace-mode)
;;(global-redspace-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customized options
