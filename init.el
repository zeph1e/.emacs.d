;;; init.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package initialization

;; global (internal) minor modes
(ido-mode t) ; ido
(column-number-mode)
(show-paren-mode)
(global-hl-line-mode t) ; highlight current line
(tool-bar-mode -1)
(unless (display-graphic-p) (menu-bar-mode -1))


;; el-get initialization
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

(el-get-bundle  ascope)
(el-get-bundle  ascope-ext)
(el-get-bundle  auto-complete)
(el-get-bundle  auto-complete-emacs-lisp)
(el-get-bundle! auto-complete-c-headers)
(el-get-bundle  ac-ispell)
(el-get-bundle  basic)
(or (fboundp 'erc) (el-get-bundle  erc))
(el-get-bundle  flyspell-popup)
(el-get-bundle  franca-idl)
(el-get-bundle  gnuplot-mode :build/windows-nt (progn nil)) (or (executable-find "gnuplot") (warn "GNUPlot is not installed"))
(el-get-bundle  google-c-style)
(el-get-bundle  google-translate)
(el-get-bundle  iedit)
(el-get-bundle  iman)
(el-get-bundle  js2-mode)
(el-get-bundle  js2-highlight-vars)
(el-get-bundle  js2-refactor)
(el-get-bundle  json)
(el-get-bundle  json-mode)
(if (version< emacs-version "24.4")
    (progn
      (el-get-bundle  magit/git-modes
        :description "GNU Emacs modes for various Git-related files"
        :type github
        :branch "1.0.0" ; default to master, like magit.rcp
        :pkgname "magit/git-modes")
      (el-get-bundle  magit/magit
        :website "https://github.com/magit/magit#readme"
        :description "It's Magit! An Emacs mode for Git."
        :type github
        :pkgname "magit/magit"
        :branch "1.4.0"
        :depends (cl-lib git-modes)
        :info "."
        ;; use the Makefile to produce the info manual, el-get can
        ;; handle compilation and autoloads on its own.
        :compile "magit.*\\.el\\'"
        :build `(("make" ,(format "EMACSBIN=%s" el-get-emacs) "docs"))
        :build/berkeley-unix (("gmake" ,(format "EMACSBIN=%s" el-get-emacs) "docs"))
        ;; assume windows lacks make and makeinfo
        :build/windows-nt (progn nil)
        :post-init (setq magit-last-seen-setup-instructions "1.4.0")))
  (el-get-bundle magit)
  (el-get-bundle magit-filenotify)) (setq magit-auto-revert-mode nil)
(el-get-bundle  magit-gh-pulls)
(el-get-bundle  markdown-mode)
(el-get-bundle! markdown-preview-mode)
(el-get-bundle  multiple-cursors)
(el-get-bundle  muse)
(if (eq system-type 'windows-nt)
    (el-get-bundle builtin:org-mode) (el-get-bundle org-mode)) ; just use builtin on windows
(el-get-bundle  org-present)
(el-get-bundle  org-publish)
(el-get-bundle  org-readme)
;; (or (eq (plist-get (el-get-package-def 'org-mode) :type) 'builtin) (el-get-bundle org-reveal))
(el-get-bundle  planner)
(el-get-bundle  plantuml-mode)
(el-get-bundle  qmake-mode)
(el-get-bundle  qml-mode)
(el-get-bundle! redo+)
(el-get-bundle  remember)
(el-get-bundle  screenshot) (or (executable-find "convert") (warn "ImageMagick is not installed"))
(el-get-bundle  smex)
(el-get-bundle  windcycle)
(el-get-bundle  yasnippet) (yas-global-mode t)
(el-get-bundle  color-theme) (color-theme-initialize)
(el-get-bundle  color-theme-tomorrow)
(el-get-bundle  web-beautify)
(el-get 'sync)

;; load files in utils/
(when (file-exists-p "~/.emacs.d/utils")
  (add-to-list 'load-path "~/.emacs.d/utils")
  (dolist (filename (directory-files "~/.emacs.d/utils"))
    (when (string-match "\\([^.]+\\).el\\'" filename)
      (require (intern (match-string 1 filename))))))


;; temporary
(when (file-exists-p "~/.emacs.d/slack/")
  (add-to-list 'load-path "~/.emacs.d/slack/"))
;;   (if (file-exists-p "~/.emacs.d/slack/slack.el") (require 'slack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic options
(set-language-environment "Korean")
(setq-default
  c-default-style '((java-mode . "java") (awk-mode . "awk") (python-mode . "python")
                    (other . "linux"))
  c-basic-offset 4
  tab-width 4 ; tab width 4
  indent-tabs-mode nil ; don't insert tabs in indent
  tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)
  tab-always-indent nil
  show-paren-mode t
  my:use-theme t
)

(setq
  truncate-partial-width-windows nil ; do not wrap
  visible-bell t ; ring a visible bell
  make-backup-files nil ; no backup files
  inhibit-startup-screen t ; no startup screen
  inhibit-startup-message t ; no startup message
  linum-format "%4d\u2502"
  default-input-method "korean-hangul390"
  ;; coding-system-for-read 'utf-8
)

(when (string-match "UTF-8" (concat (getenv "LANG")))
  (setq coding-system-for-read 'utf-8)
  (setq coding-system-for-write 'utf-8))

(ignore-errors
  (let ((warning-minimum-level :emergency)) ; a kinda tricky way to suppress warning
    (require 'server)
    (if (not (server-running-p)) (server-start)) ; start server
    (if (processp server-process)
        (process-put server-process ':as (cond ((daemonp) 'daemon)
                                               ((display-graphic-p) 'gui)
                                               (t 'tty)))))
  (when (display-graphic-p)
    (let ((korean-font (if (eq system-type 'windows-nt) "맑은 고딕-10" "NanumGothicCoding-10")))
      (set-face-font 'default "Lucida Console-10")
      (set-fontset-font "fontset-default" '(#x1100 . #xffdc) korean-font)
      (set-fontset-font "fontset-default" '(#xe0bc . #xf66e) korean-font)))
  (error nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global key-bindings
(defvar my:keys-mode-keymap
  (let ((map (make-sparse-keymap)))
    ;; On windows, resolve key conflict with windows IME
    (when (eq system-type 'windows-nt)
      (define-key map (kbd "C-<kanji>") 'set-mark-command)
      (define-key map (kbd "<kana>")    'toggle-input-method)) ; windows 10, S-<space> as <kana>

    ;; forward-to-indentation
    (define-key map (kbd "M-SPC") (lambda () (interactive)(forward-to-indentation 0)))
    (define-key map (kbd "M-S-SPC") 'just-one-space) ; original function on M-SPC

    ;; scroll to buffer beginning/end
    (define-key map (kbd "C-v") 'my:scroll-up-command)
    (define-key map (kbd "M-v") 'my:scroll-down-command)

    ;; windmove
    (define-key map (kbd "S-<left>")  'windmove-left)
    (define-key map (kbd "S-<right>") 'windmove-right)
    (define-key map (kbd "<select>")  'windmove-up) ; sometime, shift-up is sent as <select> to remote
    (define-key map (kbd "S-<up>")    'windmove-up)
    (define-key map (kbd "S-<down>")  'windmove-down)

    ;; windcycle
    (define-key map (kbd "C-<up>")    'buffer-up-swap) ; Switch window key-bindings
    (define-key map (kbd "C-<down>")  'buffer-down-swap)
    (define-key map (kbd "C-<right>") 'buffer-right-swap)
    (define-key map (kbd "C-<left>")  'buffer-left-swap)

    ;; util-window
    (define-key map (kbd "C-S-<up>")    'my:buffer-up-copy)
    (define-key map (kbd "C-S-<down>")  'my:buffer-down-copy)
    (define-key map (kbd "C-S-<left>")  'my:buffer-left-copy)
    (define-key map (kbd "C-S-<right>") 'my:buffer-right-copy)

    ;; frame key bindings
    (define-key map (kbd "C-x <up>") 'my:make-new-frame)
    (define-key map (kbd "C-x <down>") 'my:delete-selected-frame)
    (define-key map (kbd "C-x <right>") 'my:switch-to-next-frame)
    (define-key map (kbd "C-x <left>") 'my:switch-to-previous-frame)

    (define-key map (kbd "C-x -")     'split-window-vertically) ; Window Split key-bindings
    (define-key map (kbd "C-x |")     'split-window-horizontally)

    (define-key map (kbd "C-x x")     'delete-window) ; Window Close key-bindings

    (define-key map (kbd "C-x C-o") 'ff-find-other-file)

    ;; revert files
    (define-key map (kbd "<f5>") 'my:revert-all-buffers)

    ;; flyspell-mode
    (define-key map (kbd "<f8>") 'my:flyspell-mode)
    (define-key map (kbd "C-<f8>") 'flyspell-buffer)

    ;; undo+
    (define-key map (kbd "C-_") 'undo)
    (define-key map (kbd "M-_") 'redo)

    ;; iedit
    (define-key map (kbd "M-#") 'iedit-mode)

    ;; multiple-cursors
    (define-key map (kbd "M-?") 'mc/edit-lines)
    (define-key map (kbd "M-.") 'mc/mark-next-like-this)
    (define-key map (kbd "M-,") 'mc/mark-previous-like-this)
    (define-key map (kbd "M-/") 'mc/mark-all-like-this)

    ;; vi-like line insertion
    (define-key map (kbd "C-o") (lambda () (interactive)(beginning-of-line)(open-line 1)))
    (define-key map (kbd "M-o") (lambda () (interactive)(end-of-line)(newline)))

    ;; smex
    (when (boundp 'smex)
      (define-key map (kbd "M-x") 'smex)
      (define-key map (kbd "M-X") 'smex-major-mode-commands))

    ;; magit
    (define-key map (kbd "C-x RET C-s") 'magit-status)
    (define-key map (kbd "C-x RET C-b") (lambda () (interactive)
                                          (or (and (boundp 'magit-blame-mode)
                                                   magit-blame-mode
                                                   (message "Use q to quit blame mode"))
                                              (and (fboundp 'magit-blame) (magit-blame nil (buffer-file-name)))
                                              (and (fboundp 'magit-blame-mode)
                                                   (magit-blame-mode))))) ;; magit 1.x compat

    map)
  "global key mode keymap")

(define-minor-mode my:keys-mode
"My global key map to prevent annoying overriding of major modes

Key bindings:
\\{my:keys-mode-keymap}"
  t nil my:keys-mode-keymap)
(my:keys-mode t)

(defadvice load (after my:keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my:keys-mode))
      (let ((mykeys (assq 'my:keys-mode minor-mode-map-alist)))
        (assq-delete-all 'my:keys-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;; ignore mouse
(dolist (k `([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             ;; [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4] ; wheel-up
             ;; [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5] ; wheel-down
             [mouse-6] [down-mouse-6] [drag-mouse-6] [double-mouse-6] [triple-mouse-6] ; wheel-tilt-left
             [mouse-7] [down-mouse-7] [drag-mouse-7] [double-mouse-7] [triple-mouse-7] ; wheel-tilt-right
             [mouse-8] [down-mouse-8] [drag-mouse-8] [double-mouse-8] [triple-mouse-8] ; back
             [mouse-9] [down-mouse-9] [drag-mouse-9] [double-mouse-9] [triple-mouse-9] ; forward
             ,(kbd "C-z")
             ))
  (global-set-key k 'ignore))
;; unbind windcycle default
(dolist (k '([M-left] [M-right] [M-up] [M-down]
             [M-S-left] [M-S-right] [M-S-up] [M-S-down]))
  (global-unset-key k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions
(defvar my:switch-frame-hook nil
  "Hook run after `switch-frame-previous' or `switch-frame-next'.")

;; revert all buffers that are visiting a file: from http://emacswiki.org/emacs/RevertBuffer
(defun my:revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defun my:scroll-up-command (&optional arg)
  (interactive "P")
  (if (eq (point)(point-max))
      (signal 'end-of-buffer '())
    (condition-case e
        (scroll-up-command arg)
      (end-of-buffer (goto-char (point-max))))))

(defun my:scroll-down-command (&optional arg)
  (interactive "P")
  (if (eq (point)(point-min))
      (signal 'beginning-of-buffer '())
    (condition-case e
        (scroll-down-command arg)
      (beginning-of-buffer (goto-char (point-min))))))

(defun my:make-new-frame ()
  (interactive)
  (and (yes-or-no-p "Create a new frame? ")
       (select-frame (make-frame))))

(defun my:delete-selected-frame ()
  (interactive)
  (and (yes-or-no-p "Delete the current frame? ")
       (delete-frame (selected-frame))))

(defun my:switch-to-next-frame ()
  (interactive)
  (select-frame (next-frame)))

(defun my:switch-to-previous-frame ()
  (interactive)
  (select-frame (previous-frame)))

;; kill heading spaces on kill-line : from http://emacswiki.org/emacs/DeletingWhitespace
(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

;; To resolve encoding conflict of shell on windows
(eval-after-load "shell"
  '(when (eq system-type 'windows-nt)
    (defadvice shell (around shell-w32-encoding (&optional buffer))
      (interactive)
      (let ((coding-system-for-read 'korean-cp949))
        ad-do-it))
      (ad-activate 'shell)))

;; compile updated init files on exit
(defconst my:byte-compile-path '( "~/.emacs.d" "~/.emacs.d/utils" ))
(defun my:byte-compile-updated (&optional user-path)
  "Compile updated init files."
  (interactive)
  (let ((user-path (and (interactive-p) (list (read-directory-name "Directory: ")))))
    (dolist (dir (or user-path my:byte-compile-path))
      (if (file-exists-p dir)
          (dolist (file (directory-files dir))
            (when (string-match "^[^\\.]+\\.el\\'" file)
              (let* ((src (concat dir "/" file))
                     (target (concat src "c")))
                (unless (and (file-exists-p target)
                             (file-newer-than-file-p target src))
                    (byte-compile-file src)))))))))
(add-hook 'kill-emacs-hook 'my:byte-compile-updated)

;; split horizontally first, from http://www.emacswiki.org/emacs/HorizontalSplitting
(defun my:split-window-prefer-horizonally (window)
  "If there's only one window (excluding any possibly active
minibuffer), then split the current window horizontally."
  (if (and (one-window-p t)
           (not (active-minibuffer-window)))
      (let ((split-height-threshold nil))
        (split-window-sensibly window))
    (split-window-sensibly window)))
(setq split-window-preferred-function 'my:split-window-prefer-horizonally)

;; My white space mode:
;;  - highlight trailing spaces
;:  - highlight tabs: http://www.emacswiki.org/emacs/ShowWhiteSpace
(defface my:tab-face '((t :background "orchid"))
  "Used for tab highlighting."
  :group 'basic-faces)
(define-minor-mode my:whitespace-mode
"Shows trailing whitespaces."
  nil nil nil
  (setq show-trailing-whitespace t)
  (font-lock-add-keywords nil '(("\t" . 'my:tab-face))))

;; check ac-ispell is available
(define-minor-mode my:ac-ispell-ac-setup
  "Check availability before ac-ispell-ac-setup call."
  nil nil nil
  (require 'ispell)
  (and (executable-find ispell-program-name) (ac-ispell-ac-setup)))

(define-minor-mode my:flyspell-mode
  "Enable flyspell-mode."
  :init-value t
  (and my:flyspell-mode (derived-mode-p 'prog-mode) (flyspell-prog-mode))
  (and (setq my:flyspell-mode (flyspell-mode (if my:flyspell-mode 1 -1)))
       (called-interactively-p) (flyspell-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes initialization

;; el-doc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Minor modes to apply
(defvar prog-minor-mode-list nil)
(defvar text-minor-mode-list nil)
(setq prog-minor-mode-list '(linum-mode my:whitespace-mode my:flyspell-mode))
(setq text-minor-mode-list '(linum-mode my:whitespace-mode my:ac-ispell-ac-setup my:flyspell-mode))

;; enable minor modes for prog-mode(there's a case of that prog-mode is nil)
(let (value)
  (dolist (mode prog-minor-mode-list value)
    (if (fboundp 'prog-mode) (add-hook 'prog-mode-hook mode)
      (if (fboundp 'cc-mode)
          (add-hook 'cc-mode-hook mode)
        (add-hook 'c-mode-hook mode)
        (add-hook 'c++-mode-hook mode)
        (add-hook 'objc-mode-hook mode)
        (add-hook 'java-mode-hook mode)
        (add-hook 'idl-mode-hook mode))
      (add-hook 'emacs-lisp-mode-hook mode)
      (add-hook 'python-mode-hook mode)
      (add-hook 'ruby-mode-hook mode)
      (add-hook 'perl-mode-hook mode)
      (add-hook 'js-mode-hook mode)
      (add-hook 'sh-mode-hook mode))
    (add-hook 'qmake-mode-hook mode) ; not in prog-mode
    (if (fboundp 'basic-mode)
        (add-hook 'basic-mode-hook mode))
))

;; enable minor modes for text-mode
(let (value)
  (dolist (mode text-minor-mode-list value)
    (if (functionp 'text-mode) (add-hook 'text-mode-hook mode)
      (add-hook 'org-mode-hook mode)
      (add-hook 'markdown-mode-hook mode)
)))

;; file associations
(add-to-list 'auto-mode-alist '("\\.vbs\\'" . basic-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; C++, rather than C
(add-to-list 'auto-mode-alist '("\\.\\(gp\\(i\\)?\\|plt\\)\\'" . gnuplot-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customized options
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
