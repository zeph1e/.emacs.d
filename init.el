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
  load-prefer-newer t
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
    (unless (server-running-p) (server-start)) ; start server
    (if (processp server-process)
        (process-put server-process ':as (cond ((daemonp) 'daemon)
                                               ((display-graphic-p) 'gui)
                                               (t 'tty)))))
  (when (display-graphic-p)
    (let ((korean-font "NanumGothicCoding-10"))
      (set-face-font 'default "Lucida Console-10")
      (set-fontset-font "fontset-default" '(#x1100 . #xffdc) korean-font)
      (set-fontset-font "fontset-default" '(#xe0bc . #xf66e) korean-font))))

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
  (el-get-bundle magit-filenotify)
  (el-get-bundle magit-gerrit)) (setq magit-auto-revert-mode nil)
(el-get-bundle  magit-gh-pulls)
(el-get-bundle  markdown-mode)
(el-get-bundle! markdown-preview-mode)
(el-get-bundle  multiple-cursors)
(el-get-bundle  org-present)
(el-get-bundle  org-publish)
(el-get-bundle  org-readme)
(or (eq (plist-get (el-get-package-def 'org-mode) :type) 'builtin) (el-get-bundle org-reveal))
(el-get-bundle  plantuml-mode)
(el-get-bundle  qmake-mode)
(el-get-bundle  qml-mode)
(el-get-bundle! redo+)
(el-get-bundle  screenshot) (or (executable-find "convert") (warn "ImageMagick is not installed"))
(el-get-bundle  smex)
(el-get-bundle  windcycle)
(el-get-bundle  yasnippet) (yas-global-mode t)
(el-get-bundle  color-theme) (color-theme-initialize)
(el-get-bundle  color-theme-tomorrow)
(el-get-bundle  web-beautify)
(if (eq system-type 'windows-nt) ; windows
    (progn
      (el-get-bundle builtin:org-mode) (el-get-bundle org-mode)) ; just use builtin on windows
  (el-get-bundle  planner)  ; not-windows
  (el-get-bundle  muse)
  (el-get-bundle  remember))
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
;; modes initialization

;; el-doc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Minor modes to apply
(defvar prog-minor-mode-list nil)
(defvar text-minor-mode-list nil)
(setq prog-minor-mode-list '(linum-mode my:whitespace-mode my:ac-ispell-ac-setup my:flyspell-mode))
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
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customized options
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
