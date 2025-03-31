;;; init.el

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic options
(set-language-environment "Korean")
(setq-default
  show-paren-mode t
  load-prefer-newer t
  fill-column 80
  whitespace-line-column fill-column
  whitespace-style '(face trailing lines-tail tabs tab-mark)
  )
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode t)

(setq
  truncate-partial-width-windows nil ; do not truncate
  visible-bell t ; ring a visible bell
  make-backup-files nil ; no backup files
  inhibit-startup-screen t ; no startup screen
  inhibit-startup-message t ; no startup message
  default-input-method "korean-hangul390"
  default-korean-keyboard "3" ; 3 beolsik
  ;; coding-system-for-read 'utf-8
)

(when (string-match "UTF-8" (concat (getenv "LANG")))
  (setq coding-system-for-read 'utf-8)
  (setq coding-system-for-write 'utf-8))

(ignore-errors
  (when (display-graphic-p)
    (let ((korean-font "NanumGothicCoding-10"))
      (set-face-font 'default "Lucida Console-10")
      (set-fontset-font "fontset-default" '(#x1100 . #xffdc) korean-font)
      (set-fontset-font "fontset-default" '(#xe0bc . #xf66e) korean-font))))

(ignore-errors
  ;; a kinda tricky way to suppress warning
  (let ((warning-minimum-level :emergency))
    (require 'server)
    (unless (server-running-p)
      ;; http://stackoverflow.com/q/885793/emacs-error-when-calling-server-start
      (when (and (>= emacs-major-version 23)
                 (equal window-system 'w32))
        (defun server-ensure-safe-dir (dir) "Noop" t))
      (server-start) ; start server
      (when (processp server-process)
        (process-put server-process :as (cond ((daemonp) 'daemon)
                                              ((display-graphic-p) 'gui)
                                              (t 'tty)))
        (process-put server-process :terminal (frame-terminal))
        (process-put server-process :frame (selected-frame))
        (process-put server-process :children '())))))

;; customization settings
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (insert ";;; custom.el --- local customization\n")
    (write-file custom-file t)))
(load custom-file)

;; backup, auto-save and lock file settings
(let ((backups-dir (locate-user-emacs-file ".backups"))
      (auto-saves-dir (locate-user-emacs-file ".auto-saves/"))
      (lock-files-dir (locate-user-emacs-file ".lock-files/")))
  (setq backup-directory-alist `((".*" . ,backups-dir))
        auto-save-file-name-transforms  `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        lock-file-name-transforms `((".*" ,lock-files-dir t))
        tramp-backup-directory-alist `((".*" . ,backups-dir))
        tramp-auto-save-directory auto-saves-dir)
  (dolist (dir (list backups-dir auto-saves-dir lock-files-dir))
    (if (file-exists-p dir)
        (unless (file-directory-p dir)
          (warn "backup path, %s is not a directory!" dir))
      (mkdir dir))))

(setq make-backup-files t
      backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 2)

;; load workaround
(load "~/.emacs.d/workaround.el")

;; add local package path to load path
(let ((default-directory "~/.emacs.d/plugins"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package & use-package initialization
(require 'package)
;; Add various package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
;; Initialise packages
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  ;; use-package is landed as a default package into emacs 29
  ;; In this case we don't need to add a load path
  (when (< (string-to-number emacs-version) 29)
    (add-to-list 'load-path
                 (package-desc-dir
                  (car (cdr (assq 'use-package package-alist))))))
  (require 'use-package)
  (setq use-package-always-ensure t))

(defvar my:global-key-map
  (let ((map (make-sparse-keymap)))
    map)
  "My global key map")
(define-minor-mode my:global-key-mode
  "My global key mode to keep my keybindings overrides major modes keybindings
  Key bindings:
\\{my:global-key-map}"
  t nil my:global-key-map)
(my:global-key-mode t)

(defadvice load (after my:keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my:global-key-mode))
      (let* ((mykeys (assq 'my:global-key-mode minor-mode-map-alist))
             (mod (assq-delete-all 'my:global-key-mode minor-mode-map-alist)))
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;; install & configure packages
(let* ((dir "~/.emacs.d/config")
       (files
        (when (file-directory-p dir)
          (remq nil
                (mapcar
                 (lambda (file)
                   (when
                       (and (not (file-symlink-p file))
                            (not (file-directory-p file))
                            (string-match "\\([^.]+\\).el\\'" file))
                     (load-file (concat dir "/" file))
                     (concat dir "/" file)))
                 (directory-files dir))))))
    ;; byte-compile them on quit
    (add-hook 'kill-emacs-hook
              `(lambda ()
                 (mapc
                  (lambda (filename)
                    (let ((target (concat filename "c")))
                      (unless (and (file-exists-p target)
                                   (file-newer-than-file-p target filename))
                        (byte-compile-file filename))))
                    (list ,@files)))))

;; define default minor modes
(defconst my:default-minor-mode-list
  '(display-line-numbers-mode whitespace-mode))
(defconst my:default-prog-minor-mode-list
  '(flyspell-prog-mode
    display-fill-column-indicator-mode
    goto-address-prog-mode))
(defconst my:default-text-minor-mode-list
  '(visual-line-mode flyspell-mode goto-address-mode))

(mapc (lambda (mode)
        (add-hook 'prog-mode-hook mode))
      (my:flatten `(,my:default-minor-mode-list
                    ,my:default-prog-minor-mode-list)))
(mapc (lambda (mode)
        (add-hook 'text-mode-hook mode))
      (my:flatten `(,my:default-minor-mode-list
                    ,my:default-text-minor-mode-list)))

;; disable hl-mode for following modes
(defconst my:hl-mode-exceptions '(shell-mode eshell-mode term-mode))
(mapc (lambda (mode)
        (add-hook (derived-mode-hook-name mode)
                  (lambda () (setq-local global-hl-line-mode nil))))
      my:hl-mode-exceptions)

;; (el-get-bundle  arduino-mode)
;; (el-get-bundle  apache-mode)
;; (el-get-bundle  bitbake-modes)
;; (el-get-bundle  cider)
;; (el-get-bundle  command-log-mode)
;; (el-get-bundle  company-mode)
;; (el-get-bundle  company-c-headers)
;; (el-get-bundle  company-statistics)
;; (el-get-bundle  company-web)
;; (el-get-bundle  dap-mode)
;; (el-get-bundle  deft)
;; (or (fboundp 'erc) (el-get-bundle  erc))
;; (el-get-bundle  fill-column-indicator)
;; (el-get-bundle  flyspell-popup)
;; (el-get-bundle  frame-cmds)
;; (el-get-bundle  framemove) (setq framemove-hook-into-windmove t)
;; (el-get-bundle  franca-idl)
;; (el-get-bundle  glsl-mode)
;; (el-get-bundle  gn-mode)
;; (el-get-bundle  google-c-style)
;; (el-get-bundle  google-translate)
;; (el-get-bundle  grep-a-lot)
;; (el-get-bundle  gyp-mode)
;; (el-get-bundle  helm
;;   :build
;;   `(("sed" "-i"
;;      "s/mode-line-in-non-selected-windows//g"
;;      "helm-elisp.el" "helm.el")
;;     ("make" ,(format "ASYNC_ELPA_DIR=%s"
;;                      (el-get-package-directory 'emacs-async)))))
;; (el-get-bundle  helm-ag)
;; (el-get-bundle  helm-cider)
;; (el-get-bundle  helm-cmd-t)
;; (el-get-bundle  helm-company)
;; (el-get-bundle  helm-descbinds)
;; (el-get-bundle  helm-flyspell)
;; (el-get-bundle  helm-projectile :checkout "1af5979")
;; (el-get-bundle  hydra)
;; (el-get-bundle  iedit)
;; (el-get-bundle  javadoc-lookup)
;; (el-get-bundle  json)
;; (el-get-bundle  json-mode)
;; (el-get-bundle  kotlin-mode)
;; (el-get-bundle  lsp-mode)
;; (el-get-bundle  lsp-treemacs)
;; (el-get-bundle  lsp-ui)
;; (if (version< emacs-version "24.4")
;;     (progn
;;       (el-get-bundle  magit/git-modes
;;         :description "GNU Emacs modes for various Git-related files"
;;         :type github
;;         :branch "1.0.0" ; default to master, like magit.rcp
;;         :pkgname "magit/git-modes")
;;       (el-get-bundle  magit/magit
;;         :website "https://github.com/magit/magit#readme"
;;         :description "It's Magit! An Emacs mode for Git."
;;         :type github
;;         :pkgname "magit/magit"
;;         :branch "1.4.0"
;;         :depends (cl-lib git-modes)
;;         :info "."
;;         ;; use the Makefile to produce the info manual, el-get can
;;         ;; handle compilation and autoloads on its own.
;;         :compile "magit.*\\.el\\'"
;;         :build `(("make" ,(format "EMACSBIN=%s" el-get-emacs) "docs"))
;;         :build/berkeley-unix (("gmake" ,(format "EMACSBIN=%s" el-get-emacs) "docs"))
;;         ;; assume windows lacks make and makeinfo
;;         :build/windows-nt (progn nil)
;;         :post-init (setq magit-last-seen-setup-instructions "1.4.0")))
;;   (el-get-bundle compat)
;;   (el-get-bundle magit)
;;   (el-get-bundle magit-filenotify)
;;   (el-get-bundle magit-gerrit)) (setq magit-auto-revert-mode nil)
;; (el-get-bundle  magit-gh-pulls)
;; (el-get-bundle  magit-tramp)
;; (el-get-bundle  markdown-mode)
;; (el-get-bundle! markdown-preview-mode)
;; (el-get-bundle  mmm-mode) ; bitbake mode dependency
;; (el-get-bundle  multiple-cursors)
;; (el-get-bundle  nyan-mode)
;; (if (eq system-type 'windows-nt) ; windows
;;     (el-get-bundle builtin:org-mode) ; just use builtin on windows
;;   (el-get-bundle org-mode))
;; (el-get-bundle  org-present)
;; (el-get-bundle  org-publish)
;; (el-get-bundle  org-readme)
;; (or (eq (plist-get (el-get-package-def 'org-mode) :type) 'builtin)
;;     (el-get-bundle org-reveal))
;; (el-get-bundle  plantuml-mode)
;; (el-get-bundle  popup)
;; (el-get-bundle  projectile)
;; (el-get-bundle  qmake-mode)
;; (el-get-bundle  qml-mode)
;; (el-get-bundle! redo+)
;; (el-get-bundle  rfcview)
;; (el-get-bundle  tomorrow-theme)
;; (el-get-bundle  windcycle)
;; (el-get-bundle  xcscope)
;; (el-get-bundle  yasnippet)
;; (el-get-bundle  web-mode)

;; ;; packages, depends on external binaries
;; (if (executable-find "gnuplot") ; gnuplot
;;     (el-get-bundle  gnuplot-mode :build/windows-nt (progn nil))
;;   (warn "GNUPlot is not installed"))
;; (if (or (executable-find "animate") ; image magick
;;         (executable-find "magick"))
;;     (el-get-bundle  screenshot)
;;   (warn "ImageMagick is not installed"))
;; (if (executable-find "npm") ; node-js npm
;;     (progn
;;       (el-get-bundle  company-tern)
;;       (el-get-bundle  tern)
;;       (el-get-bundle  web-beautify))
;;   (warn "npm is not being installed"))
;; (if (executable-find "pip") ; python pip
;;     (progn
;;       (el-get-bundle anaconda-mode)
;;       (el-get-bundle company-anaconda))
;;   (warn "pip is not installed"))

;; (el-get 'sync)
;; (package-initialize) ; package-initialize should be placed after el-get

;; load files in utils/
;; (let ((utils-dir "~/.emacs.d/utils"))
;;   (when (file-exists-p utils-dir)
;;     (add-to-list 'load-path utils-dir)
;;     (dolist (filename (directory-files utils-dir))
;;       (when (and (not (file-symlink-p filename))
;;                  (not (file-directory-p filename))
;;                  (string-match "\\([^.]+\\).el\\'" filename))
;;         (require (intern (match-string 1 filename)))))))

;; ;; temporary
;; (when (file-exists-p "~/.emacs.d/slack/")
;;   (add-to-list 'load-path "~/.emacs.d/slack/"))
;; ;;   (if (file-exists-p "~/.emacs.d/slack/slack.el") (require 'slack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes initialization

;; global (internal) minor modes
;; (ido-mode t) ; ido
;; (column-number-mode)
;; (show-paren-mode)
;; (global-hl-line-mode t) ; highlight current line
;; (tool-bar-mode -1)
;; (yas-global-mode t)
;; (add-hook 'after-init-hook 'global-company-mode)
;; (add-hook 'after-init-hook 'company-statistics-mode)
;; (menu-bar-mode -1)


;; ;; el-doc
;; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; ;; Minor modes to apply
;; (defvar prog-minor-mode-list nil)
;; (defvar text-minor-mode-list nil)
;; (setq prog-minor-mode-list '(linum-mode
;;                              my:whitespace-mode
;;                              my:flyspell-mode
;;                              goto-address-prog-mode
;;                              vc-mode))
;; (setq text-minor-mode-list '(linum-mode
;;                              my:whitespace-mode
;;                              my:flyspell-mode
;;                              goto-address-mode))

;; ;; enable minor modes for prog-mode(there's a case of that prog-mode is nil)
;; (mapc (lambda (mode)
;;         (if (fboundp 'prog-mode) (add-hook 'prog-mode-hook mode)
;;           (if (fboundp 'cc-mode)
;;               (add-hook 'cc-mode-hook mode)
;;             (add-hook 'c-mode-hook mode)
;;             (add-hook 'c++-mode-hook mode)
;;             (add-hook 'objc-mode-hook mode)
;;             (add-hook 'java-mode-hook mode)
;;             (add-hook 'idl-mode-hook mode))
;;           (add-hook 'emacs-lisp-mode-hook mode)
;;           (add-hook 'python-mode-hook mode)
;;           (add-hook 'ruby-mode-hook mode)
;;           (add-hook 'perl-mode-hook mode)
;;           (add-hook 'js-mode-hook mode)
;;           (add-hook 'sh-mode-hook mode))
;;         (add-hook 'qmake-mode-hook mode) ; not in prog-mode
;;         )
;;       prog-minor-mode-list)


;; ;; enable minor modes for text-mode
;; (mapc (lambda (mode)
;;         (if (functionp 'text-mode) (add-hook 'text-mode-hook mode)
;;           (add-hook 'org-mode-hook mode)
;;           (add-hook 'markdown-mode-hook mode)))
;;     text-minor-mode-list)

;; ;; disable hl-mode for shell/term/eshell
;; (add-hook 'shell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
;; (add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
;; (add-hook 'term-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

;; ;; file associations
;; (add-to-list 'auto-mode-alist '("\\.vbs\\'" . basic-mode))
;; (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; C++, rather than C
;; (add-to-list 'auto-mode-alist '("\\.\\(gp\\(i\\)?\\|plt\\)\\'" . gnuplot-mode))
;; (add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
;; (add-to-list 'auto-mode-alist '("\\.\\(html\\|php\\)?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.\\(pde\\|ino\\)$" . arduino-mode))
