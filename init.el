;;; init.el

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic option

;; load workaround
(load (locate-user-emacs-file "workaround.el"))

;; language
(set-language-environment "Korean")
(setq default-korean-keyboard "3")

;; coding system
(when (string-match "UTF-8" (concat (getenv "LANG")))
  (setq coding-system-for-read 'utf-8)
  (setq coding-system-for-write 'utf-8))

;; ui configuration
(setq-default
 show-paren-mode t        ; highlights corresponding parentheses
 load-prefer-newer t      ; loads newer file even there's byte-compiled
 fill-column 80           ; set column indicator at 80
 truncate-partial-width-windows nil ; do not truncate
 visible-bell t           ; ring a visible bell
 inhibit-startup-screen t ; no startup screen
 help-window-select t     ; always select the help window
 )
(tool-bar-mode -1)        ; do not shows toolbar
(menu-bar-mode -1)        ; do not shows menu
(global-hl-line-mode t)   ; highlights the current cursor line

(ignore-errors
  (when (display-graphic-p)
    (setq use-default-font-for-symbols nil)
    ;; Font Settings
    ;; Korean: https://github.com/naver/nanumfont
    ;; Japanese: https://fonts.google.com/specimen/M+PLUS+U
    ;; Symbol: https://github.com/dejavu-fonts/dejavu-fonts
    (let ((font-set '(("NanumGothicCoding-10" . (hangul han cjk-misc))
                      ("M PLUS U-9" . (kana bopomofo))
                      ("DejaVu Sans Mono-10" . (symbol)))))
      (set-face-font 'default "Lucida Console-10")
      (mapc (lambda (f)
              (let ((spec (car f))
                    (charsets (cdr f)))
                (mapc (lambda (c)
                        (set-fontset-font t c spec nil
                                          (when (eq c 'symbol) 'prepend)))
                      charsets)))
            font-set))
    ;; maximize frame on launch
    (add-to-list 'default-frame-alist '(fullscreen . maximized))))

;; emacs server configuration
(ignore-errors
  ;; a kinda tricky way to suppress warning
  (let ((warning-minimum-level :emergency))
    (require 'server)
    (unless (server-running-p)
      ;; http://stackoverflow.com/q/885793
      (when (and (>= emacs-major-version 23)
                 (equal window-system 'w32))
        (defun server-ensure-safe-dir (dir) "Noop" t))
      (server-start) ; start server
      (when (processp server-process)
        (process-put server-process
                     :as (cond ((daemonp) 'daemon)
                               ((display-graphic-p) 'gui)
                               (t 'tty)))))))

;; customization settings
(setq custom-file (locate-user-emacs-file "custom.el"))
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
  (mapc (lambda (dir)
          (if (file-exists-p dir)
              (unless (file-directory-p dir)
                (warn "backup path, %s is not a directory!" dir))
            (mkdir dir)))
        (list backups-dir auto-saves-dir lock-files-dir)))

(setq make-backup-files t
      backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight & package & use-package initialization
(when (< (string-to-number emacs-version) 30) ; lower than v30
  (defvar bootstrap-version)
  (setq warning-suppress-types '((straight package)))
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package & use-package initialization
(require 'package)
;; Add various package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
;; Initialise packages
(package-initialize)

;; use-package is built-in from Emacs 29; :vc keyword requires Emacs 30
(require 'use-package)
(setq use-package-always-ensure t)

;; add local package path to load path
(let ((default-directory (locate-user-emacs-file "plugins")))
  (normal-top-level-add-subdirs-to-load-path) ; add sub-directories to load-path
  (mapc (lambda (dir)
          (let ((installed-flag (concat dir "/.installed")))
            (when (and (file-directory-p dir)
                       (not (string-prefix-p "." dir)))
              (unless (file-exists-p installed-flag)
                ;; Writes an autoloads file and byte-compiles the scripts
                (mapc (lambda (file)
                        (when (and (not (file-symlink-p file))
                                   (not (file-directory-p file))
                                   (string-match "\\([^.]+\\).el\\'" file))
                          (byte-compile-file (concat dir "/" file))))
                      (directory-files dir))
                (make-directory-autoloads
                 dir (concat dir "/" (directory-file-name dir) "-autoloads.el"))
                (make-empty-file installed-flag))
              ;; load autoloads files
              (require (intern (concat (directory-file-name dir)
                                       "-autoloads"))))))
          (directory-files default-directory)))

;; Defines a global key map which always overrides other keybindings
;; https://stackoverflow.com/q/683425/
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

(defun my:load-with-keybindings-priority
    (file &optional noerror nomessage nosuffix must-suffix)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my:global-key-mode))
      (let* ((mykeys (assq 'my:global-key-mode minor-mode-map-alist))
             (mod (assq-delete-all 'my:global-key-mode minor-mode-map-alist)))
        (add-to-list 'minor-mode-map-alist mykeys))))
(advice-add 'load :after #'my:load-with-keybindings-priority)


;; install & configure packages
(let* ((dir (locate-user-emacs-file "config"))
       (files
        (when (file-directory-p dir)
          (remq nil (mapcar (lambda (file)
                              (when
                                  (and (not (file-symlink-p file))
                                       (not (file-directory-p file))
                                       (string-match "\\([^.]+\\).el\\'" file))
                                (let ((byte-compile-warnings nil))
                                  (load-file (concat dir "/" file)))
                                (concat dir "/" file)))
                            (directory-files dir))))))
    ;; byte-compile them on quit
    (add-hook 'kill-emacs-hook
              `(lambda ()
                 (mapc (lambda (filename)
                         (let ((target (concat filename "c")))
                           (unless (and (file-exists-p target)
                                        (file-newer-than-file-p target
                                                                filename))
                             (byte-compile-file filename))))
                    (list ,@files)))))

;; define default major modes
(defconst my:default-minor-mode-list
  '(display-line-numbers-mode my:whitespace-mode)
  "Minor modes to apply in both of text and prog modes.")

(defconst my:default-prog-minor-mode-list
  '(flyspell-prog-mode
    display-fill-column-indicator-mode
    goto-address-prog-mode
    indent-bars-mode)
  "Minor modes to apply in `prog-mode'")

(defconst my:default-text-minor-mode-list
  '(visual-line-mode flyspell-mode goto-address-mode)
  "Minor modes to apply in `text-mode'")

(defconst my:custom-prog-mode-hook-list nil
  "Major mode work's like prog-mode without deriving it")

(defconst my:custom-text-mode-hook-list
  '(conf-mode-hook)
  "Major mode work's like text-mode without deriving it")

(mapc (lambda (mode)
        (add-hook 'prog-mode-hook mode)
        (mapc (lambda (hook)
                (add-hook hook mode))
              my:custom-prog-mode-hook-list))
      (append my:default-minor-mode-list
              my:default-prog-minor-mode-list))

(mapc (lambda (mode)
        (add-hook 'text-mode-hook mode)
        (mapc (lambda (hook)
                (add-hook hook mode))
              my:custom-text-mode-hook-list))
      (append my:default-minor-mode-list
              my:default-text-minor-mode-list))

;; disable hl-mode for following modes
(defconst my:hl-line-mode-exceptions '(shell-mode eshell-mode term-mode))

(mapc (lambda (mode)
        (add-hook (derived-mode-hook-name mode)
                  (lambda () (setq-local global-hl-line-mode nil))))
      my:hl-line-mode-exceptions)

;; Delimit fill-column for following modes
(defconst my:fill-column-exceptions '(helm-major-mode))

(mapc (lambda (mode)
        (add-hook (derived-mode-hook-name mode)
                  (lambda () (display-fill-column-indicator-mode -1))))
      my:fill-column-exceptions)



;; test
(let ((rvdir "~/Workspace/rfcview.el"))
  (when (file-directory-p rvdir)
    (add-to-list 'load-path rvdir)
    (require 'rfcview)))
