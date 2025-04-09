;;; init.el

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic option

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
 )
(tool-bar-mode -1)        ; do not shows toolbar
(menu-bar-mode -1)        ; do not shows menu
(global-hl-line-mode t)   ; highlights the current cursor line

(ignore-errors
  (when (display-graphic-p)
    (let ((korean-font "NanumGothicCoding-10"))
      (set-face-font 'default "Lucida Console-10")
      ;; HANGUL CHOSUNG KIYEOK to HALFWIDTH HANGUL LETTER I
      (set-fontset-font "fontset-default" '(#x1100 . #xffdc) korean-font))
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
;; package & use-package initialization
(require 'package)
;; Add various package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
;; Initialise packages
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package is landed as a built-in package into emacs 29
;; In this case we don't need to add a load path
(when (< (string-to-number emacs-version) 29)
  (add-to-list 'load-path
               (package-desc-dir
                (car (cdr (assq 'use-package package-alist))))))
(require 'use-package)
(setq use-package-always-ensure t)

;; load workaround
(load (locate-user-emacs-file "workaround.el"))

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

;; define default minor modes
(defconst my:default-minor-mode-list
  '(display-line-numbers-mode my:whitespace-mode))

(defconst my:default-prog-minor-mode-list
  '(flyspell-prog-mode
    display-fill-column-indicator-mode
    goto-address-prog-mode))

(defconst my:default-text-minor-mode-list
  '(visual-line-mode flyspell-mode goto-address-mode))

(mapc (lambda (mode)
        (add-hook 'prog-mode-hook mode))
      (append my:default-minor-mode-list
              my:default-prog-minor-mode-list))

(mapc (lambda (mode)
        (add-hook 'text-mode-hook mode))
      (append my:default-minor-mode-list
              my:default-text-minor-mode-list))

;; disable hl-mode for following modes
(defconst my:hl-line-mode-exceptions '(shell-mode eshell-mode term-mode))

(mapc (lambda (mode)
        (add-hook (derived-mode-hook-name mode)
                  (lambda () (setq-local global-hl-line-mode nil))))
      my:hl-line-mode-exceptions)
