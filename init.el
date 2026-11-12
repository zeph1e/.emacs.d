;;; init.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(require 'derived)
(require 'server)
(require 'tramp)
(require 'warnings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic option

;; load workaround first
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
 display-fill-column-indicator-character
 (if (fontp (char-displayable-p #x2502)) #x2502 #x7c)
 )
(tool-bar-mode -1)        ; do not shows toolbar
(menu-bar-mode -1)        ; do not shows menu
(global-hl-line-mode t)   ; highlights the current cursor line

;; Font Settings
;; Korean: https://github.com/naver/nanumfont
;; Han: Noto/Source Han (SIL OFL), WenQuanYi (GPL + font exception), all free
;;   for commercial use.
;; Japanese: https://fonts.google.com/specimen/M+PLUS+U
;; Symbol: https://github.com/dejavu-fonts/dejavu-fonts
(defvar my:font-reference-dpi 96
  "DPI that `my:font-base-size' was tuned for.")
(defvar my:font-base-size 10
  "Default font size in points at `my:font-reference-dpi'.")
(defvar my:font-preferred-families
  '("Cousine" "Lucida Console" "PTMono" "SometypeMono" "Inconsolata"
    "JetBrainsMono")
  "Candidates for the default font, in order of preference.")
(defvar my:font-override-set
  '(("NanumGothicCoding" "한" 1.67 hangul cjk-misc)
    ("Noto Sans CJK KR" "漢" 1.67 han)
    ("M PLUS U" "あ" 1.67 kana bopomofo)
    ("DejaVu Sans Mono" "a" 1 symbol))
  "Per-charset fonts: (FAMILY SAMPLE-CHAR CELLS CHARSET...).
These replace the fontset entries for CHARSET, so they win over system fonts.
Entries sharing a CHARSET are tried in order; uninstalled families are skipped.
If CELLS is a number, a SAMPLE-CHAR glyph is fitted to that many cells of the
default font.  If nil, the font gets the default font's pixel size, so CJK
scripts look as large as Latin text and as Han glyphs from system fonts.")
(defun my:frame-dpi (&optional frame)
  "Return DPI of the monitor FRAME is on, or nil if unknown."
  (let* ((attrs (frame-monitor-attributes frame))
         (mm (car (alist-get 'mm-size attrs)))
         (px (nth 2 (alist-get 'geometry attrs))))
    (when (and (numberp mm) (numberp px) (> mm 0))
      (/ px (/ mm 25.4)))))

(defun my:frame-font-scale (&optional frame)
  "Return font scale of FRAME relative to `my:font-reference-dpi'."
  (let ((dpi (or (my:frame-dpi frame) my:font-reference-dpi)))
    (/ (round (* 10 (min 3.0 (max 0.75 (/ dpi my:font-reference-dpi))))) 10.0)))

(defun my:font-glyph-width (family px sample)
  "Return pixel width of SAMPLE in FAMILY at PX pixels, or nil."
  (let ((entity (find-font (font-spec :family family :size px))))
    (when entity
      (let ((glyph (aref (font-get-glyphs (open-font entity px) 0 1 sample) 0)))
        (and glyph (aref glyph 4))))))

(defun my:font-fit-pixelsize (family sample target)
  "Return largest pixel size of FAMILY whose SAMPLE is at most TARGET px wide."
  (let* ((target (floor target))
         (px (max 6 (/ target 2))) best w)
    (while (and (<= px (* 2 target))
                (setq w (my:font-glyph-width family px sample))
                (<= w target))
      (setq best px
            px (1+ px)))
    best))

(defun my:apply-font-settings (&optional frame)
  "Size fonts of FRAME by its monitor DPI, keeping glyph cells consistent.
Sizes are set in pixels, so the result does not depend on the DPI Emacs
itself assumes.  Override fonts are sized per `my:font-override-set': fitted
to a number of default-font cells, or the same pixel size as the default
font.  Fontsets are global, so these fonts follow the most recently
updated frame."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (ignore-errors
        (let ((px (round (* my:font-base-size (/ 4.0 3)
                            (my:frame-font-scale frame))))
              (family (seq-find
                       (lambda (f) (find-font (font-spec :family f)))
                       (append my:font-preferred-families
                               (list (string-join
                                      (butlast
                                       (split-string (font-get-system-font)))
                                      " "))))))
          (when (and family (not (eq px (frame-parameter frame 'my-font-px))))
            (set-face-attribute 'default frame
                                :font (font-spec :family family :size px))
            (let ((cell (my:font-glyph-width family px "a"))
                  (seen nil))
              (dolist (f my:font-override-set)
                (let ((fpx (or (and cell (nth 2 f)
                                    (my:font-fit-pixelsize
                                     (nth 0 f) (nth 1 f) (* cell (nth 2 f))))
                               px)))
                  (dolist (c (nthcdr 3 f))
                    (set-fontset-font t c
                                      (font-spec :family (nth 0 f) :size fpx)
                                      nil
                                      (cond ((eq c 'symbol) 'prepend)
                                            ((memq c seen) 'append)))
                    (push c seen)))))
            (set-frame-parameter frame 'my-font-px px)))))))

(ignore-errors
  (setq use-default-font-for-symbols nil)
  (add-hook 'after-make-frame-functions #'my:apply-font-settings)
  (add-hook 'move-frame-functions #'my:apply-font-settings)
  (when (display-graphic-p)
    (my:apply-font-settings)
    ;; maximize frame on launch
    (add-to-list 'default-frame-alist '(fullscreen . maximized))))

;; emacs server configuration
(ignore-errors
  ;; a kinda tricky way to suppress warning
  (let ((warning-minimum-level :emergency))
    (unless (server-running-p)
      ;; http://stackoverflow.com/q/885793
      (when (and (>= emacs-major-version 23)
                 (equal window-system 'w32))
        (defun server-ensure-safe-dir (_) "Noop" t))
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

;; Defines a global key map which always overrides other keybindings
;; https://stackoverflow.com/q/683425/
(defvar my:global-key-map
  (let ((map (make-sparse-keymap)))
    map)
  "My global key map.")

(define-minor-mode my:global-key-mode
  "My global key mode to keep my keybindings overrides major modes keybindings.
Key bindings:
\\{my:global-key-map}"
  :init-value t :lighter nil :keymap my:global-key-map)
(my:global-key-mode t)

(defun my:reorder-keybindings-priority ()
  "Try to ensure that my keybindings always have priority."
  (unless (eq (car (car minor-mode-map-alist)) 'my:global-key-mode)
    (let* ((mykeys (assq 'my:global-key-mode minor-mode-map-alist)))
      (when mykeys
        (setq minor-mode-map-alist
              (cons mykeys (assq-delete-all
                            'my:global-key-mode minor-mode-map-alist)))))))
(add-hook 'after-change-major-mode-hook #'my:reorder-keybindings-priority)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package manager & package configurations bootstrap

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
  (if (fboundp 'straight-use-package)
      (straight-use-package 'use-package)
    (error "Bootstrapping straight failed")))

(require 'package)
;; Add various package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/"))
      package-install-upgrade-built-in t)
;; Initialise packages
(package-initialize)

;; use-package is built-in from Emacs 29; :vc keyword requires Emacs 30
(require 'use-package)
(setq use-package-always-ensure t)

;; Init use-package-ensure-system-package+ first
(use-package use-package-ensure-system-package+
  :vc (:url "https://github.com/zeph1e/use-package-ensure-system-package-plus.el"
       :rev :newest))

(use-package exec-path-from-shell
  :pin melpa
  :init
  (with-eval-after-load 'exec-path-from-shell
    ;; prevent to accumulate duplicated paths
    (advice-add 'exec-path-from-shell-getenvs
                :filter-return
                (lambda (pairs)
                  (mapcar (lambda (pair)
                            (let ((name (car pair)) (value (cdr pair)))
                              (if (and (stringp name) (stringp value)
                                       (string-match-p "^[A-Z_]*PATH$" name))
                                  (cons name
                                        (mapconcat #'identity
                                                   (delete-dups
                                                    (split-string value ":"))
                                                   ":"))
                                pair)))
                          pairs))))
  (when (memq window-system '(mac x ns))
    (exec-path-from-shell-initialize))
  :hook
  ;; pull updated (if any) variables into current Emacs session
  (upesp+:command-executed . (lambda (pkg res)
                               (when (eq res 'succeeded)
                                 (exec-path-from-shell-initialize)))))

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

;; install & configure packages
(let* ((dir (locate-user-emacs-file "config"))
       (files (when (file-directory-p dir)
                (remq nil (mapcar (lambda (file)
                                    (when (and (not (file-symlink-p file))
                                               (not (file-directory-p file)))
                                      file))
                                  (directory-files dir)))))
       (configs (remq nil
                      (mapcar (lambda (file)
                                (when (string-match "\\([^.]+\\).el\\'" file)
                                  (file-name-sans-extension file)))
                              files)))
       (compiled (remq nil
                       (mapcar (lambda (file)
                                 (when (string-match "\\([^.]+\\).elc\\'" file)
                                   (file-name-sans-extension file)))
                               files))))
  ;; load config files
  (mapc (lambda (file) (load-file (concat dir "/" file ".el"))) configs)

  ;; remove orphan byte-compiled right away
  (mapc (lambda (file) (delete-file (concat dir "/" file ".elc")))
        (cl-set-difference compiled configs :test 'string=))

  ;; install hook to byte-compile updated on quit
  (add-hook 'kill-emacs-hook
            `(lambda ()
               (mapc (lambda (file)
                       (let* ((source (concat ,dir "/" file ".el"))
                              (target (byte-compile-dest-file source)))
                         (when (file-newer-than-file-p source target)
                           (byte-compile-file source))))
                     (list ,@configs)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minor mode configuration

(defconst my:default-minor-mode-list
  '(display-line-numbers-mode my:whitespace-mode)
  "Minor modes to apply in both of text and prog modes.")

(defconst my:default-prog-minor-mode-list
  '(flyspell-prog-mode
    display-fill-column-indicator-mode
    goto-address-prog-mode
    indent-bars-mode)
  "Minor modes to apply in `prog-mode'.")

(defconst my:default-text-minor-mode-list
  '(visual-line-mode flyspell-mode goto-address-mode)
  "Minor modes to apply in `text-mode'.")

(defconst my:custom-prog-mode-list nil
  "Major mode work's like `prog-mode' without deriving it.")

(defconst my:custom-text-mode-list
  '(conf-mode)
  "Major mode work's like `text-mode' without deriving it.")

(mapc (lambda (mode)
        (add-hook 'prog-mode-hook mode)
        (mapc (lambda (prog)
                (add-hook (intern (concat (symbol-name prog) "-hook")) mode))
              my:custom-prog-mode-list))
      (append my:default-minor-mode-list
              my:default-prog-minor-mode-list))

(mapc (lambda (mode)
        (add-hook 'text-mode-hook mode)
        (mapc (lambda (text)
                (add-hook (intern (concat (symbol-name text) "-hook")) mode))
              my:custom-text-mode-list))
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
