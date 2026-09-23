;;; theme.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package base16-theme)              ; for reservation

(use-package color-theme-tomorrow
  :ensure nil
  :config
  (set-face-attribute 'highlight nil :background "DeepSkyBlue4")
  :init
  (unless
      ;; try theme in environment variable
      (condition-case _
          (load-theme (intern (getenv "EMACS_THEME"))) (error nil))
    (color-theme-tomorrow-night-eighties)))

(use-package nyan-mode
  :ensure t
  :config
  (defconst my:nyan-blacklisted-modes
    '(vterm-mode))

  (when window-system
    (nyan-mode 1)
    (nyan-start-animation))

    ;; Recompute per-window nyan-bar-length only when window layout
    ;; actually changes (splits/resizes/new frames), never on every
    ;; mode-line redraw.
    ;; (add-hook 'window-configuration-change-hook #'my:nyan-fit-windows)

    ;; Fit once eagerly, guarded by fboundp since `my:nyan-fit-windows'
    ;; is defined later in this file: on a cold start this is a no-op
    ;; (harmless -- window-configuration-change-hook fires soon after
    ;; anyway, during initial frame setup) and on a live `load-file'
    ;; reload during development, the previous load's definition is
    ;; still bound at this point, so it fires immediately.
    ;; (when (fboundp 'my:nyan-fit-windows)
    ;;   (my:nyan-fit-windows)))
  :custom
  ((nyan-wavy-trail t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my custom mode-line (inspired from emacs-fu)
(defface my:mode-line-buffer-id
  `((t :inherit mode-line-buffer-id :background
       ,(face-attribute 'default :background)))
  "Set buffer id background color as default background.")

(defface my:mode-line-readonly
  '((t :inherit mode-line :background "red" :foreground "white"))
  "Used for highlight readonly buffer.")

(defface my:mode-line-tab
  `((t :foreground ,(face-attribute 'default :background)))
  "Used for tab.")

(defface my:mode-line-readonly-tab
  `((t :inherit my:mode-line-readonly
       :foreground ,(face-attribute 'default :background)))
  "Used for readonly tab left slant.")

(defface my:mode-line-vc
  `((t :foreground "sky blue"
       :background ,(face-attribute 'mode-line-inactive :background)))
  "Used for vc.")

(defface my:mode-line-vc-tab
  `((t :foreground ,(face-attribute 'my:mode-line-vc :background)))
  "Used for vc tab.")

(defun my:mode-line-generate-slant-xpm
    (direction color width height &optional reverse)
  "Generate a programmatic XPM string for a slant.
DIRECTION can be 'left or 'right.  COLOR is the hex string.
WIDTH and HEIGHT determine the pixel dimensions."
  (let* ((header (list (format "/* XPM */" )
                       (format "static char * slant_%s[] = {" direction)
                       (format "\"%d %d 2 1\"," width height)
                       "\"  c None\","
                       (format "\". c %s\"," color)))
         (rows nil))
    (dotimes (y height)
      (let* ((dot-count (truncate
                           (+ 1 (* (- width 1)
                                   (/ (float y) (max 1 (- height 1)))))))
             (space-count (- width dot-count))
             (dots (make-string dot-count ?.))
             (spaces (make-string space-count ?\s)))
        (push (if (eq direction 'left)
                  (format "\"%s%s\"," spaces dots)
                (format "\"%s%s\"," dots spaces))
              rows)))
    (when reverse
      (setq rows (nreverse rows)))

    ;; Combine everything into a single multi-line XPM string
    (mapconcat 'identity (append header rows '("};")) "\n")))

(defun my:mode-line-tab-image (direction face &optional reverse)
  "Create an Emacs image object from a dynamically generated XPM slant."
  (let* ((tab-color (face-attribute face :foreground nil t))
         ;; Dynamically scale height to match the current line/font height roughly
         (height (frame-char-height))
         ;; Width controls how aggressive or shallow the slant angle is
         (width (truncate (* height 0.6))))
    (if window-system
        (create-image
         (my:mode-line-generate-slant-xpm
          direction tab-color width height reverse)
         'xpm t :ascent (if reverse 'center 100))
      ;; fallback for terminal
      (let ((fallback
             (cdr (assoc reverse
                         `((nil . ((left . ,(char-to-string #x25e5))
                                   (right . ,(char-to-string #x25e4))))
                           (t . ((left . ,(char-to-string #x25e2))
                                 (right . ,(char-to-string #x25e3)))))))))
        (propertize (cdr (assq direction fallback)) 'face face)))))

(defconst my:mode-line-buffer-name-maxlen 40)

(defun my:mode-line-adjusted-buffer-name ()
  (let* ((name (buffer-name))
         (tr-name (truncate-string-to-width
                   name my:mode-line-buffer-name-maxlen)))
    (if (string= name tr-name) name
      (concat tr-name (truncate-string-ellipsis)))))

(defun my:mode-line-buffer-name-tab ()
  (let* ((tab-face (if buffer-read-only
                       'my:mode-line-readonly-tab 'my:mode-line-tab))
         (left-img  (my:mode-line-tab-image 'left tab-face))
         (right-img (my:mode-line-tab-image 'right 'my:mode-line-tab))
         (left-slant  (propertize " " 'display left-img 'face tab-face))
         (right-slant (propertize " " 'display right-img
                                  'face 'my:mode-line-tab))
         (buffer-text (propertize (format "  %-10s  "
                                          (my:mode-line-adjusted-buffer-name))
                                  'face 'my:mode-line-buffer-id
                                  'help-echo
                                  (format
                                   (concat "Buffer name: %s\n"
                                           "mouse-1: Previous buffer\n"
                                           "mouse-3: Next buffer")
                                   (buffer-name))
                                  'mouse-face 'mode-line-highlight 'local-map
                                  '(keymap
                                    (mode-line
                                     keymap (mouse-3 . mode-line-next-buffer)
                                     (mouse-1 . mode-line-previous-buffer))))))
    (concat left-slant buffer-text right-slant)))

(defun my:mode-line-trait-readonly ()
  (if window-system (if buffer-read-only "🔒" "  ") ""))

(defun my:mode-line-trait-modified ()
  (if (buffer-modified-p) "*" " "))

(defun my:mode-line-align-right (str)
  (let ((len (string-width str)))
    (list (propertize " " 'display `(space :align-to (- right ,(- len 3))))
          str)))

(defun my:mode-line-buffer-pos ()
  (unless (and (boundp 'my:nyan-blacklisted-modes)
               (memq major-mode my:nyan-blacklisted-modes))
    (let ((nyan-bar-length (or (window-parameter (selected-window) 'my:nyan-bar-length)
                                nyan-bar-length)))
      (list (nyan-create) " %p "))))

(defun my:mode-line-vc-rev ()
  (if vc-mode
      (let* ((left-img (my:mode-line-tab-image 'left 'my:mode-line-vc-tab t))
             (left-slant (propertize " " 'display left-img
                                     'face 'my:mode-line-vc-tab))
             (vc-text (propertize (format " %s " vc-mode)
                                  'face 'my:mode-line-vc)))
        (concat left-slant vc-text))
    ""))

(defconst my:mode-line-nyan-segment '(:eval (my:mode-line-buffer-pos))
  "The nyan-bar mode-line construct, as it literally appears in `mode-line-format'.
Used by `my:nyan-fit-window' to find, via `equal', where in a buffer's
`mode-line-format' the nyan segment sits, so the segments before it
can be measured without duplicating them into a separate list.")

(defun my:nyan-fit-window (window)
  "Recompute and store a fitted `nyan-bar-length' value for WINDOW."
  (let* ((buffer (window-buffer window))
         (format (buffer-local-value 'mode-line-format buffer))
         (before-nyan (and (listp format)
                            (seq-take-while
                             (lambda (seg)
                               (not (equal seg my:mode-line-nyan-segment)))
                             format))))
    (when (and before-nyan
               (not (equal before-nyan format)) ; marker actually found
               (not (memq (buffer-local-value 'major-mode buffer)
                          my:nyan-blacklisted-modes)))
      (let* ((current-length (or (window-parameter window 'my:nyan-bar-length)
                                  nyan-bar-length))
             (before-nyan-width
              (string-width
               (format-mode-line before-nyan nil window buffer)))
             (vc-text-width
              (string-width
               (format-mode-line '(:eval (my:mode-line-vc-rev))
                                 nil window buffer)))
             (current-nyan-width
              (string-width
               (format-mode-line my:mode-line-nyan-segment nil window buffer)))
             (new-length
              (max 3
                   (+ current-length
                      (- (window-total-width window)
                         before-nyan-width
                         vc-text-width
                         current-nyan-width)))))
        (set-window-parameter window 'my:nyan-bar-length new-length)))))

(defun my:nyan-fit-windows ()
  "Refit the nyan bar length for every live window on every frame.
Intended for `window-configuration-change-hook' only -- must not run
on every redisplay."
  (when (bound-and-true-p nyan-mode)
    (walk-windows #'my:nyan-fit-window nil t)))

(setq-default
 mode-line-format
 (list
  '(:eval (propertize (concat (my:mode-line-trait-readonly)
                              " %I "
                              (my:mode-line-trait-modified)
                              " ")
                      'face (when buffer-read-only 'my:mode-line-readonly)))

  '(:eval (my:mode-line-buffer-name-tab))
  "  "
  ;; line/column
  (propertize "%05l" 'face 'font-lock-type-face)
  ":"
  (propertize "%03c" 'face 'font-lock-type-face)
  "  "
  ;; input method
  '(:eval (propertize (if current-input-method-title
                          current-input-method-title
                        "ENG")
                      'face '(:height 0.8)
                      'help-echo (format
                                  (concat "Input Method: %S\n"
                                          "mouse-1: Toggle Input Method\n")
                                  (or current-input-method
                                      "english-default"))
                      'mouse-face 'mode-line-highlight 'local-map
                                  '(keymap
                                    (mode-line
                                     keymap
                                     (mouse-1 . toggle-korean-input-method)))))
  " "
  ;; major mode
  (propertize "%m" 'face 'bold)
  ;; process status; eg. compilation buffer
  '("" mode-line-process)
  " "
  ;; nyan-mode!!!!!
  '(:eval (list (nyan-create)))
  " %p "
  ;; '(:eval (my:mode-line-buffer-pos))
  '(:eval (my:mode-line-align-right (my:mode-line-vc-rev)))
  ))
