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
  (when (display-graphic-p)
    (setq-default nyan-wavy-trail t
                  nyan-bar-length 24)
    (nyan-mode 1)
    (nyan-start-animation)))

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

(defun my:mode-line-generate-slant-xpm (direction color width height)
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
    ;; Combine everything into a single multi-line XPM string
    (mapconcat 'identity (append header rows '("};")) "\n")))

(defun my:mode-line-tab-image (direction face)
  "Create an Emacs image object from a dynamically generated XPM slant."
  (let* ((tab-color (face-attribute face :foreground nil t))
         ;; Dynamically scale height to match the current line/font height roughly
         (height (frame-char-height))
         ;; Width controls how aggressive or shallow the slant angle is
         (width (truncate (* height 0.6))))
    (if window-system
        (create-image
         (my:mode-line-generate-slant-xpm direction tab-color width height)
         'xpm t :ascent 100)
      ;; fallback for terminal
      (propertize (if (eq direction 'left) "◥" "◤") 'face face))))

(defun my:mode-line-buffer-name-tab ()
  (let* ((tab-face (if buffer-read-only
                       'my:mode-line-readonly-tab 'my:mode-line-tab))
         (left-img  (my:mode-line-tab-image 'left tab-face))
         (right-img (my:mode-line-tab-image 'right 'my:mode-line-tab))
         (left-slant  (propertize " " 'display left-img 'face tab-face))
         (right-slant (propertize " " 'display right-img
                                  'face 'my:mode-line-tab))
         (buffer-text (propertize (format "  %s  " (buffer-name))
                                  'face 'my:mode-line-buffer-id)))
    (concat left-slant buffer-text right-slant)))

(defun my:mode-line-trait-readonly ()
  (if window-system (if buffer-read-only "🔒" "  ") ""))

(defun my:mode-line-trait-modified ()
  (if (buffer-modified-p) "*" " "))

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
  (propertize "%02l" 'face 'font-lock-type-face)
  ":"
  (propertize "%02c" 'face 'font-lock-type-face)
  " "
  ;; input method
  '(:eval (propertize (if current-input-method-title
                          current-input-method-title
                        "ENG")
                      'face '(:height 0.8)))
  " "
  ;; major mode
  (propertize "%m" 'face 'bold)
  ;; process status; eg. compilation buffer
  '("" mode-line-process)

  " "
  ;; nyan-mode!!!!!
  '(:eval (list (nyan-create)))
  " %p "        ; percent of buffer
  ;; vc-mode
  '(:eval (propertize (if vc-mode vc-mode "")
                      'face '(:foreground "sky blue" :height 0.9 :weight bold)))
  ))
