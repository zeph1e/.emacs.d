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

  ;; The stock `nyan-create' rebuilds the whole bar on every redraw.
  (defvar my:nyan-string-cache (make-hash-table :test 'equal)
    "Bar strings keyed by (LENGTH RAINBOWS CAT-FACE ANIM-FRAME WAVY).")

  (defun my:nyan--build (len rainbows animating)
    ;; Each unit needs its own image spec: Emacs merges neighbours that share one.
    (let* ((xpm (image-type-available-p 'xpm))
           (cat (propertize (aref (nyan-catface) (nyan-catface-index))
                            'display (nyan-get-anim-frame)))
           (units nil))
      (dotimes (n rainbows)
        (push (if xpm
                  (propertize
                   "|" 'display
                   (create-image
                    nyan-rainbow-image 'xpm nil
                    :ascent (or (and nyan-wavy-trail
                                     (nyan-wavy-rainbow-ascent n))
                                (if animating 95 'center))))
                "|")
              units))
      (push cat units)
      (dotimes (_ (- len rainbows nyan-cat-size))
        (push (if xpm
                  (propertize "-" 'display
                              (create-image nyan-outerspace-image 'xpm nil
                                            :ascent (if animating 95 'center)))
                "-")
              units))
      ;; my:nyan-layout: (RAINBOWS CAT-LENGTH LEN), read by `my:nyan-scroll-click'.
      (propertize (apply #'concat (nreverse units))
                  'help-echo nyan-modeline-help-string
                  'keymap '(keymap (mode-line keymap
                                              (down-mouse-1
                                               . my:nyan-scroll-click)))
                  'my:nyan-layout (list rainbows (length cat) len))))

  (defun my:nyan-create ()
    "Cached replacement for `nyan-create'."
    (if (or (< (window-width) nyan-minimum-window-width)
            (memq major-mode my:nyan-blacklisted-modes))
        ""
      (let* ((nyan-bar-length (or (window-parameter nil 'my:nyan-bar-length)
                                  nyan-bar-length))
             (rainbows (nyan-number-of-rainbows))
             (animating (nyan--is-animating-p))
             (key (list nyan-bar-length rainbows (nyan-catface-index)
                        (and animating nyan-current-frame) nyan-wavy-trail)))
        (or (gethash key my:nyan-string-cache)
            (progn
              (when (>= (hash-table-count my:nyan-string-cache) 128)
                (clrhash my:nyan-string-cache))
              (puthash key (my:nyan--build nyan-bar-length rainbows animating)
                       my:nyan-string-cache))))))

  (defun my:nyan-scroll-click (event)
    "Scroll to the place clicked on the nyan bar."
    (interactive "e")
    (let* ((posn (event-start event))
           (hit (posn-string posn))
           (layout (and hit (get-text-property (cdr hit) 'my:nyan-layout
                                               (car hit)))))
      (when layout
        (let* ((rainbows (nth 0 layout))
               (cat-length (nth 1 layout))
               (len (nth 2 layout))
               (index (cdr hit))
               (unit (cond ((< index rainbows) index)
                           ((>= index (+ rainbows cat-length))
                            (+ rainbows nyan-cat-size
                               (- index rainbows cat-length))))))
          (when unit
            (nyan-scroll-buffer (/ (float unit) len)
                                (window-buffer (posn-window posn))))))))

  (defun my:nyan-fit-frame (frame)
    "Fit each window's nyan bar on FRAME to its mode-line."
    (when (and (bound-and-true-p nyan-mode)
               (display-graphic-p frame)
               (not (frame-parent frame)))
      (dolist (window (window-list frame 'nomini))
        (let* ((buffer (window-buffer window))
               (width (window-total-width window))
               (mode (buffer-local-value 'major-mode buffer))
               (fmt (buffer-local-value 'mode-line-format buffer))
               (tail (and (listp fmt)
                          (member '(:eval (list (nyan-create))) fmt)))
               (signature (list width buffer (buffer-name buffer) mode
                                (buffer-local-value 'vc-mode buffer))))
          (when (and tail
                     (not (memq mode my:nyan-blacklisted-modes))
                     (>= width nyan-minimum-window-width)
                     (not (equal signature
                                 (window-parameter window
                                                   'my:nyan-fit-signature))))
            (set-window-parameter window 'my:nyan-fit-signature signature)
            (let* ((other (+ (string-width
                              (format-mode-line (butlast fmt (length tail))
                                                nil window buffer))
                             (string-width
                              (format-mode-line (cdr tail) nil window buffer))))
                   ;; One nyan unit is an 8 px image.
                   (len (max nyan-cat-size
                             (floor (* (- width other 1) (frame-char-width frame))
                                    8))))
              (unless (eql len (window-parameter window 'my:nyan-bar-length))
                (set-window-parameter window 'my:nyan-bar-length len)
                (force-mode-line-update t))))))))

  (when window-system
    (advice-add 'nyan-create :override #'my:nyan-create)
    (add-hook 'window-size-change-functions #'my:nyan-fit-frame)
    (nyan-mode 1)
    (nyan-start-animation)
    (mapc #'my:nyan-fit-frame (frame-list)))
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

(defun my:mode-line-vc-rev ()
  (if vc-mode
      (let* ((left-img (my:mode-line-tab-image 'left 'my:mode-line-vc-tab t))
             (left-slant (propertize " " 'display left-img
                                     'face 'my:mode-line-vc-tab))
             (vc-text (propertize (format " %s " vc-mode)
                                  'face 'my:mode-line-vc)))
        (concat left-slant vc-text))
    ""))

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
  '(:eval (my:mode-line-align-right (my:mode-line-vc-rev)))
  ))
