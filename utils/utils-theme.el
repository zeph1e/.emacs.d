;;; theme.el -- theme related

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(defvar my:use-theme t
  "Enable theme if it is not nil.")
(defvar my:can-theme nil
  "Auto configured variable. Check environment and make decision to use theme or not.")

(when (or (string-match "256color" (concat "" (getenv "TERM")))
          (display-graphic-p)
          (daemonp))
    (setq my:can-theme my:use-theme))

(defconst my:theme-function 'color-theme-tomorrow-night-eighties)

(if my:can-theme
    (let ((envtheme (getenv "EMACS_THEME")))
      (if (and (stringp envtheme) ; if emacs theme is set, apply that theme to all frames
               (string-match "\\`color-theme-" envtheme)
               (functionp (intern envtheme)))
          (funcall (intern envtheme))
        (when (functionp my:theme-function)
          (funcall my:theme-function)
          (set-face-attribute 'mode-line nil :background "olive drab" :foreground "white")
          (set-face-attribute 'mode-line-buffer-id nil :foreground "gold"))))
  (color-theme-standard))

;; (when (fboundp 'powerline-default-theme)
;;   (powerline-default-theme))

;; my custom mode-line (inspired from emacs-fu)
(defface my:mode-line-readonly-buffer-id
  '((t :inherit mode-line-buffer-id :background "red"))
  "Used for highlight readonly buffer")

(setq-default
 mode-line-format
 (list
  ;; buffer name
  mode-line-front-space
  (propertize "%Z " 'face 'mode-line-emphasis)
  '(:eval (propertize "%b "
                      'face (if buffer-read-only 'my:mode-line-readonly-buffer-id 'mode-line-buffer-id)
                      'help-echo (buffer-file-name)))

  (propertize "%02l" 'face 'font-lock-type-face)
  ":"
  (propertize "%02c" 'face 'font-lock-type-face)

  ;; relative position, size of file
  " ["
  (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
  "/"
  (propertize "%I" 'face 'font-lock-constant-face) ;; size
  "]"

  ;; the current major mode for the buffer.
  " ["
  '(:eval (propertize "%m" 'face 'font-lock-string-face))
  "]"
  ;; minor-mode-alist  ;; list of minor modes

  " "
  '(:eval (list (nyan-create)))

  ))

(when (display-graphic-p)
  (setq-default nyan-wavy-trail t)
  (setq-default nyan-bar-length 10)
  (nyan-mode 1)
  (nyan-start-animation))

(provide 'utils-theme)
