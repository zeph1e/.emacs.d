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

(when (display-graphic-p)
  (setq-default nyan-wavy-trail t)
  (setq-default nyan-bar-length 24)
  (nyan-mode 1)
  (nyan-start-animation))

(provide 'utils-theme)
