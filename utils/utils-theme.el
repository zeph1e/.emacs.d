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
    (if (and (stringp (getenv "EMACS_THEME")) ; if emacs theme is set, apply that theme to all frames
             (string-match "\\`color-theme-" (getenv "EMACS_THEME"))
             (functionp (intern (getenv "EMACS_THEME"))))
        (funcall (intern (getenv "EMACS_THEME")))
      (when (functionp my:theme-function)
        (funcall my:theme-function)
        (set-face-attribute 'mode-line nil :background "tomato" :foreground "white")))
  (color-theme-standard))
