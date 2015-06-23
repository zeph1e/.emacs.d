;;; theme.el -- theme related

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(defvar my:use-theme t
  "Enable theme if it is not nil.")
(defvar my:can-theme nil
  "Auto configured variable. Check environment and make decision to use theme or not.")
(defvar my:use-theme-per-frame nil
  "Apply themes per frame.
But it's too slow, doesn't work for minibar, and conflict with show-paren-mode.")

(when (or (string-match "256color" (concat "" (getenv "TERM")))
        (display-graphic-p))
    (setq my:can-theme my:use-theme))

(defvar my:themes-for-frames '(color-theme-tomorrow-night-eighties
                               color-theme-kingsajz
                               color-theme-gray30
                               color-theme-arjen
                               color-theme-tomorrow-night-blue
                               color-theme-blue-sea
                               color-theme-oswald
                               color-theme-parus))

(defun my:apply-color-theme (&optional frame)
  "Apply color-themes for each frames."
  (setq color-theme-is-global (eq (length (frame-list)) 1))
  (if (and (stringp (getenv "EMACS_THEME")) ; if emacs theme is set, apply that theme to all frames
           (string-match "\\`color-theme-" (getenv "EMACS_THEME"))
           (functionp (intern (getenv "EMACS_THEME"))))
      (funcall (intern (getenv "EMACS_THEME")))
    (let* ((f (if (framep frame) frame (selected-frame)))
           (sf (selected-frame))
           (sthm (frame-parameter f 'selected-theme))
           (thm (if sthm sthm (car my:themes-for-frames))))
  (when (functionp thm)
        (when (framep f)
          (unless (eq f sf) (select-frame f))
          (unless sthm
            (set-frame-parameter f 'selected-theme thm)
            (setq my:themes-for-frames (cdr my:themes-for-frames)))
        (funcall thm))
        (select-frame sf)))))

(defun my:restore-color-theme (&optional frame)
  "Restore color theme fcrom the frame being deleted."
  (let* ((f (if (framep frame) frame (selected-frame)))
         (thm (frame-parameter f 'selected-theme)))
    (add-to-list 'my:themes-for-frames thm))) ; put into list head & no possible duplicate

(defun my:make-frame-function (frame)
  (if my:use-theme-per-frame (my:apply-color-theme frame)))

(defun my:delete-frame-function (frame)
  (if my:use-theme-per-frame (my:restore-color-theme frame)))


;; (defun my:set-paren-face ()
;;   (set-face-foreground 'show-paren-match "#333")
;;   (set-face-background 'show-paren-match "#FFF")
;;   (set-face-attribute 'show-paren-match-face  nil :weight 'extra-bold)
;;   (set-face-foreground 'show-paren-mismatch "#3F3")
;;   (set-face-background 'show-paren-mismatch "#F33")
;;   (set-face-attribute 'show-paren-mismatch-face  nil :weight 'extra-bold))


;; (add-hook 'after-make-frame-functions 'my:make-frame-function)
;; (add-hook 'delete-frame-functions 'my:delete-frame-function)

(if my:can-theme (my:apply-color-theme) (color-theme-standard))
