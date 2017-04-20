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

(defconst my:theme 'tomorrow-night-eighties)

(if my:can-theme
    (unless (condition-case nil
                (load-theme (intern (getenv "EMACS_THEME")) t)
              (error nil))
      (when (and (boundp 'my:theme) (symbolp my:theme))
        (load-theme my:theme t)
        (set-face-attribute 'mode-line nil
                            :background "olive drab"
                            :foreground "white"
                            :box `(:line-width 2 :color "dim gray"))
        (set-face-attribute 'mode-line-inactive nil
                            :box `(:line-width 2 :color ,(face-attribute
                                                          'mode-line-inactive
                                                          :background)))
        (set-face-attribute 'mode-line-buffer-id nil :foreground "gold")))
  (color-theme-standard))

;; my custom mode-line (inspired from emacs-fu)
(defface my:mode-line-readonly-buffer-id
  '((t :inherit mode-line-buffer-id :background "red" :foreground "yellow"))
  "Used for highlight readonly buffer")

(setq-default
 mode-line-format
 (list
  " %I "
  ;; modified
  '(:eval (propertize (if (buffer-modified-p) "* " "  ")
                      'face 'compilation-mode-line-fail))
  ;; relative position, size of file

  ;; buffer name
  '(:eval (propertize " %20b "
                      'face (if buffer-read-only 'my:mode-line-readonly-buffer-id 'mode-line-buffer-id)
                      'help-echo (buffer-file-name)))

  " "
  ;; line/column
  (propertize "%02l" 'face 'font-lock-type-face)
  ":"
  (propertize "%02c" 'face 'font-lock-type-face)

  " "
  (propertize "%m" 'face 'bold)
  '("" mode-line-process)

  " "
  '(:eval (list (nyan-create)))
  " %p "
  '(:eval (propertize (if vc-mode
                          (let ((file (buffer-file-name (current-buffer))))
                            (format "%s[%s]"
                                    vc-mode
                                    (vc-working-revision file))) "")
                      'face '(:foreground "sky blue" :weight bold)))
  ))

(when (display-graphic-p)
  (setq-default nyan-wavy-trail t)
  (setq-default nyan-bar-length 24)
  (nyan-mode 1)
  (nyan-start-animation))

(provide 'utils-theme)
