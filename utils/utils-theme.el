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
                            :background "DeepSkyBlue4"
                            :foreground "white"
                            :box `(:line-width 2 :color "dim gray"))
        (set-face-attribute 'mode-line-inactive nil
                            :box `(:line-width 2 :color ,(face-attribute
                                                          'mode-line-inactive
                                                          :background)))
        (set-face-attribute 'mode-line-buffer-id nil :foreground "gold")

        (set-face-attribute 'highlight nil :background "DeepSkyBlue4")))
  (color-theme-standard))

;; my custom mode-line (inspired from emacs-fu)
(defface my:mode-line-readonly-buffer-id
  '((t :inherit mode-line-buffer-id :background "red" :foreground "yellow"))
  "Used for highlight readonly buffer")

(setq-default
 mode-line-format
 (list
  " %I " ; buffer size
  ;; modified
  '(:eval (propertize (if (buffer-modified-p) "* " "  ")
                      'face 'compilation-mode-line-fail))
  ;; relative position, size of file

  ;; buffer name
  '(:eval (propertize " %20b "
                      'face (if buffer-read-only
                                'my:mode-line-readonly-buffer-id
                              'mode-line-buffer-id)
                      'help-echo (buffer-file-name)))

  " "
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

;; header line displaying file path
;; from EmacsWiki https://www.emacswiki.org/emacs/HeaderLine
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]"))
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat (with-face sl/header
                             ;; :background "red"
                             :foreground "#8fb28f"
                             :weight 'bold
                             )))
      (concat (with-face sl/header
                         ;; :background "green"
                         ;; :foreground "black"
                         :weight 'bold
                         :foreground "#8fb28f"
                         )
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))


(defun sl/display-header ()
  (setq header-line-format
        (when (buffer-file-name)
          `("" (:eval (sl/make-header))))))



(add-hook 'buffer-list-update-hook
          'sl/display-header)


(when (display-graphic-p)
  (setq-default nyan-wavy-trail t)
  (setq-default nyan-bar-length 24)
  (nyan-mode 1)
  (nyan-start-animation))

(provide 'utils-theme)
