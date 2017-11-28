;;; utils-readonly.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defconst my:read-only-keybind-alist '(
  ("p" . previous-line)
  ("n" . next-line)
  ("f" . forward-char)
  ("b" . backward-char)
  ("a" . beginning-of-line)
  ("e" . end-of-line)

  ;; vi-like
  ("h" . (lambda ()
           (interactive)
           (if (bolp) (error "Beginning of line")
             (backward-char))))
  ("l" . (lambda ()
           (interactive)
           (if (eolp) (error "End of line")
             (forward-char))))
  ("j" . next-line)
  ("k" . previous-line)
  ("/" . isearch-forward)
  ("?" . isearch-backward)

  ;; page scroll
  ("SPC" . my:scroll-up-command)
  ("u" . my:scroll-down-command)

  ) "key binding for read-only buffer")

(defconst my:read-only-mode-blacklist '(
  magit-popup-mode
  dired-mode
  ) "The blacklist which can not be run well with my:read-only-mode.")

(defvar my:read-only-overridden-keys nil)
(make-variable-buffer-local 'my:read-only-overridden-keys)

(defface my:read-only-face
  '((t :foreground "yellow" :background "red"))
  "Used for readonly lighter"
  :group 'basic-faces)

(defvar my:read-only-mode-lighter
  (list " " (propertize "Readonly" 'face `my:read-only-face)))
(put 'my:read-only-mode-lighter 'risky-local-variable t)

;; https://www.emacswiki.org/emacs/BufferLocalKeys
(defun buffer-local-set-key (key func)
      (interactive "KSet key on this buffer: \naCommand: ")
      (let ((name (format "%s-magic" (buffer-name))))
        (eval
         `(define-minor-mode ,(intern name)
            "Automagically built minor mode to define buffer-local keys."
            :lighter ""))
        (let* ((mapname (format "%s-map" name))
               (map (intern mapname)))
          (unless (boundp (intern mapname))
            (set map (make-sparse-keymap)))
          (eval
           `(define-key ,map ,key func)))
        (funcall (intern name) t)))

(define-minor-mode my:read-only-mode
  "Control-key-less keybind for read-only buffer."
  :lighter my:read-only-mode-lighter
  :variable buffer-read-only
  (let ((overwritten-keys my:read-only-overridden-keys))
    (if buffer-read-only
        (progn (dolist (keybind my:read-only-keybind-alist)
                 (unless (and (current-local-map)
                              (lookup-key (current-local-map) (kbd (car keybind))))
                   (add-to-list 'overwritten-keys (car keybind))
                   (buffer-local-set-key (kbd (car keybind)) (cdr keybind))))
               (setq my:read-only-overridden-keys overwritten-keys))
      (dolist (key overwritten-keys)
        (buffer-local-set-key (kbd key) nil))
      (setq my:read-only-overridden-keys nil)))
  ;; if buffer name was changed while it is in read-only state, the previous
  ;; `magic' minor mode would be survived and make things be messed.
  ;; We need to clear it.
  (mapc (lambda (minor)
          (let* ((active (ignore-errors
                           (and (symbolp minor) (symbol-value minor) minor)))
                 (name (and (symbolp active) (symbol-name active)))
                 (magic (when (and (stringp name) (string-match "\\(.+\\)\-magic" name))
                          (replace-match "\\1" nil nil name))))
            (when (and (stringp magic)
                       (not (string= (buffer-name) magic)))
              (message "Deactivates old buffer-local-key mode: %S" minor)
              (apply minor '(-1)))))
        (mapcar #'car minor-mode-alist))

  (read-only-mode (if buffer-read-only 1 -1)))

;; add my:read-only-mode when the buffer-read-only is set
(add-hook 'buffer-list-update-hook
          (lambda ()
            (with-current-buffer (current-buffer)
              (when (and buffer-read-only
                         (not (member major-mode my:read-only-mode-blacklist))
                         (not (member 'my:read-only-mode minor-mode-list)))
                (my:read-only-mode 1)))))

(provide 'utils-readonly)
