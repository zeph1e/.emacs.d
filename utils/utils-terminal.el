;;; utils-terminal.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;; To make colors in term mode derive emacs' ansi color map
(eval-after-load 'term
  '(let ((term-face-vector [term-color-black
                            term-color-red
                            term-color-green
                            term-color-yellow
                            term-color-blue
                            term-color-magenta
                            term-color-cyan
                            term-color-white]))
     (require 'ansi-color)
     (dotimes (index (length term-face-vector))
       (let ((fg (cdr (aref ansi-color-map (+ index 30))))
             (bg (cdr (aref ansi-color-map (+ index 40)))))
         (set-face-attribute (aref term-face-vector index) nil
                             :foreground fg
                             :background bg)))))

;; to notify current major mode is switched
(defun my:term-refresh-buffer-name (&optional buffer)
  (let* ((buf (or buffer (current-buffer)))
         (bufname (buffer-name buf))
         (matched (or (when (string-match
                             "\\*\\(term\\|terminal\\|ansi-term\\)\\(-shell\\)?\\*\\(<.+>\\)?\\(.+\\)?"
                             bufname)
                         (split-string (replace-match "\\1 \\3" t nil bufname)))
                      (error "Unable to get original mode!")))
         (modename (car matched))
         (ident (cadr matched))
         (toggled (not (equal 'term-mode major-mode))))
    (rename-buffer (concat "*" modename (and toggled "-shell") "*"
                           ident
                           "[ " (abbreviate-file-name default-directory) " ]"))))

;; excerpted from http://www.emacswiki.org/emacs/ShellMode
(defun my:term-switch-to-shell-mode ()
  (interactive)
  (cond ((equal major-mode 'term-mode)
         (require 'shell)
         (shell-mode)
         (set-process-filter (get-buffer-process (current-buffer))
                             'comint-output-filter)
         (local-set-key (kbd "C-j") 'my:term-switch-to-shell-mode)
         (compilation-shell-minor-mode 1)
         (comint-send-input))
        ((equal major-mode 'shell-mode)
         (require 'term)
         (compilation-shell-minor-mode -1)
         (font-lock-mode -1)
         (term-mode)
         (term-char-mode)
         (set-process-filter (get-buffer-process (current-buffer))
                             'term-emulate-terminal)
         (term-send-raw-string (kbd "C-l"))
         (define-key term-raw-map (kbd "C-j") 'my:term-switch-to-shell-mode)))
  (my:term-refresh-buffer-name))

;; make original "term" to be "terminal"
(unless (fboundp 'terminal)
  (setq original-term (symbol-function 'term))
  (defun terminal (program)
    (interactive (list (read-from-minibuffer "Run program: "
                                             (or explicit-shell-file-name
                                                 (getenv "ESHELL")
                                                 (getenv "SHELL")
                                                 "/bin/sh"))))
    (funcall (symbol-function 'original-term) program)))

;; make "term" to be "ansi-term"
(defadvice term (around my:term-adviced (&optional program new-buffer-name directory))
  (interactive)
  (funcall 'ansi-term (or program "/bin/bash") (or new-buffer-name "term")))
(ad-activate 'term)

(defvar my:ansi-term-list nil
  "Multiple term mode list to iterate between.")

;; make ansi-term be a kinda multi-term
(defadvice ansi-term (around my:ansi-term-adviced (program &optional new-buffer-name))
  (interactive (list (read-from-minibuffer "Run program: "
                       (or explicit-shell-file-name
                           (getenv "ESHELL")
                           (getenv "SHELL")
                           "/bin/sh"))))
  ad-do-it
  (push (current-buffer) my:ansi-term-list)
  (define-key term-raw-map (kbd "M-<left>") 'my:select-prev-ansi-term)
  (define-key term-raw-map (kbd "M-<right>") 'my:select-next-ansi-term))
(ad-activate 'ansi-term)

;; update ansi-term-list on kill-buffer
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (or (equal major-mode 'term-mode)
                      (equal major-mode 'shell-mode))
              (setq my:ansi-term-list (remove (current-buffer) my:ansi-term-list)))))

(defun my:select-prev-ansi-term ()
  (interactive)
  (let* ((pos (1+ (cl-position (current-buffer) my:ansi-term-list))) ; list is reversed
         (buffer (nth (% pos (length my:ansi-term-list)) my:ansi-term-list)))
    (and (equal (switch-to-buffer  buffer) (current-buffer))
         (message "Switching to %S" (buffer-name buffer)))))

(defun my:select-next-ansi-term ()
  (interactive)
  (let* ((pos (1- (+ (length my:ansi-term-list)
                     (cl-position (current-buffer) my:ansi-term-list)))) ; list is reversed
         (buffer (nth (% pos (length my:ansi-term-list)) my:ansi-term-list)))
    (and (equal (switch-to-buffer  buffer) (current-buffer))
         (message "Switching to %S" (buffer-name buffer)))))

;; Try to update buffer-name with current directory
(defvar-local my:term-current-directory nil)
(defadvice term-command-hook (around my:term-command-hook)
  (and ad-do-it (my:term-refresh-buffer-name)))
(ad-activate 'term-command-hook)

;; install hook for term-mode
(add-hook 'term-mode-hook
          (lambda ()
            (setq-local yas-dont-activate t) ; yas-expand is bound on tab!!!!
            (setq-local my:term-current-directory default-directory)
            (define-key term-raw-map (kbd "C-j") 'my:term-switch-to-shell-mode)))

(provide 'utils-terminal)
