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
(defun my:rename-term-shell-buffer (&optional buffer)
  (let* ((buf (or buffer (current-buffer)))
         (bufname (buffer-name buf))
         (modename (or (when (string-match "\\*\\(terminal\\|shell\\)\\(-\\(shell\\|term\\)\\)?\\*"
                                           bufname)
                         (replace-match "\\1" t nil bufname))
                       (error "Unable to get original mode!")))
         (mode (or (and (string= "terminal" modename) 'term-mode)
                   (and (string= "shell" modename) 'shell-mode)))
         (toggled-mode (when (not (equal mode major-mode))
                         (cond ((equal major-mode 'shell-mode) "shell")
                               ((equal major-mode 'term-mode) "term")))))
    (rename-buffer (concat "*" modename (and toggled-mode (concat "-" toggled-mode)) "*"))))

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
  (my:rename-term-shell-buffer))

(add-hook 'shell-mode-hook
          (lambda () (local-set-key (kbd "C-j") 'my:term-switch-to-shell-mode)))

(add-hook 'term-mode-hook
          (lambda ()
            (setq-local yas-dont-activate t) ; yas-expand is bound on tab!!!!
            (define-key term-raw-map (kbd "C-j") 'my:term-switch-to-shell-mode)))
(provide 'utils-terminal)
