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

;; excerpted from http://www.emacswiki.org/emacs/ShellMode
;; this switches from-to shell-mode.
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

;;
;; I hate to type `ansi-term' and hope typing `term' to be ansi-term.
;; I also hate to type `enter' to choose program `/bin/bash' because I will
:; not use others than `bash' with `term' command.
;;

;; make "term" to be "ansi-term"
(defadvice term (around my:term-adviced (&optional program new-buffer-name directory))
  (interactive)
  (if (eq system-type 'windows-nt) (funcall 'shell)
    (funcall 'ansi-term (or program "/bin/bash") (or new-buffer-name "term"))))
(ad-activate 'term)

(defvar my:ansi-term-list nil
  "Multiple term mode list to iterate between.")

;; update ansi-term-list on kill-buffer
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (or (equal major-mode 'term-mode)
                      (equal major-mode 'shell-mode))
              (setq my:ansi-term-list (remove (current-buffer) my:ansi-term-list))
              (setq my:term-used-history (remove (current-buffer) my:term-used-history)))))

(defun my:select-prev-ansi-term ()
  "Find previous ansi-term in ringed list of buffers."
  (interactive)
  (let* ((pos (1+ (cl-position (current-buffer) my:ansi-term-list))) ; list is reversed
         (buffer (nth (% pos (length my:ansi-term-list)) my:ansi-term-list)))
    (and (equal (my:term-switch-to-buffer  buffer) (current-buffer))
         (message "Switching to %S" (buffer-name buffer)))))

(defun my:select-next-ansi-term ()
  "Find next ansi-term in ringed list of buffers."
  (interactive)
  (let* ((pos (1- (+ (length my:ansi-term-list)
                     (cl-position (current-buffer) my:ansi-term-list)))) ; list is reversed
         (buffer (nth (% pos (length my:ansi-term-list)) my:ansi-term-list)))
    (and (equal (my:term-switch-to-buffer  buffer) (current-buffer))
         (message "Switching to %S" (buffer-name buffer)))))

(defun my:term-update-directory ()
  "Update buffer-name when directory is changed."
  (and (not (equal my:term-current-directory default-directory))
       (setq-local my:term-current-directory default-directory)
       (my:term-refresh-buffer-name)))

(defvar my:term-used-history nil
  "The terminal buffer which used at last")

;; to notify current major mode or directory is switched by changing its buffer name
(defun my:term-refresh-buffer-name (&optional buffer)
  (let* ((buf (or buffer (current-buffer)))
         (bufname (buffer-name buf))
         (bufinfo (or (when (string-match
                             "\\*\\(term\\|terminal\\|ansi-term\\)\\((S)\\)?\\(:.+\\)?\\*\\(<.+>\\)?"
                             bufname)
                        (split-string (replace-match "\\1 \\4" t nil bufname)))
                      (error "Unable to get original mode!")))
         (modename (car bufinfo))
         (modeident (cadr bufinfo))
         (toggled (not (equal 'term-mode major-mode)))
         (seed (concat "*" modename (and toggled "(S)")
                            ":" (abbreviate-file-name default-directory) "*"))
         (candidate (concat seed modeident)))
    (or (equal candidate (buffer-name (current-buffer)))
        (rename-buffer (generate-new-buffer-name seed)))
    (add-to-list 'my:term-used-history buf)))

;;
;; There's no hooks for directory update or exit in term-mode
;; So, advice is used instead
;;
;; Try to update buffer-name with current directory
(defvar my:term-current-directory nil)
(defadvice term-command-hook (around my:term-command-hook-adviced)
  (and ad-do-it (my:term-update-directory)))
(ad-activate 'term-command-hook)

;;close terminal buffer on exit
(defadvice term-handle-exit (after my:term-handle-exit-adviced)
  (kill-buffer (current-buffer)))
(ad-activate 'term-handle-exit)

;; install hook for term-mode
(add-hook 'term-mode-hook
          (lambda ()
            (and (boundp 'yas-minor-mode) ; yas-expand is bound on tab!!!!
                 (setq-local yas-dont-activate t)
                 (yas-minor-mode -1))
            (set (make-local-variable 'my:term-current-directory) nil)
            (my:term-update-directory)
            (push (current-buffer) my:ansi-term-list)
            (define-key term-raw-map (kbd "M-<left>") 'my:select-prev-ansi-term)
            (define-key term-raw-map (kbd "M-<right>") 'my:select-next-ansi-term)
            (define-key term-raw-map (kbd "C-c ?") 'my:term-list-popup)
            (define-key term-raw-map (kbd "C-j") 'my:term-switch-to-shell-mode)))

;; For hot-key functions
(defun my:term-switch-to-buffer (buffer-or-name)
  (let (term-win)
    (dolist (win (window-list))
      (with-current-buffer (window-buffer win)
        (and (eq (get-buffer buffer-or-name) (current-buffer))
             (setq term-win win))))
    (if term-win (select-window term-win)
      (switch-to-buffer buffer-or-name))))

(defun my:term-get-create ()
  "Get terminal for current directory or create if there's no such terminal buffer."
  (interactive)
  (or (with-current-buffer (current-buffer)
        (let ((dir (abbreviate-file-name default-directory)) found)
          (dolist (buf my:ansi-term-list)
            (with-current-buffer buf
              (and (equal dir (abbreviate-file-name default-directory))
                   (setq found buf))))
          (and found (my:term-switch-to-buffer found))))
      (term)))

(defun my:term-get-last-used ()
  "Pick last used terminal."
  (interactive)
  (if my:term-used-history (my:term-switch-to-buffer (car my:term-used-history))
      (my:term-get-create)))

(provide 'utils-terminal)
