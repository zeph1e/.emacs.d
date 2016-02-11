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
  "Multiple term mode list to iterate between. Ordered by creation.")

(defvar my:term-recent-history nil
  "The terminal buffer which used recently. Ordered by recent.")

(defvar my:term-current-directory nil
  "Current directory of terminal.")
(make-variable-buffer-local 'my:term-current-directory)


(defun my:term-buffer-p (&optional buffer)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (and (equal major-mode 'term-mode)
           my:term-current-directory))))

;; update ansi-term-list on kill-buffer
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (my:term-buffer-p)
              (setq my:ansi-term-list (remove (current-buffer) my:ansi-term-list))
              (setq my:term-recent-history (remove (current-buffer) my:term-recent-history)))))

(add-hook 'buffer-list-update-hook
          (lambda ()
            (let ((buffer (car (buffer-list))))
              (when (and (my:term-buffer-p buffer)
                         (buffer-live-p buffer))
                (setq my:term-recent-history (remove buffer my:term-recent-history))
                (push buffer my:term-recent-history)))))

(defun my:term-select-prev-ansi-term ()
  "Find previous ansi-term in ringed list of buffers."
  (interactive)
  (let* ((pos (1+ (cl-position (current-buffer) my:ansi-term-list))) ; list is reversed
         (buffer (nth (% pos (length my:ansi-term-list)) my:ansi-term-list)))
    (and (equal (my:term-switch-to-buffer  buffer) (current-buffer))
         (message "Switching to %S" (buffer-name buffer)))))

(defun my:term-select-next-ansi-term ()
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

;; to notify current major mode or directory is switched by changing its buffer name
(defun my:term-refresh-buffer-name (&optional buffer)
  (let* ((buf (or buffer (current-buffer)))
         (bufname (buffer-name buf))
         (bufinfo (or (when (string-match
                             "\\*\\(term\\|terminal\\|ansi-term\\)\\(:.+\\)?\\*\\(<.+>\\)?"
                             bufname)
                        (split-string (replace-match "\\1 \\3" t nil bufname)))
                      (error "Unable to get original mode!")))
         (modename (car bufinfo))
         (modeident (cadr bufinfo))
         (toggled (not (equal 'term-mode major-mode)))
         (seed (concat "*" modename (and toggled "(S)")
                            ":" (abbreviate-file-name default-directory) "*"))
         (candidate (concat seed modeident)))
    (or (equal candidate (buffer-name (current-buffer)))
        (rename-buffer (generate-new-buffer-name seed)))))

;;
;; There's no hooks for directory update or exit in term-mode
;; So, advice is used instead
;;
;; Try to update buffer-name with current directory
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
            (my:term-update-directory)
            (add-to-list 'my:ansi-term-list (current-buffer))
            (define-key term-raw-map (kbd "M-<left>") 'my:term-select-prev-ansi-term)
            (define-key term-raw-map (kbd "M-<right>") 'my:term-select-next-ansi-term)
            (define-key term-raw-map (kbd "C-c ?") 'my:term-list-popup)))


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

(defun my:term-get-recent ()
  "Pick last used terminal."
  (interactive)
  (if my:term-recent-history (my:term-switch-to-buffer (car my:term-recent-history))
      (my:term-get-create)))

(defvar my:term-list-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'my:term-list-next)
    (define-key map (kbd "p") 'my:term-list-prev)
    (define-key map (kbd "RET") 'my:term-list-select)
    (define-key map (kbd "SPC") 'my:term-list-show)
    (define-key map (kbd "q") 'my:term-list-quit)
    (define-key map (kbd "C-g") 'my:term-list-quit)
    map)
  "Terminal list popup keybinding.")

(define-derived-mode my:term-list-mode fundamental-mode ""
  (use-local-map my:term-list-keymap)
  (set (make-local-variable 'buffer-read-only) t))

(defun my:term-list-next ()
  (interactive)
  (next-line)
  (my:term-list-show))

(defun my:term-list-prev ()
  (interactive)
  (previous-line)
  (my:term-list-show))

(defun my:term-list-select ()
  (interactive)
  (let ((popup-window (selected-window))
        buffer-name)
    (with-current-buffer (window-buffer popup-window)
      (setq buffer-name (get-text-property (point) 'buffer-name)))
    (my:term-list-quit (get-buffer buffer-name))))

(defun my:term-list-show ()
  (interactive)
  (let ((popup-window (selected-window))
        (parent-window my:term-list-parent-window)
        buffer-name)
    (with-current-buffer (window-buffer popup-window)
      (setq buffer-name (get-text-property (point) 'buffer-name)))
    (when (and parent-window buffer-name)
      (select-window parent-window)
      (switch-to-buffer buffer-name)
      (select-window popup-window))))

(defun my:term-list-quit (&optional buffer)
  (interactive)
  (when (equal major-mode 'my:term-list-mode)
    (let (restore popup)
      (with-current-buffer (current-buffer)
        (setq popup (current-buffer))
        (setq restore (or buffer
                          (and (buffer-live-p my:term-list-parent-window-buffer)
                               my:term-list-parent-window-buffer))))
      (delete-window (selected-window))
      (and my:term-list-parent-window
           (select-window my:term-list-parent-window))
      (switch-to-buffer restore)
      (kill-buffer popup)
      (setq-local my:term-list-parent-window nil)
      (setq-local my:term-list-parent-window-buffer nil))))

(defvar my:term-list-parent-window nil)
(make-variable-buffer-local 'my:term-list-parent-window)

(defvar my:term-list-parent-window-buffer nil)
(make-variable-buffer-local 'my:term-list-parent-window-buffer)

(defun my:term-list-popup ()
  (interactive)
  (let ((parent-window (selected-window))
        (popup-buffer (get-buffer-create "*Terminal List*"))
        (inhibit-read-only t))
    (with-current-buffer popup-buffer
      (delete-region (point-min)(point-max))
      (goto-char (point-min))
      (dolist (term-buf my:term-recent-history)
        (insert (propertize (buffer-name term-buf)
                            'buffer-name (buffer-name term-buf))"\n"))
      (my:term-list-mode)
      (setq-local my:term-list-parent-window parent-window)
      (setq-local my:term-list-parent-window-buffer (window-buffer parent-window))
      (goto-char (point-min)))
    (select-window (display-buffer popup-buffer '(display-buffer-below-selected)))
    (fit-window-to-buffer (selected-window) 15 5)))

(provide 'utils-terminal)
