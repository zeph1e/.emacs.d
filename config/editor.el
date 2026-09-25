;;; editor.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package iedit
  :bind
  (:map my:global-key-map
   ("C-M-#" . iedit-mode)))

(use-package multiple-cursors
  :config
  (defvar my:mc--peek-origin nil)  ; marker at the original cursor
  (defvar my:mc--peek-start nil)   ; window-start before the peek
  (defvar my:mc--peek-timer nil)

  (defun my:mc--peek-end ()
    "Cycle back to the original cursor and restore its view."
    (let ((origin (and my:mc--peek-origin
                       (eq (marker-buffer my:mc--peek-origin) (current-buffer))
                       (mc/fake-cursor-at-point
                        (marker-position my:mc--peek-origin)))))
      (when origin
        (mc/cycle origin nil nil)
        (set-window-start nil my:mc--peek-start t)))
    (when my:mc--peek-origin (set-marker my:mc--peek-origin nil))
    (when my:mc--peek-timer (cancel-timer my:mc--peek-timer))
    (setq my:mc--peek-origin nil my:mc--peek-timer nil)
    (remove-hook 'pre-command-hook #'my:mc--peek-end))

  (defun my:mc--reveal-newest-cursor (_skip-last direction)
    "Cycle to the newest match when it is off screen."
    (let ((newest (if (eq direction 'forwards)
                      (mc/furthest-cursor-after-point)
                    (mc/furthest-cursor-before-point))))
      (when (and newest
                 (not (pos-visible-in-window-p (overlay-get newest 'point))))
        (setq my:mc--peek-start (window-start)
              my:mc--peek-origin (point-marker))
        (mc/cycle newest nil nil)
        (add-hook 'pre-command-hook #'my:mc--peek-end)
        (setq my:mc--peek-timer
              (run-with-idle-timer 1.5 nil #'my:mc--peek-end)))))
  (advice-add 'mc/mark-more-like-this :after #'my:mc--reveal-newest-cursor)
  :bind
  (:map my:global-key-map
   ("M-?" . mc/edit-lines)
   ("M-." . mc/mark-next-like-this)
   ("M-," . mc/mark-previous-like-this)
   ("M-/" . mc/mark-all-like-this)))

(use-package simple
  :ensure nil
  :pin manual
  :config
  ;; idea from https://www.emacswiki.org/emacs/MarkCommands
  (defun my:mark-word (&optional arg allow-extend)
    "Mark word at point."
    (interactive "p\np")
    (my:mark-thing 'word arg allow-extend))

  (defun my:mark-symbol (&optional arg allow-extend)
    "Mark symbol at point."
    (interactive "p\np")
    (my:mark-thing 'symbol arg allow-extend))

  (defun my:mark-thing (thing arg allow-extend)
    "Mark things."
    (setq arg (if arg arg 1))
    (if (and allow-extend
             (or (and (eq last-command this-command) (mark t))
                 (region-active-p)))
        (set-mark
         (save-excursion
           (when (< (mark) (point))
             (setq arg (- arg)))
           (goto-char (mark))
           (forward-word arg)
           (point)))
      (let ((bounds (bounds-of-thing-at-point thing)))
        (unless (consp bounds)
          (error "No %s at point" (symbol-name thing)))
        (if (>= arg 0)
            (goto-char (car bounds))
          (goto-char (cdr bounds)))
        (push-mark (save-excursion
                     (funcall (intern (format "forward-%s" thing)) arg)
                     (point)))
        (activate-mark))))

  (defun my:forward-to-indentation ()
    "Move forward to the first nonblank characther at the current line."
    (interactive)
    (forward-to-indentation 0))

  ;; kill heading spaces on kill-line:
  ;; from http://emacswiki.org/emacs/DeletingWhitespace
  (defun my:kill-line (&rest arg)
    "cleanup whitespace on kill-line"
    (if (not (bolp))
        (delete-region (point) (progn (skip-chars-forward " \t") (point)))))
  (advice-add 'kill-line :after #'my:kill-line)

  (defun my:open-line-above ()
    "Open a new line at the beginning of line"
    (interactive)
    (beginning-of-line)
    (open-line 1))

  (defun my:new-line-below ()
    "Insert new line at the end of line"
    (interactive)
    (end-of-line)
    (newline))

  (defun my:toggle-buffer-read-only ()
    "Toggles read-only flag of the current buffer."
    (interactive)
    (setq-local buffer-read-only (null buffer-read-only)))

  :bind
  (:map my:global-key-map
   ("M-@" . my:mark-word)
   ("M-#" . my:mark-symbol)
   ("M-SPC" . my:forward-to-indentation)
   ("M-S-SPC" . just-one-space)
   ("C-o" . my:open-line-above)
   ("M-o" . my:new-line-below)
   ("<f12>" . my:toggle-buffer-read-only)))

(use-package redo+
  :ensure nil
  :bind
  (:map my:global-key-map
   ("C-_" . undo)
   ("M-_" . redo)))

(use-package block-travel
  :ensure nil
  :vc (:url "https://github.com/emacs-vs/block-travel" :rev :newest)
  :bind
  (("M-p" . block-travel-up) ; let these be overwritten
   ("M-n" . block-travel-down)))

(use-package ibuffer
  :ensure nil
  :pin manual
  :bind
  (:map my:global-key-map
        ("C-x C-b" . ibuffer)))

(use-package indent-bars
  :ensure t
  :pin gnu)
