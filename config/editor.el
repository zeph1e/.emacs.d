;;; editor.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package iedit
  :bind
  (:map my:global-key-map
   ("C-M-#" . iedit-mode)))

(use-package multiple-cursors
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
