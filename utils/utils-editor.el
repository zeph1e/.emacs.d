;;; utils-editor.el -- modify editor behavior

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defun my:scroll-up-command (&optional arg)
  "Modify scroll-up behavior to make it move to the end of buffer."
  (interactive "P")
  (if (eq (point)(point-max))
      (signal 'end-of-buffer '())
    (condition-case e
        (scroll-up-command arg)
      (end-of-buffer (goto-char (point-max))))))

(defun my:scroll-down-command (&optional arg)
  "Modify scroll-down behaviour to make it move to the beginning of buffer."
  (interactive "P")
  (if (eq (point)(point-min))
      (signal 'beginning-of-buffer '())
    (condition-case e
        (scroll-down-command arg)
      (beginning-of-buffer (goto-char (point-min))))))

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

;; kill heading spaces on kill-line : from http://emacswiki.org/emacs/DeletingWhitespace
(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

;; split horizontally first, from http://www.emacswiki.org/emacs/HorizontalSplitting
(defun my:split-window-prefer-horizonally (window)
  "If there's only one window (excluding any possibly active
minibuffer), then split the current window horizontally."
  (if (and (one-window-p t)
           (not (active-minibuffer-window)))
      (let ((split-height-threshold nil))
        (split-window-sensibly window))
    (split-window-sensibly window)))
(setq split-window-preferred-function 'my:split-window-prefer-horizonally)

;; My white space mode:
;;  - highlight trailing spaces
;:  - highlight tabs: http://www.emacswiki.org/emacs/ShowWhiteSpace
(defface my:tab-face '((t :foreground "gray50" :background "dark slate gray"))
  "Used for tab highlighting."
  :group 'basic-faces)

(defvar my:buffer-display-table)
(make-variable-buffer-local 'my:buffer-display-table)

(define-minor-mode my:whitespace-mode
  "Highlight trailing white space and tab"
  :variable my:whitespace-mode
  (setq show-trailing-whitespace my:whitespace-mode)
  (if my:whitespace-mode
      (progn
        (font-lock-add-keywords nil '(("\t" . 'my:tab-face)))
        (setq my:buffer-display-table buffer-display-table)
        (let ((table (or buffer-display-table
                         (make-display-table))))
          (aset table ?\t (vector 187 ?\t))
          (setq buffer-display-table table)))
    (setq-local buffer-display-table my:buffer-display-table)
    (font-lock-remove-keywords nil '(("\t" . 'my:tab-face))))
  (font-lock-fontify-buffer))

(provide 'utils-editor)
