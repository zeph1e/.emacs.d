;;; utils-window.el -- window util

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely
(require 'windmove)

(defun my:buffer-up-copy()
  (interactive)
  (my:buffer-copy 'windmove-up-cycle))

(defun my:buffer-down-copy()
  (interactive)
  (my:buffer-copy 'windmove-down-cycle))

(defun my:buffer-right-copy()
  (interactive)
  (my:buffer-copy 'windmove-right-cycle))

(defun my:buffer-left-copy()
  (interactive)
  (my:buffer-copy 'windmove-left-cycle))

(defun my:buffer-copy(cycle-function)
  (let ((current-window (selected-window))
        (current-buffer (buffer-name))
        (copied-window nil)
        (copied-buffer nil))
    (funcall cycle-function)
    (setq copied-window (selected-window))
    (setq copied-buffer (buffer-name))
    (when (and (not (string= copied-buffer current-buffer)))
      (set-window-buffer copied-window current-buffer)
      (select-window current-window))))

;; To use multi-monitor more usefully
(defun my:move-window-or-frame-left (&optional arg)
  (interactive "P")
  (let ((other-window (windmove-find-other-window 'left arg nil)))
    (if (and (display-graphic-p)
             (null other-window)
             (framep (next-frame)))
        (progn
          (select-frame-set-input-focus (next-frame))
          (unless (window-at-side-p (selected-window) 'right)
            (select-window (car (window-at-side-list (selected-frame) 'right)))))
      (windmove-left arg))))

(defun my:move-window-or-frame-right (&optional arg)
  (interactive "P")
  (let ((other-window (windmove-find-other-window 'right arg nil)))
    (if (and (display-graphic-p)
             (null other-window)
             (framep (previous-frame)))
        (progn
          (select-frame-set-input-focus (previous-frame))
          (unless (window-at-side-p (selected-window) 'left)
            (select-window (car (window-at-side-list (selected-frame) 'left)))))
      (windmove-right arg))))

(provide 'utils-window)
