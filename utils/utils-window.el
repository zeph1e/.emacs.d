;;; utils-window.el -- window util

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely
(require 'windmove)

(defun my:buffer-up-copy (&optional arg)
  (interactive "P")
  (my:buffer-copy 'up arg))

(defun my:buffer-down-copy (&optional arg)
  (interactive "P")
  (my:buffer-copy 'down arg))

(defun my:buffer-right-copy (&optional arg)
  (interactive "P")
  (my:buffer-copy 'right arg))

(defun my:buffer-left-copy (&optional arg)
  (interactive "P")
  (my:buffer-copy 'left arg))

(defun my:buffer-copy (dir &optional arg)
  (let ((current-window (selected-window))
        (current-frame (selected-frame))
        (current-buffer (buffer-name))
        (copied-window nil)
        (copied-frame nil)
        (copied-buffer nil)
        (other-window (windmove-find-other-window dir arg nil))
        (opposite (cond ((eq dir 'left) 'right)
                        ((eq dir 'right) 'left))))
      (save-window-excursion
        (if (and (display-graphic-p)
               (null other-window)
               (framep (cond ((eq dir 'left) (next-frame))
                             ((eq dir 'right) (previous-frame))
                             nil)))
            (progn
              (select-frame (next-frame))
              (unless (window-at-side-p (selected-window) opposite)
                (select-window (car (window-at-side-list (selected-frame) opposite)))))
          (windmove-do-window-select dir arg))
        (setq copied-frame (selected-frame))
        (setq copied-window (selected-window))
        (setq copied-buffer (buffer-name)))
    (when (and (not (string= copied-buffer current-buffer)))
      (with-selected-frame copied-frame
        (set-window-buffer copied-window current-buffer)
        (select-window current-window)))))

(defun my:buffer-up-swap (&optional arg)
  (interactive "P")
  (my:buffer-swap 'up arg))

(defun my:buffer-down-swap (&optional arg)
  (interactive "P")
  (my:buffer-swap 'down arg))

(defun my:buffer-right-swap (&optional arg)
  (interactive "P")
  (my:buffer-swap 'right arg))

(defun my:buffer-left-swap (&optional arg)
  (interactive "P")
  (my:buffer-swap 'left arg))

(defun my:buffer-swap (dir &optional arg)
  (let ((current-window (selected-window))
        (current-frame (selected-frame))
        (current-buffer (buffer-name))
        (swapped-window nil)
        (swapped-frame nil)
        (swapped-buffer nil)
        (other-window (windmove-find-other-window dir arg nil))
        (opposite (cond ((eq dir 'left) 'right)
                        ((eq dir 'right) 'left))))
      (save-window-excursion
        (if (and (display-graphic-p)
               (null other-window)
               (framep (cond ((eq dir 'left) (next-frame))
                             ((eq dir 'right) (previous-frame))
                             nil)))
            (progn
              (select-frame (next-frame))
              (unless (window-at-side-p (selected-window) opposite)
                (select-window (car (window-at-side-list (selected-frame) opposite)))))
          (windmove-do-window-select dir arg))
        (setq swapped-frame (selected-frame))
        (setq swapped-window (selected-window))
        (setq swapped-buffer (buffer-name)))
    (when (and (not (string= swapped-buffer current-buffer)))
      (with-selected-frame swapped-frame
        (set-window-buffer swapped-window current-buffer))
      (with-selected-frame current-frame
        (set-window-buffer current-window swapped-buffer)))))

;; To use multi-monitor more usefully
(defun my:move-to-window-or-frame-left (&optional arg)
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

(defun my:move-to-window-or-frame-right (&optional arg)
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
