;;; utils-window.el -- window util

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

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
      (select-window copied-window))))

(provide 'utils-window)
