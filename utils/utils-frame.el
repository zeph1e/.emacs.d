;;; utils-frame.el -- frame related functions

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defun my:make-new-frame ()
  (interactive)
  (and (yes-or-no-p "Create a new frame? ")
       (select-frame (make-frame))))

(defun my:delete-selected-frame ()
  (interactive)
  (and (yes-or-no-p "Delete the current frame? ")
       (delete-frame (selected-frame))))

(defun my:switch-to-next-frame ()
  (interactive)
  (select-frame (next-frame)))

(defun my:switch-to-previous-frame ()
  (interactive)
  (select-frame (previous-frame)))

(provide 'utils-frame)
