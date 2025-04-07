;;; utils-frame.el -- frame related functions

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defun my:make-new-frame ()
  (interactive)
  (and (yes-or-no-p "Create a new frame? ")
       (let ((frame (make-frame))
             (children (process-get server-process :children)))
         (and (framep frame)
              (server-running-p)
              (processp server-process)
              (or (eq (process-get server-process :frame) (selected-frame))
                  (member (selected-frame) (process-get server-process :children)))
              (process-put server-process :children (add-to-list 'children frame)))
         (select-frame-set-input-focus frame))))

(add-hook 'delete-frame-hook
          #'(lambda (frame)
              (process-put server-process :children
                           (remove frame (process-get server-process :children)))))

(defun my:delete-selected-frame ()
  (interactive)
  (and (yes-or-no-p "Delete the current frame? ")
       (delete-frame (selected-frame))))

(defun my:switch-to-next-frame ()
  (interactive)
  (select-frame-set-input-focus (next-frame)))

(defun my:switch-to-previous-frame ()
  (interactive)
  (select-frame-set-input-focus (previous-frame)))

(defun my:fm-next-frame (dir)
  "Select window at direction where the focus entered from."
  (let* ((opposite '((up . bottom) (left . right) (right . left) (down . top)))
         (from (cdr (assoc dir opposite))))
    (unless (window-at-side-p (selected-window) from)
      (select-window (car (window-at-side-list (selected-frame) from))))))
(advice-add 'fm-next-frame :after #'my:fm-next-frame)

(provide 'utils-frame)
