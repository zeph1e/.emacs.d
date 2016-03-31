;;; utils-buffer.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

;; revert all buffers that are visiting a file: from http://emacswiki.org/emacs/RevertBuffer
(defun my:revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed all opened files."))

(defun my:revert-local-buffers ()
  "Refreshes only local buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (not (file-remote-p (buffer-file-name (current-buffer))))
                 (not (buffer-modified-p)))
        (revert-buffer t t t))))
    (message "Refreshed opened local files."))


(provide 'utils-buffer)
