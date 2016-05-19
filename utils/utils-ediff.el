;;; utils-ediff.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; command line diff & merge with emacsclient
(defvar my:ediff-frame nil)
(make-variable-frame-local 'my:ediff-frame)

(defun my:ediff-cmd-register-frame (&optional frame)
  (setq my:ediff-frame (or frame (selected-frame))))

(defun my:ediff-cmd-cleanup-frame ()
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (when (and (boundp 'my:ediff-frame)
                 (framep my:ediff-frame)
                 (equal frame my:ediff-frame))
        (delete-frame my:ediff-frame)
        (setq my:ediff-frame nil)))))

(defun my:ediff-cmd (file-a file-b)
  (add-hook 'ediff-quit-hook #'(lambda ()
                                 (dolist (buf (list ediff-buffer-A ediff-buffer-B))
                                   (kill-buffer-if-not-modified buf))
                                 (my:ediff-cmd-cleanup-frame)))
  (my:ediff-cmd-register-frame (selected-frame))
  (ediff-files file-a file-b))

(defun my:emerge-cmd (file-a file-b file-out)
  (add-hook 'ediff-quit-hook #'(lambda ()
                                 (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
                                   (kill-buffer-if-not-modified buf))
                                 (my:ediff-cmd-cleanup-frame)))
  (my:ediff-cmd-register-frame (selected-frame))
  (ediff-merge-files file-a file-b nil file-out))

(defun my:emerge-with-ancestor-cmd (file-a file-b file-out file-ancestor)
  (add-hook 'ediff-quit-hook #'(lambda ()
                                 (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
                                   (kill-buffer-if-not-modified buf))
                                 (my:ediff-cmd-cleanup-frame)))
  (my:ediff-cmd-register-frame (selected-frame))
  (ediff-merge-files-with-ancestor file-a file-b file-ancestor nil file-out))

(provide 'utils-ediff)
