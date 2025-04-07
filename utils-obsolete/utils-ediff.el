;;; utils-ediff.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; command line diff & merge with emacsclient
(defvar my:ediff-frame nil)
(make-variable-frame-local 'my:ediff-frame)

(defun my:command-line-diff (switch)
  (let* ((file-a (pop command-line-args-left))
         (file-b (pop command-line-args-left)))
    (add-hook 'ediff-quit-hook 'kill-emacs)
    (ediff-files file-a file-b)))

(defun my:command-line-merge (switch)
  (let* ((file-a (pop command-line-args-left))
         (file-b (pop command-line-args-left))
         (file-out (pop command-line-args-left))
         (file-ancestor (pop command-line-args-left)))
    (add-hook 'ediff-quit-hook 'kill-emacs)
    (if (and file-ancestor
             (file-exists-p file-ancestor)
             (file-readable-p file-ancestor))
        (ediff-merge-files-with-ancestor file-a file-b file-ancestor nil file-out)
      (ediff-merge-files file-a file-b nil file-out))))

(add-to-list 'command-switch-alist '("diff" . my:command-line-diff))
(add-to-list 'command-switch-alist '("merge" . my:command-line-merge))

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
  (add-hook 'ediff-quit-hook #'my:ediff-cmd-cleanup-frame)
  (my:ediff-cmd-register-frame (selected-frame))
  (ediff-files file-a file-b))

(defun my:emerge-cmd (file-a file-b file-out &optional file-ancestor)
  (add-hook 'ediff-quit-hook #'my:ediff-cmd-cleanup-frame)
  (my:ediff-cmd-register-frame (selected-frame))
  (if (and file-ancestor
           (file-exists-p file-ancestor)
           (file-readable-p file-ancestor))
      (ediff-merge-files-with-ancestor file-a file-b file-ancestor nil file-out)
    (ediff-merge-files file-a file-b nil file-out)))

;; ediff help functions
(defun my:ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(add-hook 'ediff-keymap-setup-hook (lambda () (define-key ediff-mode-map "c" 'my:ediff-copy-both-to-C)))
(provide 'utils-ediff)
