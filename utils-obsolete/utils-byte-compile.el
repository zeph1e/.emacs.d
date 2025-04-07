;;; utils-byte-compile.el -- byte-compile

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

;; compile updated init files on exit
(defconst my:byte-compile-path '( "~/.emacs.d" "~/.emacs.d/utils" ))
(defun my:byte-compile-updated (&optional user-path)
  "Compile updated init files."
  (interactive)
  (let ((user-path (and (interactive-p) (list (read-directory-name "Directory: ")))))
    (dolist (dir (or user-path my:byte-compile-path))
      (if (file-exists-p dir)
          (dolist (file (directory-files dir))
            (when (string-match "^[^\\.]+\\.el\\'" file)
              (let* ((src (concat dir "/" file))
                     (target (concat src "c")))
                (unless (and (file-exists-p target)
                             (file-newer-than-file-p target src))
                    (byte-compile-file src)))))))))
(add-hook 'kill-emacs-hook 'my:byte-compile-updated)

(provide 'utils-byte-compile)
