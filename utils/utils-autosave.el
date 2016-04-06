;;; utils-autosave.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.


;; store backup files in .emacs.d/backups
(defconst emacs-backup-directory "~/.emacs.d/backups/")
(setq backup-directory-alist `((".*" . ,emacs-backup-directory))
      auto-save-file-name-transforms `((".*" ,emacs-backup-directory t)))

;; create auto save backup directory unless it exists
(if (file-exists-p emacs-backup-directory)
    (unless (file-directory-p emacs-backup-directory)
      (warn "Auto save backup path, %s is not a directory!" emacs-backup-directory))
  (mkdir emacs-backup-directory))

(provide 'utils-autosave)
