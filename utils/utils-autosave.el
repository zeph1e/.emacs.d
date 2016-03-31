;;; utils-autosave.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.


;; store backup files in .emacs.d/backups
(defconst emacs-backup-directory "~/.emacs.d/backups/")
(setq backup-directory-alist `((".*" . ,emacs-backup-directory))
      auto-save-file-name-transforms `((".*" ,emacs-backup-directory t)))

(provide 'utils-autosave)
