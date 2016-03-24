;;; utils-screenshot.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(setq-default screenshot-schemes
              '(("local" :dir "~/Pictures/")
                ("current-directory :dir default-directory")))

(provide 'utils-screenshot)
