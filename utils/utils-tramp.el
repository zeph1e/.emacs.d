;; utils-tramp.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(setq enable-remote-dir-locals t)

(defconst my:tramp-own-remote-path '("/home/ysjang/.local/bin")
  "The remote paths need to search first")

(eval-after-load 'tramp
  `(progn
     (dolist (path my:tramp-own-remote-path)
       (add-to-list 'tramp-remote-path path))

     (when (executable-find "ssh")
       (setq-default tramp-default-method "ssh"))

     ))

(provide 'utils-tramp)
