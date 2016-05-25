;; utils-tramp.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(defconst my:tramp-own-remote-path '("/home/ysjang/.local/bin")
  "The remote paths need to search first")

(eval-after-load 'tramp
  `(add-to-list 'tramp-remote-path 'my:tramp-own-remote-path))

(provide 'utils-tramp)
