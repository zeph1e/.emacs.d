;;; utils-aliases.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defalias 'ke! #'(lambda () (interactive) (save-some-buffers) (kill-emacs))
  "Save buffers and kill emacs.")

(provide 'utils-aliases)
