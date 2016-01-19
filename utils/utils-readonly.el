;;; utils-readonly.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(defvar my:read-only-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map)
  "my:read-only-mode-keymap")

(define-minor-mode my:read-only-mode
  "My read only mode which have no-mod-key bindings."
  :lighter " READONLY"
  :variable buffer-read-only
  :keymap my:read-only-mode-keymap)

(provide 'utils-readonly)
