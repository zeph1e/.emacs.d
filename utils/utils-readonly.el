;;; utils-readonly.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(defvar my:read-only-mode-keymap
  (make-sparse-keymap)
  "my:read-only-mode-keymap")

(defconst my:insert-command-list
  '(self-insert-command
    undefined
    org-self-insert-command))

(define-minor-mode my:read-only-mode
  "My read only mode which have no-mod-key bindings."
  :lighter " READONLY"
  :variable buffer-read-only
  :keymap my:read-only-mode-keymap
  ;; if there is any bind other than inserting p/n character
  ;; this mode will not override it.
  (let ((pkey (key-binding (kbd "p")))
        (nkey (key-binding (kbd "n"))))
    (define-key my:read-only-mode-keymap (kbd "p")
      (and buffer-read-only (member pkey my:insert-command-list) 'previous-line))
    (define-key my:read-only-mode-keymap (kbd "n")
      (and buffer-read-only (member nkey my:insert-command-list) 'next-line))))

;;(add-hook 'help-mode-hook 'my:read-only-mode)
(provide 'utils-readonly)
