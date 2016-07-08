;;; utils-keybinding.el -- key bindings

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

;; global key-bindings
(defvar my:keys-mode-keymap
  (let ((map (make-sparse-keymap)))
    ;; forward-to-indentation
    (define-key map (kbd "M-SPC") (lambda () (interactive)(forward-to-indentation 0)))
    (define-key map (kbd "M-S-SPC") 'just-one-space) ; original function on M-SPC

    (define-key map (kbd "M-F") 'forward-symbol)
    (define-key map (kbd "M-B") (lambda (&optional arg) (interactive "p")
                                  (forward-symbol (- (or arg 1))))) ; backward-symbol doesn't exist

    (define-key map (kbd "C-M-q") 'bury-buffer)
    (define-key map (kbd "C-S-M-q") 'unbury-buffer)

    ;; scroll to buffer beginning/end
    (define-key map (kbd "C-v") 'my:scroll-up-command)
    (define-key map (kbd "M-v") 'my:scroll-down-command)

    ;; windmove
    (define-key map (kbd "S-<left>")  'windmove-left)
    (define-key map (kbd "S-<right>") 'windmove-right)
    (define-key map (kbd "<select>")  'windmove-up) ; sometime, shift-up is sent as <select> to remote
    (define-key map (kbd "S-<up>")    'windmove-up)
    (define-key map (kbd "S-<down>")  'windmove-down)

    ;; framemove
    (define-key map (kbd "M-<left>")  'fm-left-frame)
    (define-key map (kbd "M-<right>") 'fm-right-frame)
    (define-key map (kbd "M-<up>")    'fm-up-frame)
    (define-key map (kbd "M-<down>")  'fm-down-frame)

    ;; windcycle
    (define-key map (kbd "C-<up>")    'buffer-up-swap) ; Switch window key-bindings
    (define-key map (kbd "C-<down>")  'buffer-down-swap)
    (define-key map (kbd "C-<right>") 'buffer-right-swap)
    (define-key map (kbd "C-<left>")  'buffer-left-swap)

    ;; util-window
    (define-key map (kbd "C-S-<up>")    'my:buffer-up-copy)
    (define-key map (kbd "C-S-<down>")  'my:buffer-down-copy)
    (define-key map (kbd "C-S-<left>")  'my:buffer-left-copy)
    (define-key map (kbd "C-S-<right>") 'my:buffer-right-copy)

    ;; frame key bindings
    (define-key map (kbd "C-x <up>") 'my:make-new-frame)
    (define-key map (kbd "C-x <down>") 'my:delete-selected-frame)
    (define-key map (kbd "C-x <right>") 'my:switch-to-next-frame)
    (define-key map (kbd "C-x <left>") 'my:switch-to-previous-frame)

    (define-key map (kbd "C-x -")     'split-window-vertically) ; Window Split key-bindings
    (define-key map (kbd "C-x \\")     'split-window-horizontally)
    (define-key map (kbd "C-x _")     #'(lambda () (interactive)
                                          (split-window-vertically)
                                          (windmove-down)))
    (define-key map (kbd "C-x |")     #'(lambda () (interactive)
                                          (split-window-horizontally)
                                          (windmove-right)))

    (define-key map (kbd "C-x x")     'delete-window) ; Window Close key-bindings

    (define-key map (kbd "C-x C-o") 'ff-find-other-file)

    ;; set-fill-column updates whitespace-mode's lines-tail highlight
    (define-key map (kbd "C-x f") (lambda (arg)
                                    (interactive
                                     (list (call-interactively 'set-fill-column)))
                                    (when (and (boundp my:whitespace-mode)
                                               my:whitespace-mode)
                                      (my:whitespace-mode -1)
                                      (my:whitespace-mode t))))
    ;; revert files
    (define-key map (kbd "<f5>") 'my:revert-this-buffer)
    (define-key map (kbd "C-<f5>") 'my:revert-all-buffers)

    ;; tramp cleanup
    (define-key map (kbd "<f6>") (lambda () (interactive)
                                   (when (y-or-n-p "Cleanup all tramp connections? ")
                                     (tramp-cleanup-all-connections)
                                     (message "Cleaned all tramp connections up"))))
    (define-key map (kbd "C-<f6>") (lambda () (interactive)
                                     (when (y-or-n-p "Cleanup all tramp buffers? ")
                                       (tramp-cleanup-all-buffers)
                                       (message "Cleaned all tramp buffers up"))))

    ;; flyspell-mode
    (define-key map (kbd "<f8>") 'my:flyspell-mode)
    (define-key map (kbd "C-<f8>") 'flyspell-buffer)

    ;; undo+
    (define-key map (kbd "C-_") 'undo)
    (define-key map (kbd "M-_") 'redo)

    ;; modified mark word
    (define-key map (kbd "M-@") 'my:mark-word)
    (define-key map (kbd "M-#") 'my:mark-symbol)

    ;; iedit
    (define-key map (kbd "C-M-#") 'iedit-mode)

    ;; multiple-cursors
    (define-key map (kbd "M-?") 'mc/edit-lines)
    (define-key map (kbd "M-.") 'mc/mark-next-like-this)
    (define-key map (kbd "M-,") 'mc/mark-previous-like-this)
    (define-key map (kbd "M-/") 'mc/mark-all-like-this)

    ;; vi-like line insertion
    (define-key map (kbd "C-o") (lambda () (interactive)(beginning-of-line)(open-line 1)))
    (define-key map (kbd "M-o") (lambda () (interactive)(end-of-line)(newline)))

    ;; helm
    (define-key map (kbd "M-x") 'helm-M-x)
    (define-key map (kbd "M-y") 'helm-show-kill-ring)
    (define-key map (kbd "C-x C-f") 'helm-find-files)
    (define-key map (kbd "C-x r b") 'helm-filtered-bookmarks)
    (define-key map (kbd "C-x b") 'helm-mini)
    (define-key map (kbd "C-h a") 'helm-apropos)
    (define-key map (kbd "C-h b") 'helm-descbinds)
    (define-key map (kbd "M-r") 'helm-occur)
    (define-key map (kbd "C-M-r") 'helm-ag)
    (define-key map (kbd "M-R") 'helm-grep-do-git-grep)
    (define-key map (kbd "M-g s") 'helm-google-suggest)
    (define-key map (kbd "M-t") 'helm-ls-git-ls)
    (define-key map (kbd "M-T") 'helm-cmd-t)

    ;; keep original M-x
    (define-key map (kbd "M-X") 'execute-extended-command)

    ;; auto-complete
    (define-key map (kbd "C-;") 'auto-complete)
    (define-key map (kbd "C-:") 'ac-complete-with-helm)

    ;; magit
    (define-key map (kbd "C-x RET C-s") 'magit-status)
    (define-key map (kbd "C-x RET C-b") 'magit-blame)
    (define-key map (kbd "C-x RET C-f") 'magit-log-buffer-file)
    (define-key map (kbd "C-x RET C-l") (lambda () (interactive) (magit-log-head)))

    ;; term
    (define-key map (kbd "C-x t") 'my:term-get-create)
    (define-key map (kbd "C-x y") 'my:term-get-recent)

    ;; read-only
    (define-key map (kbd "C-x C-q") 'my:read-only-mode) ; override default key
    (define-key map (kbd "<f12>") 'my:read-only-mode)
    map)
  "global key mode keymap")

(define-minor-mode my:keys-mode
"My global key map to prevent annoying overriding of major modes

Key bindings:
\\{my:keys-mode-keymap}"
  t nil my:keys-mode-keymap)
(my:keys-mode t)

(defadvice load (after my:keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my:keys-mode))
      (let ((mykeys (assq 'my:keys-mode minor-mode-map-alist)))
        (assq-delete-all 'my:keys-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;; ignore mouse
(dolist (k `([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             ;; [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4] ; wheel-up
             ;; [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5] ; wheel-down
             [mouse-6] [down-mouse-6] [drag-mouse-6] [double-mouse-6] [triple-mouse-6] ; wheel-tilt-left
             [mouse-7] [down-mouse-7] [drag-mouse-7] [double-mouse-7] [triple-mouse-7] ; wheel-tilt-right
             [mouse-8] [down-mouse-8] [drag-mouse-8] [double-mouse-8] [triple-mouse-8] ; back
             [mouse-9] [down-mouse-9] [drag-mouse-9] [double-mouse-9] [triple-mouse-9] ; forward
             ,(kbd "C-z")
             ))
  (global-set-key k 'ignore))
;; unbind windcycle default
(dolist (k '([M-left] [M-right] [M-up] [M-down]
             [M-S-left] [M-S-right] [M-S-up] [M-S-down]))
  (global-unset-key k))

;; Disable C-x C-c only for server frame
(global-set-key (kbd "C-x C-c")
                (lambda (&optional ARG)
                  (interactive)
                  (if (and (processp server-process)
                           (not (and (equal (process-get server-process :terminal)
                                            (frame-terminal))
                                     (or
                                      (equal (process-get server-process :frame)
                                             (selected-frame))
                                      (member (selected-frame)
                                              (process-get server-process :children))))))
                      (save-buffers-kill-terminal ARG)
                  (error "C-x C-c is disabled. Use M-x kill-emacs instead."))))
(provide 'utils-keybinding)
