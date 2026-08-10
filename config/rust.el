;;; rust.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package rust-mode
  :pin melpa
  :config
  (defun my:run-command-to-string (command)
    "Run shell `command' and return output on success. Otherwise error."
    (with-temp-buffer
      (let* ((cmd (split-string-shell-command command))
             (p (car cmd))
             (a (cdr cmd))
             (ec (apply #'call-process p nil t nil a )))
        (if (= ec 0)
            (buffer-string)
          (error (string-trim-right (buffer-string)))))))

  (defvar my:rust-crates-cache nil
    "Cache for cargo search result.")

  (defun my:rust-search-crates (keyword)
    "Search packages, matching its name to `keyword'."
    (when (> (length keyword) 0)
      (or (gethash keyword (or my:rust-crates-cache
                               (setq my:rust-crates-cache
                                     (make-hash-table :test 'equal))))
          (let ((result (seq-filter
                         (lambda (s)
                           (string-match-p (regexp-quote keyword) s))
                         (split-string
                          (my:run-command-to-string
                           (format "%s search --limit 30 %s"
                                   rust-cargo-bin keyword))
                          "[ ]+=.+[\r\n]+" t))))
            (puthash keyword result my:rust-crates-cache)))))

  (defun my:rust-add-dependency (package)
    "Add dependency to `package', using 'cargo add'."
    (interactive "P")
    (let ((chosen-package
           (or package
               (if (and (boundp 'helm-mode) helm-mode)
                   ;; Use a dynamic sync source instead
                   (helm :sources (helm-build-sync-source "Cargo Search"
                                    :candidates
                                    (lambda ()
                                      (my:rust-search-crates helm-pattern))
                                    :volatile t)
                         :buffer "*helm cargo search*"
                         :prompt "Package to add: "
                         :input-idle-delay 0.5
                         :must-match nil)
                 (completing-read
                  "Package to add: "
                  (completion-table-dynamic #'my:rust-search-crates))))))
      (when chosen-package
        (rust--compile nil "%s add %s" rust-cargo-bin chosen-package))))

  (defun my:rust-remove-dependency (package)
    "Remove dependency to `package', using 'cargo remove'."
    (interactive "P")
    (rust--compile nil "%s remove %s" rust-cargo-bin
                   (or package
                       (completing-read
                        "Package to remove: "
                        (let ((workspace
                               (car (split-string
                                     (my:run-command-to-string
                                      (format "%s tree --prefix none --depth 0"
                                              rust-cargo-bin))))))
                          (when (stringp workspace)
                            (seq-remove (lambda (s) (string= s workspace))
                                        (split-string
                                         (my:run-command-to-string
                                          (format
                                           "%s tree --prefix none --depth 1"
                                           rust-cargo-bin))
                                         " v.+[ \r\n]+" t))))
                        nil t))))

  (defvar my:rust-explain-error-buffer " *rust-explain-error*"
    "Buffer name for the `rustc --explain' documentation posframe.")
  (defvar my:rust-explain-error-list-buffer " *rust-explain-error-list*"
    "Buffer name for the multi-error picker posframe.")
  (defvar my:rust-explain-error-cache nil
    "Cache for `rustc --explain' results, keyed by error id.")

  (defvar my:rust-explain-error--state nil
    "Runtime state for the explain popup.
A plist with keys :errors :index :win :pos :list-frame :backup.")

  (defconst my:rust-explain-error--suppressed-vars
    '(lsp-ui-doc-show-with-cursor
      lsp-ui-doc-show-with-mouse
      flycheck-display-errors-function)
    "Variables disabled buffer-locally while our popup is shown.")

  (defun my:rust-explain-error--get (key)
    "Return field KEY of the explain-popup state."
    (plist-get my:rust-explain-error--state key))

  (defun my:rust-explain-error--set (key value)
    "Set field KEY of the explain-popup state to VALUE."
    (setq my:rust-explain-error--state
          (plist-put my:rust-explain-error--state key value)))

  (defun my:rust-explain-error--suppress-other-popup ()
    "Disable competing popups buffer-locally, saving originals to restore."
    (let (backup)
      (dolist (sym my:rust-explain-error--suppressed-vars)
        (when (boundp sym)
          (push (cons sym (symbol-value sym)) backup)
          (set (make-local-variable sym) nil)))
      (my:rust-explain-error--set :backup backup)))

  (defun my:rust-explain-error--restore-other-popup ()
    "Restore the popups saved by the matching suppress function."
    (dolist (entry (my:rust-explain-error--get :backup))
      (set (make-local-variable (car entry)) (cdr entry))))

  (defun my:rust-explain-error--teardown ()
    "Hide both posframes and restore the suppressed popups."
    (posframe-hide my:rust-explain-error-list-buffer)
    (posframe-hide my:rust-explain-error-buffer)
    (my:rust-explain-error--restore-other-popup)
    (remove-hook 'window-state-change-functions
                 #'my:rust-explain-error--on-window-state-change))

  (defun my:rust-explain-error--hidehandler (_info)
    "Return non-nil once the anchor window has moved point away.
INFO is the plist posframe passes to its hidehandler.  Rather than
comparing against the globally selected window/buffer (which can
transiently be one of our own child frames), this inspects the anchor
window and buffer recorded in `my:rust-explain-error--state', so the
popup stays put while focus bounces between our frames and only hides
once the user actually moves point in the source buffer."
    (let ((buf (my:rust-explain-error--get :buf))
          (win (my:rust-explain-error--get :win))
          (pos (my:rust-explain-error--get :pos)))
      (when (and (eq (selected-window) win)
                 (eq (with-selected-window (selected-window)
                       (current-buffer))
                     buf)
                 (eq (current-buffer) buf)
                 (not (eq (point) pos)))
        (my:rust-explain-error--teardown)
        t)))
  (defun my:rust-explain-error--on-window-state-change ()
    "Tear down the popup once the anchor window/buffer loses focus.
Added to `window-state-change-functions', which is a global hook that
does not pass any per-call argument here, so the anchor window and
buffer are read back from `my:rust-explain-error--state' instead."
    (let ((win (my:rust-explain-error--get :win))
          (buf (my:rust-explain-error--get :buf)))
      (unless (and (eq (selected-window) win)
                   (eq (current-buffer) buf))
        (with-current-buffer buf
          (my:rust-explain-error--teardown)))))

  (defvar my:rust-explain-error-picker-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-n") #'my:rust-explain-error-picker-next)
      (define-key map (kbd "C-p") #'my:rust-explain-error-picker-prev)
      (define-key map (kbd "C-g") #'my:rust-explain-error-picker-quit)
      map)
    "Transient keymap active while the multi-error picker is shown.")

  (defun my:rust-explain-error--fontify-code-blocks ()
    "Replace fenced code blocks with their rust-mode-fontified rendering."
    (goto-char (point-min))
    (while (re-search-forward "^```[^\n]*\n" nil t)
      (let ((block-start (match-beginning 0))
            (code-start (match-end 0)))
        (when (re-search-forward "^```[ \t]*\n?" nil t)
          (let ((code (buffer-substring-no-properties
                       code-start (match-beginning 0)))
                (block-end (match-end 0)))
            (delete-region block-start block-end)
            (goto-char block-start)
            (insert (with-temp-buffer
                      (insert code)
                      (delay-mode-hooks (rust-mode))
                      (font-lock-ensure)
                      (buffer-string))))))))

  (defun my:rust-explain-error--explain (error-id)
    "Return the rust-mode-fontified `rustc --explain' text for ERROR-ID.
Cached in `my:rust-explain-error-cache', keyed by ERROR-ID, since the
explanation only depends on the error code, not the occurrence."
    (or (gethash error-id
                 (or my:rust-explain-error-cache
                     (setq my:rust-explain-error-cache
                           (make-hash-table :test 'equal))))
        (puthash error-id
                 (with-temp-buffer
                   (insert (my:run-command-to-string
                            (format "rustc --explain %s" error-id)))
                   (my:rust-explain-error--fontify-code-blocks)
                   (buffer-string))
                 my:rust-explain-error-cache)))

  (defun my:rust-explain-error--show-posframe (buffer &rest extra)
    "Show BUFFER in a posframe using our shared styling.
EXTRA is a plist of posframe-show arguments prepended before the common
ones, so a caller can add (e.g. :position) or override them."
    (apply #'posframe-show buffer
           (append extra
                   (list :internal-border-width 10
                         :background-color
                         (face-background 'company-tooltip nil t)
                         :foreground-color
                         (face-foreground 'default nil t)
                         :hidehandler
                         #'my:rust-explain-error--hidehandler))))

  (defun my:rust-explain-error--reposition-beside (frame list-frame)
    "Move doc FRAME beside LIST-FRAME, on whichever side has more room."
    (let* ((parent (frame-parent list-frame))
           (list-pos (frame-position list-frame))
           (list-width (frame-pixel-width list-frame))
           (doc-width (frame-pixel-width frame))
           (gap (/ (frame-char-width parent) 2))
           (space-right (- (frame-native-width parent)
                           (car list-pos) list-width))
           (space-left (car list-pos)))
      (set-frame-position
       frame
       (if (>= space-right space-left)
           (+ (car list-pos) list-width gap)
         (max 0 (- (car list-pos) doc-width gap)))
       (cdr list-pos))))

  (defun my:rust-explain-error--show-doc (error pos &optional list-frame)
    "Show the explanation for flycheck ERROR anchored at POS.
With LIST-FRAME, position the doc frame beside it instead of at POS."
    (let ((error-id (flycheck-error-id error))
          (error-level (flycheck-error-level error)))
      (with-current-buffer (get-buffer-create my:rust-explain-error-buffer)
        (erase-buffer)
        (insert (my:rust-explain-error--level-icon error-level 1.2) " ")
        (insert (propertize
                 (format "[%s] %s\n\n"
                         error-id (flycheck-error-message error))
                 'face 'info-title-3))
        (insert (my:rust-explain-error--explain error-id))
        (visual-line-mode 1))
      (let ((frame (my:rust-explain-error--show-posframe
                    my:rust-explain-error-buffer :position pos)))
        (when (and frame list-frame)
          (my:rust-explain-error--reposition-beside frame list-frame)))))

  (defun my:rust-explain-error--level-icon (level &optional scale)
    "Return a display string with the icon for flycheck LEVEL."
    (let* ((name (symbol-name level))
           (file (expand-file-name
                  (format "misc/res/icons8-%s-32.png" name)
                  user-emacs-directory))
           (file (if (file-exists-p file)
                     file
                   (expand-file-name "misc/res/icons8-info-32.png"
                                     user-emacs-directory)))
           (scale (or scale 0.9)))
      (propertize " " 'display
                  (create-image file 'png nil :ascent 'center
                                :height (round
                                         (* (frame-char-height) scale))))))

  (defun my:rust-explain-error--render-list ()
    "Redraw the picker list buffer for the current selection."
    (with-current-buffer (get-buffer-create
                          my:rust-explain-error-list-buffer)
      (erase-buffer)
      (let ((index (my:rust-explain-error--get :index))
            (i 0))
        (dolist (error (my:rust-explain-error--get :errors))
          (let ((line (concat
                       (my:rust-explain-error--level-icon
                        (flycheck-error-level error))
                       (format " [%s] %s"
                               (flycheck-error-id error)
                               (flycheck-error-message error)))))
            (when (= i index)
              (setq line (propertize
                          line 'face
                          (list :background
                                (face-background
                                 'company-tooltip-selection nil t)
                                :extend t))))
            (insert line "\n"))
          (setq i (1+ i))))
      (goto-char (point-min))))

  (defun my:rust-explain-error--show-current ()
    "Show the list and doc posframes for the current selection."
    (my:rust-explain-error--render-list)
    (let* ((pos (my:rust-explain-error--get :pos))
           (errors (my:rust-explain-error--get :errors))
           (index (my:rust-explain-error--get :index))
           (list-frame (my:rust-explain-error--show-posframe
                        my:rust-explain-error-list-buffer
                        :position pos :lines-truncate t)))
      (my:rust-explain-error--set :list-frame list-frame)
      (my:rust-explain-error--show-doc (nth index errors) pos list-frame)))

  (defun my:rust-explain-error-picker-next ()
    "Select the next error in the picker."
    (interactive)
    (let ((errors (my:rust-explain-error--get :errors)))
      (my:rust-explain-error--set
       :index (mod (1+ (my:rust-explain-error--get :index))
                   (length errors))))
    (my:rust-explain-error--show-current))

  (defun my:rust-explain-error-picker-prev ()
    "Select the previous error in the picker."
    (interactive)
    (let ((errors (my:rust-explain-error--get :errors)))
      (my:rust-explain-error--set
       :index (mod (1- (my:rust-explain-error--get :index))
                   (length errors))))
    (my:rust-explain-error--show-current))

  (defun my:rust-explain-error-picker-quit ()
    "Dismiss the picker; teardown runs via the transient-map on-exit."
    (interactive))

  (defun my:rust-explain-error--show-list (errors)
    "Show a picker posframe cycling through the multiple ERRORS."
    (my:rust-explain-error--set :errors errors)
    (my:rust-explain-error--set :index 0)
    (my:rust-explain-error--show-current)
    (set-transient-map
     my:rust-explain-error-picker-map
     (lambda ()
       (memq this-command
             '(my:rust-explain-error-picker-next
               my:rust-explain-error-picker-prev)))
     #'my:rust-explain-error--teardown))

  (defun my:rust-explain-error-at-point ()
    "Explain the flycheck error(s) at point via `rustc --explain'."
    (interactive)
    (let* ((pos (point))
           (errors (seq-filter
                    (lambda (e)
                      (and (flycheck-error-id e)
                           (string-match "E[0-9]+" (flycheck-error-id e))))
                    (flycheck-overlay-errors-at pos))))
      (if (null errors)
          (message "No explainable flycheck error at point")
        (setq my:rust-explain-error--state nil)
        (let ((buf (current-buffer)))
          (my:rust-explain-error--set :buf buf)
          (my:rust-explain-error--set :win (get-buffer-window buf))
          (my:rust-explain-error--set :pos pos)
          (my:rust-explain-error--suppress-other-popup))
        ;; install hook to handle window selection, buffer burying
        (add-hook 'window-state-change-hook
                  #'my:rust-explain-error--on-window-state-change)
        (if (cdr errors)
            (my:rust-explain-error--show-list errors)
          (my:rust-explain-error--show-doc (car errors) pos)))))
  :bind
  (:map rust-mode-map
   ("C-c C-c C-a" . 'my:rust-add-dependency)
   ("C-c C-c C-d" . 'my:rust-remove-dependency)
   ("C-c C-c C-e" . 'my:rust-explain-error-at-point)))
