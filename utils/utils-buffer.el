;;; utils-buffer.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defconst display-buffer-custom-actions '(
    ("*Buffer List*" . (display-buffer-same-window))
    ("\\`\\*helm.*\\*\\'"
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4))
    ) "display-buffer custom actions.")

(dolist (action display-buffer-custom-actions)
  (add-to-list 'display-buffer-alist action nil))

(defun my:read-eol-type ()
  (let* ((os (cond
              ((eq system-type 'ms-dos) "dos")
              ((eq system-type 'windows-nt) "dos")
              ((eq system-type 'darwin) "mac")
              (t "unix")))
         (cur-eol (or (with-current-buffer (current-buffer)
                        (let ((cs (symbol-name buffer-file-coding-system)))
                          (when (string-match "\\(.+\\)-\\(unix\\|dos\\|mac\\)\\'" cs)
                            (replace-match "\\2" t nil cs))))
                      os))
         (readfunc (if (and (boundp 'ido-mode) ido-mode)
                       #'ido-completing-read
                     #'completing-read)))
    (values (apply readfunc `(,(format "EOL Type (current: %s): " cur-eol) ("unix" "dos" "mac") nil t
                              "" nil ,os)))))

(defun cheol (eol-type)
  "Change buffer's EOL to preferred one in given OS"
  (interactive (my:read-eol-type))
  (let ((cs (format "%s-%s" (with-current-buffer (current-buffer)
                              (let ((cs (symbol-name buffer-file-coding-system)))
                                (if (string-match "\\(.+\\)\\(-unix\\|-dos\\|-mac\\)\\'" cs)
                                    (replace-match "\\1" t nil cs) cs)))
                    eol-type)))
    (unless (member (list cs) coding-system-alist)
      (error "Unsupported coding system: %S" cs))
    (setq buffer-file-coding-system (intern cs))
    (set-buffer-modified-p t)))

(defun my:revert-this-buffer (&optional buffer-or-name)
  "Refreshes this buffer from its respective file."
  (interactive)
  (let ((buffer (get-buffer (or buffer-or-name (current-buffer)))))
    (when (and (bufferp buffer)
               (buffer-file-name buffer)
               (file-exists-p (buffer-file-name buffer)))
      (with-current-buffer buffer
        (when (or (not (buffer-modified-p))
                  (save-window-excursion
                    (my:display-buffer-modification buffer)
                    (yes-or-no-p (format "Buffer %s modified; discard the changes? "
                                         (buffer-name buffer)))))
          (revert-buffer t t t)
          (message "Refreshed buffer: %s." (buffer-name buffer)))))))

(defun my:revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (my:revert-this-buffer buf)))
  (message "Refreshed all opened files."))

(defun my:display-buffer-modification (&optional buffer-or-name)
  (let* ((buffer (get-buffer (or buffer-or-name (current-buffer))))
         (diff-switches "-urN")
         (file-name (and (bufferp buffer) (buffer-file-name buffer))))
    (display-buffer buffer '(display-buffer-same-window))
    (delete-other-windows)
    (diff (if (file-exists-p file-name) file-name null-device) buffer nil 'noasync)))

(defadvice kill-buffer (around my:kill-buffer-modified (&optional buffer-or-name))
  "Adviced kill buffer to show diff with original file to verify the changes."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if (and (buffer-live-p (current-buffer))
             (buffer-modified-p)
             (buffer-file-name))
        (save-window-excursion
          (my:display-buffer-modification (current-buffer))
          (when (yes-or-no-p (format "Buffer %s modified; kill anyway? " (buffer-name)))
            (set-buffer-modified-p nil)
            ad-do-it))
      ad-do-it)))
(ad-activate 'kill-buffer)

;; override default save-some-buffers (from 24.5.5 files.el)
(defun my:save-some-buffers (&optional arg pred)
  "Save some modified file-visiting buffers.  Asks user about each one.
You can answer `y' to save, `n' not to save, `C-r' to look at the
buffer in question with `view-buffer' before deciding or `d' to
view the differences using `diff-buffer-with-file'.

This command first saves any buffers where `buffer-save-without-query' is
non-nil, without asking.

Optional argument (the prefix) non-nil means save all with no questions.
Optional second argument PRED determines which buffers are considered:
If PRED is nil, all the file-visiting buffers are considered.
If PRED is t, then certain non-file buffers will also be considered.
If PRED is a zero-argument function, it indicates for each buffer whether
to consider it or not when called with that buffer current.

See `save-some-buffers-action-alist' if you want to
change the additional actions you can take on files."
  (interactive "P")
  (save-window-excursion
    (let* (queried autosaved-buffers
                   files-done abbrevs-done)
      (dolist (buffer (buffer-list))
        ;; First save any buffers that we're supposed to save unconditionally.
        ;; That way the following code won't ask about them.
        (with-current-buffer buffer
          (when (and buffer-save-without-query (buffer-modified-p))
            (push (buffer-name) autosaved-buffers)
            (save-buffer))))
      ;; Ask about those buffers that merit it,
      ;; and record the number thus saved.
      (setq files-done
            (map-y-or-n-p
             (lambda (buffer)
               ;; Note that killing some buffers may kill others via
               ;; hooks (e.g. Rmail and its viewing buffer).
               (and (buffer-live-p buffer)
                    (buffer-modified-p buffer)
                    (not (buffer-base-buffer buffer))
                    (or
                     (buffer-file-name buffer)
                     (and pred
                          (progn
                            (set-buffer buffer)
                            (and buffer-offer-save (> (buffer-size) 0)))))
                    (or (not (functionp pred))
                        (with-current-buffer buffer (funcall pred)))
                    (if arg
                        t
                      (setq queried t)
                      (if (buffer-file-name buffer)
                          (progn (my:display-buffer-modification buffer)
                                 (format "Save file %s? "
                                         (buffer-file-name buffer)))
                        (format "Save buffer %s? "
                                (buffer-name buffer))))))
             (lambda (buffer)
               (with-current-buffer buffer
                 (save-buffer)))
             (buffer-list)
             '("buffer" "buffers" "save")
             save-some-buffers-action-alist))
      ;; Maybe to save abbrevs, and record whether
      ;; we either saved them or asked to.
      (and save-abbrevs abbrevs-changed
           (progn
             (if (or arg
                     (eq save-abbrevs 'silently)
                     (y-or-n-p (format "Save abbrevs in %s? " abbrev-file-name)))
                 (write-abbrev-file nil))
             ;; Don't keep bothering user if he says no.
             (setq abbrevs-changed nil)
             (setq abbrevs-done t)))
      (or queried (> files-done 0) abbrevs-done
          (cond
           ((null autosaved-buffers)
            (message "(No files need saving)"))
           ((= (length autosaved-buffers) 1)
            (message "(Saved %s)" (car autosaved-buffers)))
           (t
            (message "(Saved %d files: %s)"
                     (length autosaved-buffers)
                     (mapconcat 'identity autosaved-buffers ", "))))))))

;; to override recover-file (from 24.5.5 files.el)
(defun my:recover-file (file)
  "Visit file FILE, but get contents from its last auto-save file."
  ;; Actually putting the file name in the minibuffer should be used
  ;; only rarely.
  ;; Not just because users often use the default.
  (interactive "FRecover file: ")
  (setq file (expand-file-name file))
  (if (auto-save-file-name-p (file-name-nondirectory file))
      (error "%s is an auto-save file" (abbreviate-file-name file)))
  (let ((file-name (let ((buffer-file-name file))
                     (make-auto-save-file-name))))
    (cond ((if (file-exists-p file)
               (not (file-newer-than-file-p file-name file))
             (not (file-exists-p file-name)))
           (error "Auto-save file %s not current"
                  (abbreviate-file-name file-name)))
          ((save-window-excursion
             (with-temp-buffer-window
              "*Directory*" nil
              #'(lambda (window _value)
                  (with-selected-window window
                    (unwind-protect
                        (yes-or-no-p (format "Recover auto save file %s? " file-name))
                      (when (window-live-p window)
                        (quit-restore-window window 'kill)))))
              (let* ((file-buffer (find-buffer-visiting file))
                     (diff-switches "-urN"))
                (display-buffer file-buffer '(display-buffer-same-window))
                (delete-other-windows)
                (diff file-buffer file-name nil 'noasync)
                (with-current-buffer standard-output
                  (let ((switches dired-listing-switches))
                    (if (file-symlink-p file)
                        (setq switches (concat switches " -L")))
                    ;; Use insert-directory-safely, not insert-directory,
                    ;; because these files might not exist.  In particular,
                    ;; FILE might not exist if the auto-save file was for
                    ;; a buffer that didn't visit a file, such as "*mail*".
                    ;; The code in v20.x called `ls' directly, so we need
                    ;; to emulate what `ls' did in that case.
                    (insert-directory-safely file switches)
                    (insert-directory-safely file-name switches)))
                (fit-window-to-buffer
                 (select-window (display-buffer standard-output '(display-buffer-at-bottom)))))))
           (switch-to-buffer (find-file-noselect file t))
           (let ((inhibit-read-only t)
                 ;; Keep the current buffer-file-coding-system.
                 (coding-system buffer-file-coding-system)
                 ;; Auto-saved file should be read with special coding.
                 (coding-system-for-read 'auto-save-coding))
             (erase-buffer)
             (insert-file-contents file-name nil)
             (set-buffer-file-coding-system coding-system))
           (after-find-file nil nil t))
          (t (user-error "Recover-file canceled")))))

(eval-after-load 'files
  `(progn
     (defalias 'save-some-buffers 'my:save-some-buffers)
     (defalias 'recover-file 'my:recover-file)))

(provide 'utils-buffer)
