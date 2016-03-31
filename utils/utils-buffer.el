;;; utils-buffer.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

;; revert all buffers that are visiting a file: from http://emacswiki.org/emacs/RevertBuffer
(defun my:revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed all opened files."))

(defun my:revert-local-buffers ()
  "Refreshes only local buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (not (file-remote-p (buffer-file-name (current-buffer))))
                 (not (buffer-modified-p)))
        (revert-buffer t t t))))
  (message "Refreshed opened local files."))

(defadvice kill-buffer (around my:kill-buffer-modified (&optional buffer-or-name))
  "Adviced kill buffer to show diff with original file to verify the changes."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if (and (buffer-live-p (current-buffer))
             (buffer-modified-p)
             (buffer-file-name))
        (save-window-excursion
          (display-buffer (current-buffer) '(display-buffer-same-window))
          (delete-other-windows)
          (let ((diff-switches "-urN"))
            (diff (if (file-exists-p buffer-file-name) buffer-file-name null-device)
                  (current-buffer) nil 'noasync))
          (when (yes-or-no-p (format "Buffer %s modified; kill anyway? " (buffer-name)))
            (set-buffer-modified-p nil)
            ad-do-it))
      ad-do-it)))
(ad-activate 'kill-buffer)

;; override default save-some-buffers (from 24.5.5 files.el)
(eval-after-load 'files
  (defun save-some-buffers (&optional arg pred)
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
                            (progn
                              (display-buffer buffer '(display-buffer-same-window))
                              (delete-other-windows)
                              (let ((diff-switches "-urN"))
                                (diff (if (file-exists-p (buffer-file-name buffer))
                                          (buffer-file-name buffer) null-device)
                                      buffer nil 'noasync))
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
                       (mapconcat 'identity autosaved-buffers ", ")))))))))

(provide 'utils-buffer)
