(use-package files
  :ensure nil
  :pin manual
  :init
  (defun my:display-buffer-modification (&optional buffer-or-name)
    (interactive)
    (let* ((buffer (get-buffer (or buffer-or-name (current-buffer))))
           (diff-switches "-urN")
           (file-name (and (bufferp buffer) (buffer-file-name buffer)))
           (args (list buffer diff-switches file-name)))
      (if (called-interactively-p)
          (save-window-excursion
            (apply #'my:display-buffer-modification-internal args)
            (read-char "Press any key to quit: "))
        (apply #'my:display-buffer-modification-internal args))))

  (defun my:display-buffer-modification-internal
      (buffer diff-switches file-name)
    (if (null file-name)
        (error "Visiting buffer %S doesn't have a name of file" buffer)
      (display-buffer buffer '(display-buffer-same-window))
      (delete-other-windows)
      (diff (if (file-exists-p file-name)
                file-name
              null-device)
            buffer nil 'noasync)))

  (defun my:revert-buffer (&optional buffer-or-name)
    "Refreshes this buffer from its respective file."
    (interactive)
    (let ((buffer (get-buffer (or buffer-or-name (current-buffer))))
          choice)
      (when (and (bufferp buffer)
                 (buffer-file-name buffer)
                 (file-exists-p (buffer-file-name buffer)))
        (with-current-buffer buffer
          (setq choice
                (or (unless (buffer-modified-p) ?d)
                    (save-window-excursion
                      (my:display-buffer-modification buffer)
                      (read-char-choice
                       (format
                        "Buffer %s modified; (d)iscard (s)ave or (q)uit? "
                        (buffer-name buffer))
                       '(?d ?q ?s)))))
          (cond ((equal choice ?d)
                 (revert-buffer t t t)
                 (message "Refreshed buffer: %s." (buffer-name buffer)))
                ((equal choice ?s)
                 (save-buffer)))))))

  (defun my:revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (my:revert-buffer buf)))
    (message "Refreshed all opened files."))

  (defadvice kill-buffer (around my:kill-buffer (&optional buffer-or-name))
    "Adviced kill buffer to show diff with original file to verify the changes."
    (with-current-buffer (or buffer-or-name (current-buffer))
      (if (and (buffer-live-p (current-buffer))
               (buffer-modified-p)
               (buffer-file-name))
          (save-window-excursion
            (my:display-buffer-modification (current-buffer))
            (when (yes-or-no-p
                   (format "Buffer %s modified; kill anyway? " (buffer-name)))
              (set-buffer-modified-p nil)
              ad-do-it))
        ad-do-it)))
  (ad-activate 'kill-buffer)


  ;; (defun my:recover-file (file)
  ;;   (interactive "FRecover file: ")
  ;;   (when (null file)
  ;;     (error "Visiting buffer %S doesn't have a name of file."
  ;;            (current-buffer)))
  ;;   (setq file (expand-file-name file))
  ;;   (save-window-excursion
  ;;     (let* ((file-buffer (find-buffer-visiting file))
  ;;            (file-name (let ((buffer-file-name file))
  ;;                         (make-auto-save-file-name)))
  ;;            (diff-switches "-urN"))
  ;;       (display-buffer file-buffer '(display-buffer-same-window))
  ;;       (delete-other-windows)
  ;;       (diff file-buffer file-name nil 'noasync)
  ;;       (recover-file file))))
  :bind
  (:map my:global-key-map
   ("<f5>" . my:revert-buffer)
   ("C-<f5>" . my:revert-all-buffers)))
