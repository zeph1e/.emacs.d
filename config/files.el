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

  (defun my:revert-buffer (ignore-auto noconfirm)
    "My custom function for revert-buffer which shows the differences.
Runs revert-buffer--default internally"
    (cl-letf (((symbol-function 'yes-or-no-p)
               #'(lambda (prompt)
                   (save-window-excursion
                     (my:display-buffer-modification (current-buffer))
                     (let* ((buffer (current-buffer))
                           (choice
                            (read-char-choice
                             (format
                              "Buffer %s modified; (d)iscard (s)ave or (q)uit? "
                              (buffer-name buffer))
                             '(?d ?q ?s))))
                       (cond ((equal choice ?d)
                              (message "Refreshed buffer: %s."
                                       (buffer-name buffer)) t)
                             ((equal choice ?s) (save-buffer) nil)))))))
      (apply #'revert-buffer--default `(,ignore-auto ,noconfirm))))

  (defun my:revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (revert-buffer buf)))
    (message "Refreshed all opened files."))

  (defadvice kill-buffer (around my:kill-buffer (&optional buffer-or-name))
    "Advised kill buffer to show diff with original file to verify the changes."
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

  (defadvice recover-file (around my:recover-file (file))
    "Advised recover-file to diff buffer with auto-save file."
    (interactive "FRecover file: ")
    (cl-letf* ((file (expand-file-name file))
               (file-buffer (find-buffer-visiting file))
               (file-name (let ((buffer-filen-name file))
                            (make-auto-save-file-name)))
               (diff-switches "-urN")
               ((symbol-function 'read-answer)
                #'(lambda (question answer)
                    (display-buffer file-buffer '(display-buffer-same-window))
                    (delete-other-windows)
                    (diff file-buffer file-name nil 'noasync)
                    (if (y-or-n-p question) "yes" "no"))))
      (save-window-excursion
        ad-do-it)))
  (ad-activate 'recover-file)

  (defadvice save-some-buffers
      (around my:save-some-buffers (&optional arg pred))
    "Advised save-some-buffers to show what is changed in the buffers."
    (cl-letf* ((orig-map-y-or-n-p (symbol-function 'map-y-or-n-p))
               ((symbol-function 'map-y-or-n-p)
                #'(lambda (prompter actor list &optional help action-alist
                                    no-cursor-in-echo-area)
                    (funcall orig-map-y-or-n-p
                             #'(lambda (buffer)
                                 (my:display-buffer-modification buffer)
                                 (funcall prompter buffer))
                             actor list help
                             `((?k ,(lambda (buf)
                                      (if (null (buffer-file-name buf))
                                          (message "Not applicable: no file")
                                        (with-current-buffer buf
                                          (revert-buffer))))
                                   ,(purecopy "revert changes in this buffer")))
                             no-cursor-in-echo-area))))
      ad-do-it))
  (ad-activate 'save-some-buffers)

  :bind
  (:map my:global-key-map
   ("<f5>" . revert-buffer)
   ("C-<f5>" . my:revert-all-buffers))
  :custom
  (revert-buffer-function #'my:revert-buffer))
