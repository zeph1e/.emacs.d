;;; files.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package files
  :ensure nil
  :pin manual
  :config
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
      ;; If the directory containing the file is removed already, diff will
      ;; grumble about it.
      (let ((default-directory (if (file-exists-p
                                    (file-name-directory file-name))
                                   (file-name-directory file-name)
                                 "/")))
        (diff (if (file-exists-p file-name)
                  file-name
                null-device)
              buffer nil 'noasync))))

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

  (defun my:kill-buffer (orig-fun &rest args)
    "Advised kill buffer to show diff with original file to verify the changes."
    (let* ((buffer-or-name (car args))  ; original argument
           (buffer (get-buffer (or buffer-or-name (current-buffer)))))
      (if (and (called-interactively-p)
               (buffer-live-p buffer)
               (buffer-modified-p buffer)
               (buffer-file-name buffer))
          (with-current-buffer buffer
            (save-window-excursion
              (my:display-buffer-modification)
              (apply orig-fun args)))
          (apply orig-fun args))))
  (advice-add 'kill-buffer :around #'my:kill-buffer)

  (defun my:recover-file (orig-fun &rest args)
    "Advised recover-file to diff buffer with auto-save file."
    (interactive "FRecover file: ")
    (cl-letf* ((file (expand-file-name (car args)))  ; original argument
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
        (apply orig-fun args))))
  (advice-add 'recover-file :around #'my:recover-file)

  (defun my:save-some-buffers-around (orig-fun &rest args)
    "Advised save-some-buffers to show what is changed in the buffers."
    (cl-letf* ((arg (car args)) (pred (cdr args))  ; original arguments
               (orig-map-y-or-n-p (symbol-function 'map-y-or-n-p))
               ((symbol-function 'map-y-or-n-p)
                (lambda (prompter actor list &optional help action-alist
                                  no-cursor-in-echo-area)
                  (funcall orig-map-y-or-n-p
                           (lambda (buffer)
                             (my:display-buffer-modification buffer)
                             (funcall prompter buffer))
                           actor list help
                           `((?k ,(lambda (buf)
                                    (if (null (buffer-file-name buf))
                                        (message "Not applicable: no file")
                                      (with-current-buffer buf
                                        (revert-buffer t t))))
                                 ,(purecopy
                                   "revert changes in this buffer")))
                           no-cursor-in-echo-area))))
      (apply orig-fun args)))
  (advice-add 'save-some-buffers :around #'my:save-some-buffers-around)

  :bind
  (:map my:global-key-map
   ("<f5>" . revert-buffer)
   ("C-<f5>" . my:revert-all-buffers))
  :custom
  (revert-buffer-function #'my:revert-buffer))
