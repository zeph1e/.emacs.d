;;-*- mode: emacs-lisp; -*-
(defconst wslp (string-match "-[Mm]icrosoft" operating-system-release))

(defvar wsl-browser-command
  (when wslp
    (let* ((reg (executable-find "reg.exe"))
           (coding-system-for-read (default-value 'buffer-file-coding-system))
           (user-choice-key
            (concat "HKCU\\SOFTWARE\\Microsoft\\Windows\\Shell\\Associations\\"
                    "UrlAssociations\\http\\UserChoice"))
           (command-key-format
            "HKLM\\SOFTWARE\\Classes\\%s\\shell\\open\\command")
           (prog-id
            (with-temp-buffer
              (when (= 0 (call-process reg nil (current-buffer) nil
                                       "query" user-choice-key "/v" "ProgId"))
                (goto-char (point-min))
                (when (re-search-forward "ProgId\\s-+REG_SZ\\s-+\\(.*\\)" nil t)
                  (string-trim
                   (decode-coding-string (match-string 1) 'utf-8)))))))
      (when (and prog-id (stringp prog-id))
        (let ((command-key (format command-key-format prog-id)))
          (with-temp-buffer
            (when (= 0 (call-process reg nil (current-buffer) nil
                                     "query" command-key "/ve"))
              (goto-char (point-min))
              (when (re-search-forward "REG_SZ\\s-+\\(.*\\)" nil t)
                (let* ((input-string (string-trim (decode-coding-string (match-string 1) 'utf-8)))
                       (prog-command
                         (let ((start 0)
                               (result '()))
                           (while (string-match
                                   "\"\\([^\"]+\\)\"\\|\\([^ ]+\\)"
                                   input-string start)
                             (setq start (match-end 0))
                             (push (or (match-string 1 input-string)
                                       (match-string 2 input-string))
                                   result))
                           (nreverse result))))
                  (setf (car prog-command)
                        (concat "/mnt/"
                                (downcase (substring (car prog-command) 0 1))
                                (replace-regexp-in-string
                                 "\\\\" "/" (substring (car prog-command) 2))))
                  prog-command)))))))))

(use-package browse-url
  :pin manual
  :init
  (setq browse-url-secondary-browser-function
        #'(lambda (url &rest args)
            (if wsl-browser-command
                (call-process (car wsl-browser-command) nil 0 nil
                              (mapcar (lambda (param)
                                        (if (string= "%1" param)
                                            url
                                          param))
                                      (cdr wsl-browser-command)))
            (funcall (default-value 'browse-url-secondary-browser-function)
                     url args)))))
