(defconst my:wslp
  (string-match "-[Mm]icrosoft" operating-system-release)
  "Non-nil if it is on WSL.")

(use-package mailcap
  :pin manual
  :config
  ;; This is to launch external viewer programs on Windows, not in WSL.
  (defun my:mailcap-mime-info (orig-fun &rest args)
    (let ((viewer (apply orig-fun args)))
      (if (and my:wslp (stringp viewer))
          (concat (executable-find "wslview") " %s")
        viewer)))
  (advice-add 'mailcap-mime-info :around #'my:mailcap-mime-info))

(use-package browse-url
  :pin manual
  :init
  ;; This is to launch URLs in a web browser on Windows, not in WSL, even if
  ;; there's a installed web browser in WSL.
  (defvar my:wsl-browser-command
    (when my:wslp
      (let* ((reg (executable-find "reg.exe"))
             (coding-system-for-read (default-value 'buffer-file-coding-system))
             (user-choice-key
              (concat "HKCU\\SOFTWARE\\Microsoft\\Windows\\Shell\\"
                      "Associations\\UrlAssociations\\http\\UserChoice"))
             (command-key-format
              "HKLM\\SOFTWARE\\Classes\\%s\\shell\\open\\command")
             (prog-id
              (with-temp-buffer
                ;; This queries the program ID of default web browser to open
                ;; a url in http. `prog-id' may contains "ChromeHTML" for
                ;; Google Chrome as well as "microsoft-edge" for Microsoft Edge.
                ;; I have not tested for Firefox unfortunately.
                (when (= 0 (call-process reg nil (current-buffer) nil
                                         "query" user-choice-key "/v" "ProgId"))
                  (goto-char (point-min))
                  (when (re-search-forward "ProgId\\s-+REG_SZ\\s-+\\(.*\\)"
                                           nil t)
                    (string-trim
                     (decode-coding-string (match-string 1) 'utf-8)))))))
        (when (and prog-id (stringp prog-id))
          ;; This queries the program path and the list of arguments to pass.
          ;; Since it is in Windows style path, we need to convert it to one
          ;; can be accessed in unix-like path.
          (let ((command-key (format command-key-format prog-id)))
            (with-temp-buffer
              (when (= 0 (call-process reg nil (current-buffer) nil
                                       "query" command-key "/ve"))
                (goto-char (point-min))
                (when (re-search-forward "REG_SZ\\s-+\\(.*\\)" nil t)
                  (let* ((input-string (string-trim
                                        (decode-coding-string
                                         (match-string 1) 'utf-8)))
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
                                   "\\\\" "/"
                                   (substring (car prog-command) 2))))
                    prog-command)))))))))

  (setq browse-url-browser-function
        #'(lambda (url &rest args)
            (if my:wsl-browser-command
                (apply #'call-process
                       (cons (car my:wsl-browser-command)
                             (append '(nil 0 nil)
                                     (mapcar (lambda (param)
                                               (if (string= "%1" param)
                                                   url
                                                 param))
                                             (cdr my:wsl-browser-command)))))
                       (funcall (default-value 'browse-url-browser-function)
                                url args)))))
