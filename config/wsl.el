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
          (concat my:view-file-opener " %s")
        viewer)))
  (advice-add 'mailcap-mime-info :around #'my:mailcap-mime-info)
  :after (dired))

(use-package browse-url
  :pin manual
  :config
  (defun my:browse-url-wslview (url &optional _new-window)
    "Ask the default WWW browser in Windows to load URL through wslview
in WSL. Default to the URL around or before point.
The optional argument NEW-WINDOW is not used."
    (when my:wslp
      (let ((wslview my:view-file-opener))
        (if wslview
            (call-process wslview nil (current-buffer) nil url)
          (error "Unable to find wslview")))))

  (function-put 'my:browse-url-wslview 'browse-url-browser-kind 'external)

  (defun my:browse-url-default-browser (url &rest args)
    (if my:wslp
        (my:browse-url-wslview url args)
      (browse-url-default-browser url args)))
  :custom
  (browse-url-browser-function #'my:browse-url-default-browser)
  :after (dired))
