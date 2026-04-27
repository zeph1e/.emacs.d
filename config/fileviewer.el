;;; fileviewer.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(defconst my:wslp
  (string-match "-[Mm]icrosoft" operating-system-release)
  "Non-nil if it is on WSL.")

(defconst my:remote-host
  (when-let ((c (getenv "SSH_CLIENT")))
    (car (split-string c))))

(defun my:emacs-running-at ()
  (cond
   (my:wslp 'wsl)
   (my:remote-host 'remote)
   (t nil)))

(defconst my:view-file-viewer-candidates
  `((wsl . ,(executable-find "wslview"))
    (remote . (,(executable-find "ssh") ,my:remote-host "xdg-open"))
    (nil . ,(or (executable-find "xdg-open")
                (executable-find "gnome-open")
                (executable-find "kde-open")
                (executable-find "open"))))
  "The candidates of executable which opens file in external viewer")

(defvar my:view-file-viewer
  (let ((running-at (my:emacs-running-at)))
    (cdr (assoc running-at
                my:view-file-viewer-candidates)))
  "The executable which opens file in external viewer")


(use-package dired
  :ensure nil
  :pin manual
  :config
  ;; http://superuser.com/q/1728902
  ;; If there's an issue in opening files with wslview, it would be from a bug
  ;; in wslu, WSL Utilities. You may update wslu by:
  ;; sudo add-apt-repository ppa:wslutilities/wslu
  ;; sudo apt update
  ;; sudo apt upgrade

  (defun my:view-file-external (file)
    (interactive (list (convert-standard-filename
                        (expand-file-name (dired-file-name-at-point)))))
    (when (file-remote-p file)
      (let ((target-file
             (make-temp-file "view-file" nil
                             (concat "-" (file-name-nondirectory file)))))
        (tramp-compat-copy-file file target-file t)
        (setq file target-file)))
    (let* ((remotep (eq (my:emacs-running-at) 'remote))
           (my:view-file-viewer
            (if remotep
                (when (display-graphic-p)
                  (cdr (assoc nil my:view-file-viewer-candidates)))
              my:view-file-viewer)))
      (and my:view-file-viewer (call-process my:view-file-viewer nil 0 nil file))))
  :bind
  (:map my:global-key-map
   ("C-x C-j" . dired-jump)
   :map dired-mode-map
   ("V" . my:view-file-external)))

(use-package mailcap
  :pin manual
  :config
  ;; This is to launch external viewer programs on Windows, not in WSL.
  (defun my:mailcap-mime-info (orig-fun &rest args)
    (let ((viewer (apply orig-fun args)))
      (if (and my:wslp (stringp viewer))
          (concat my:view-file-viewer " %s")
        viewer)))
  (advice-add 'mailcap-mime-info :around #'my:mailcap-mime-info)
  :after (dired))

(use-package browse-url
  :pin manual
  :config
  (defun my:browse-url-view (url &optional _new-window)
    "Ask the default WWW browser in Windows or local host to load URL through
wslview in WSL, as well as local xdg-open when emacs is running at remote.
The optional argument NEW-WINDOW is not used."
    (let* ((args (cond
                  ((stringp my:view-file-viewer)
                    `(,my:view-file-viewer nil 0 nil ,url))
                  ((listp my:view-file-viewer)
                   `(,(car my:view-file-viewer)
                     nil 0 nil ,@(flatten-list (cdr my:view-file-viewer)) ,url))
                  (t nil))))
      (if args
          (apply #'call-process args)
        (error "Unable to find viewer"))))

  (function-put 'my:browse-url-view 'browse-url-browser-kind 'external)

  (defun my:browse-url-default-browser (url &rest args)
    (if (my:emacs-running-at)
        (apply #'my:browse-url-view url args)
      (apply #'browse-url-default-browser url args)))
  :custom
  (browse-url-browser-function #'my:browse-url-default-browser)
  :after (dired))
