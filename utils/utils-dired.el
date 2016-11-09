;;; utils-dired.el

(defvar my:dired-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "V")    'my:view-file-external)
    (define-key map (kbd "@ f")  'my:git-file-log)
    (define-key map (kbd "@ t")  'my:git-stage-file)
    (define-key map (kbd "@ g")  'magit-git-command)
    (define-key map (kbd "@ i")  'magit-init)
    (define-key map (kbd "@ s")  'magit-status)
    (define-key map (kbd "@ l")  'magit-log)
    map)
  "my:dired-mode keymap for dired.")

(defvar my:view-file-opener
  (eval-when-compile
    (or (executable-find "explorer.exe") (executable-find "xdg-open"))))

(defun my:view-file-external (file)
  (interactive (list (convert-standard-filename (expand-file-name (dired-file-name-at-point)))))
  (when (file-remote-p file)
    (let ((target-file (make-temp-file "view-file" nil (concat "-" (file-name-nondirectory file)))))
      (tramp-compat-copy-file file target-file t)
      (setq file target-file)))
  (and my:view-file-opener (call-process my:view-file-opener nil 0 nil file)))

(defun my:git-stage-file (file)
  (interactive
   (progn
     (unless (fboundp 'magit-get-current-branch)
       (require 'magit))
     (if (< 2 (length (remove '&optional (help-function-arglist 'magit-read-file-from-rev))))
         (list (magit-read-file-from-rev (magit-get-current-branch)
                                         "Git stage file"
                                         (magit-file-relative-name (dired-file-name-at-point)))
               current-prefix-arg)
       (list (magit-read-file-from-rev (magit-get-current-branch)
                                       (magit-file-relative-name (dired-file-name-at-point)))
             current-prefix-arg))))
  (magit-stage-file file))

(defun my:git-file-log (file &optional use-graph)
  (interactive
   (progn
     (unless (fboundp 'magit-get-current-branch)
       (require 'magit))
     (if (= 3  (length (remove '&optional (help-function-arglist 'magit-read-file-from-rev))))
         (list (magit-read-file-from-rev (magit-get-current-branch)
                                         "Git log file"
                                         (magit-file-relative-name (dired-file-name-at-point)))
               current-prefix-arg)
       (list (magit-read-file-from-rev (magit-get-current-branch)
                                       (magit-file-relative-name (dired-file-name-at-point)))
             current-prefix-arg))))
  (if (fboundp 'magit-file-log)
      (magit-file-log file use-graph)
    (magit-log-head '("--decorate") (list file))))

(define-minor-mode my:dired-mode
  "Add some keybindings into dired mode.

Key bindings:
\\{my:dired-mode-keymap}"
  nil nil my:dired-mode-keymap)
(add-hook 'dired-mode-hook 'my:dired-mode)

;; (define-minor-mode my:magit-branch-mode
;; "Show git branch if it's in git controlled directory."
;;   nil nil nil
;;   (add-to-list 'mode-line-position '(:eval
;;     (let ((branch
;;       (with-temp-buffer
;;         (if (= (process-file "git" nil t nil "rev-parse" "--abbrev-ref" "HEAD") 0)
;; 	  (unless (= (point-min) (point-max))
;; 	    (goto-char (point-min))
;; 	    (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))
;;       (if branch (format "GIT[%s]" branch)))) 'append))
;; (add-hook 'dired-after-readin-hook 'my:magit-branch-mode)

(provide 'utils-dired)
