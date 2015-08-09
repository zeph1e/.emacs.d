;;; magit-assist.el

(when (eq system-type 'windows-nt) ; to resolve push problems in windows
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(defvar magit-assist-dired-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "@ f")  'magit-assist-dired-file-log)
    (define-key map (kbd "@ g")  'magit-git-command)
    (define-key map (kbd "@ i")  'magit-init)
    (define-key map (kbd "@ s")  'magit-status)
    (define-key map (kbd "@ l")  'magit-log)
    map)
  "Magit-assist keymap for dired.")

(define-minor-mode magit-assist-dired-mode
  "Add some keybindings into dired mode.

Key bindings:
\\{magit-assist-dired-mode-keymap}"
  nil nil magit-assist-dired-mode-keymap
  (require 'magit)
  (defun magit-assist-dired-file-log (file &optional use-graph)
    (interactive
     (if (< 2 (length (remove '&optional (help-function-arglist 'magit-read-file-from-rev))))
         (list (magit-read-file-from-rev (magit-get-current-branch)
                                         "File"
                                         (magit-file-relative-name (dired-file-name-at-point)))
               current-prefix-arg)
       (list (magit-read-file-from-rev (magit-get-current-branch)
                                       (magit-file-relative-name (dired-file-name-at-point)))
             current-prefix-arg)))
    (if (fboundp 'magit-file-log)
        (magit-file-log file use-graph)
      (magit-log-head '("--decorate") (list file)))))
(add-hook 'dired-mode-hook 'magit-assist-dired-mode)

;; (define-minor-mode magit-assist-dired-branch-mode
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
;; (add-hook 'dired-after-readin-hook 'magit-assist-dired-branch-mode)



