;;; git.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package transient ; to keep compatibility to magit
  :pin melpa)

(use-package magit
  :pin melpa
  :bind
  (:map my:global-key-map
   ("C-x RET C-s" . magit)
   ("C-x RET C-h" . magit-log-head)
   ("C-x RET C-b" . magit-blame)
   ("C-x RET C-f" . magit-find-file)
   ("C-x RET C-l" . magit-log-buffer-file))
  :config
  (defun my:magit-upstream-status-bracket (status hint)
    "Append ` [TEXT]' to STATUS, prompting with HINT; empty keeps STATUS."
    (let ((text (string-trim (read-string (format "%s [%s]: " status hint)))))
      (if (string-empty-p text) status (format "%s [%s]" status text))))

  (defun my:magit-format-patch-read-upstream-status (_prompt initial-input
                                                             history)
    "Read a Yocto Upstream-Status value, with an optional [..] detail."
    (let ((status (magit-completing-read
                   "Upstream-Status"
                   '("Pending" "Submitted" "Backport" "Denied"
                     "Inactive-Upstream" "Inappropriate")
                   nil t initial-input history)))
      (pcase status
        ("Submitted"
         (my:magit-upstream-status-bracket status "where submitted"))
        ((or "Backport" "Inactive-Upstream" "Inappropriate")
         (my:magit-upstream-status-bracket status "detail"))
        (_ status))))

  (defun my:magit-format-patch-read-cve (&rest _)
    "Read space-separated CVE ids, completing bare YYYY-NNNN to CVE-."
    (let (ids)
      (while (null ids)
        (let ((toks (split-string (read-string "CVE id(s): ") nil t)))
          (if (and toks
                   (seq-every-p
                    (lambda (tok)
                      (string-match-p
                       "\\`\\(?:CVE-\\)?[0-9]\\{4\\}-[0-9]\\{4,\\}\\'" tok))
                    toks))
              (setq ids (mapconcat
                         (lambda (tok)
                           (if (string-prefix-p "CVE-" tok)
                               tok
                             (concat "CVE-" tok)))
                         toks " "))
            (message "Invalid CVE id; expected CVE-YYYY-NNNN")
            (sit-for 1))))
      ids))

  (defun my:magit-patch-chomp-filename (stem limit)
    "Chomp STEM to around LIMIT bytes at a `-' boundary, like format-patch."
    (if (<= (length stem) limit)
        stem
      (let ((cut (substring stem 0 limit)))
        (if (string-match "\\`\\(.+\\)-[^-]*\\'" cut)
            (match-string 1 cut)
          cut))))

  (transient-define-argument my:magit-format-patch:--upstream-status ()
    :description "Upstream-Status"
    :class 'transient-option
    :key "=U"
    :argument "--trailer=Upstream-Status: "
    :reader #'my:magit-format-patch-read-upstream-status)

  (transient-define-argument my:magit-format-patch:--cve ()
    :description "CVE"
    :class 'transient-option
    :key "=C"
    :argument "--trailer=CVE: "
    :reader #'my:magit-format-patch-read-cve)

  (defun my:magit-patch-write-trailered (range args files trailers)
    "Write one patch for RANGE with TRAILERS inserted, no temp file."
    (let* ((topdir (magit-toplevel))
           (dir (if-let ((d (transient-arg-value "--output-directory=" args)))
                    (expand-file-name d topdir)
                  topdir))
           (limit (string-to-number
                   (or (magit-get "format.filenameMaxLength") "64")))
           (stem (my:magit-patch-chomp-filename
                  (magit-git-string "log" "-1" "--pretty=format:%f" range)
                  limit))
           (reroll (transient-arg-value "--reroll-count=" args))
           ;; --stdout is exclusive with -o/cover-letter; we place the file.
           (clean (seq-remove
                   (lambda (a)
                     (and (stringp a)
                          (or (string-prefix-p "--trailer=" a)
                              (string-prefix-p "--output-directory=" a)
                              (equal a "--cover-letter"))))
                   args))
           (file (expand-file-name
                  (format "%s0001-%s.patch"
                          (if reroll (format "v%s-" reroll) "") stem)
                  dir)))
      (with-temp-buffer
        (let ((default-directory topdir))
          (apply #'magit-git-insert "format-patch" "--stdout"
                 (append clean (list range "--") files))
          (apply #'call-process-region (point-min) (point-max)
                 magit-git-executable t t nil "interpret-trailers"
                 (mapcan (lambda (v) (list "--trailer" v)) trailers))
          (write-region (point-min) (point-max) file)))
      (message "Wrote %s" (abbreviate-file-name file))))

  (defun my:magit-patch-create-with-trailers (fn range args files)
    "`magit-patch-create' advice: create one patch with Yocto trailers."
    ;; =U/=C add `--trailer=' pseudo-args that format-patch cannot take; strip
    ;; them and apply via `git interpret-trailers' on a single patch.
    (let ((trailers (seq-keep
                     (lambda (a)
                       (and (stringp a)
                            (string-prefix-p "--trailer=" a)
                            (substring a 10)))
                     args)))
      (if (or (not range) (null trailers))
          (funcall fn range args files)
        (let ((n (length (magit-git-lines "rev-list" range "--" files))))
          (unless (= n 1)
            (user-error
             "Yocto trailers apply to a single patch; RANGE has %d commits" n))
          (my:magit-patch-write-trailered range args files trailers)))))

  (advice-add 'magit-patch-create :around
              #'my:magit-patch-create-with-trailers)

  (with-eval-after-load 'magit-patch
    (unless (ignore-errors
              (transient-get-suffix 'magit-patch-create "=U"))
      (transient-insert-suffix 'magit-patch-create '(-1)
        ["Yocto arguments"
         (my:magit-format-patch:--upstream-status)
         (my:magit-format-patch:--cve)
         ("-s" "Sign off" "--signoff")])))
  :hook
  (text-mode . (lambda ()
                 (let ((file-name (buffer-file-name)))
                   (when file-name
                     (when (string-match ".+\\(.git/COMMIT_EDITMSG\\)\\'"
                                         file-name)
                       (setq-local fill-column 70)
                       (display-fill-column-indicator-mode 1)))))))

(use-package magit-gh
  :after magit)
