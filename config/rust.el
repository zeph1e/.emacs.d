;;; rust.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package rust-mode
  :pin melpa
  :config
  (defun my:run-command-to-string (command)
    "Run shell `command' and return output on success. Otherwise error."
    (with-temp-buffer
      (let* ((cmd (split-string-shell-command command))
             (p (car cmd))
             (a (cdr cmd))
             (ec (apply #'call-process p nil t nil a )))
        (if (= ec 0)
            (buffer-string)
          (error (string-trim-right (buffer-string)))))))

  (defvar my:rust-search-cache nil
    "Cache for cargo search result.")

  (defun my:rust-search (keyword)
    "Search packages, matching its name to `keyword'."
    (when (> (length keyword) 0)
      (or (gethash keyword (or my:rust-search-cache
                               (setq my:rust-search-cache
                                     (make-hash-table :test 'equal
                                                      :size 100))))
          (let ((result (seq-filter
                         (lambda (s)
                           (string-match-p (regexp-quote keyword) s))
                         (split-string
                          (my:run-command-to-string
                           (format "%s search --limit 30 %s"
                                   rust-cargo-bin keyword))
                          "[ ]+=.+[\r\n]+" t))))
            (puthash keyword result my:rust-search-cache)))))

  (defun my:rust-add-dependency (package)
    "Add dependency to `package', using 'cargo add'."
    (interactive "P")
    (let ((chosen-package
           (or package
               (if (and (boundp 'helm-mode) helm-mode)
                   ;; Use a dynamic sync source instead
                   (helm :sources (helm-build-sync-source "Cargo Search"
                                    :candidates (lambda ()
                                                  (my:rust-search helm-pattern))
                                    :volatile t)
                         :buffer "*helm cargo search*"
                         :prompt "Package to add: "
                         :input-idle-delay 0.5
                         :must-match nil)
                 (completing-read
                  "Package to add: "
                  (completion-table-dynamic #'my:rust-search))))))
      (when chosen-package
        (rust--compile nil "%s add %s" rust-cargo-bin chosen-package))))

  (defun my:rust-remove-dependency (package)
    "Remove dependency to `package', using 'cargo remove'."
    (interactive "P")
    (rust--compile nil "%s remove %s" rust-cargo-bin
                   (or package
                       (completing-read
                        "Package to remove: "
                        (let ((workspace
                               (car (split-string
                                     (my:run-command-to-string
                                      (format "%s tree --prefix none --depth 0"
                                              rust-cargo-bin))))))
                          (when (stringp workspace)
                            (seq-remove (lambda (s) (string= s workspace))
                                        (split-string
                                         (my:run-command-to-string
                                          (format
                                           "%s tree --prefix none --depth 1"
                                           rust-cargo-bin))
                                         " v.+[ \r\n]+" t))))
                        nil t))))
  :bind
  (:map rust-mode-map
   ("C-c C-c C-a" . 'my:rust-add-dependency)
   ("C-c C-c C-d" . 'my:rust-remove-dependency)))
