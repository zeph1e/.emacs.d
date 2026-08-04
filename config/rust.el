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

  (defun my:rust-add-dependency (package)
    "Add dependency to `package', using 'cargo add'."
    (interactive "P")
    (rust--compile nil "%s add %s" rust-cargo-bin
                   (or package
                       (read-string "Package to add: "))))
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
