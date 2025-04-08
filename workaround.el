;;-*- mode: emacs-lisp; -*-
;; To fix issue in dictionaries-common:
;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=968955
(setq ispell-menu-map-needed t)

;; This fixes failure in accessing lock of system package manager
;; when there're concurrent `async-shell-command' calls from multiple
;; `:ensure-system-package' directives.
(require 'use-package)
(require 'system-packages nil t)
(declare-function system-packages-get-command "system-packages")

(defvar my:async-shell-command-process nil)
(defvar my:async-shell-command-queue nil)

(defun my:async-shell-command (command &optional _output-buffer _error-buffer)
  (when command
    (push command my:async-shell-command-queue))
  (if my:async-shell-command-queue
      (unless my:async-shell-command-process
        (setq my:async-shell-command-process 'occupied)
        (let ((cmd (pop my:async-shell-command-queue)))
          (when cmd
            (set-process-sentinel
             (with-current-buffer (get-buffer-create
                                   (format "*Use Package: %s*" cmd))
               (async-shell-command cmd (current-buffer))
               (get-buffer-process (current-buffer)))
             (lambda (process _string)
               (setq my:async-shell-command-process nil)
               (kill-buffer (process-buffer process))
               (run-at-time
                1 nil #'my:async-shell-command nil))))))
    (setq my:async-shell-command-process nil)))

(defun my:use-package-ensure-system-package-consify (arg)
  "Turn ARG into a cons of the form (PACKAGE-NAME . INSTALL-COMMAND')."
  (cond
   ((stringp arg)
    (cons arg `(system-packages-install ,arg)))
   ((symbolp arg)
    (cons arg `(system-packages-install ,(symbol-name arg))))
   ((consp arg)
    (cond
     ((not (cdr arg))
      (use-package-ensure-system-package-consify (car arg)))
     ((stringp (cdr arg))
      (progn
        (push (cdr arg) use-package-ensure-system-package--custom-packages)
        (cons (car arg) `(my:async-shell-command ,(cdr arg)))))
     (t
      (cons (car arg)
            `(system-packages-install ,(symbol-name (cdr arg)))))))))
(advice-add 'use-package-ensure-system-package-consify
            :override #'my:use-package-ensure-system-package-consify)
