;; utils-24compat.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;; v23 doesn't have this one (for magit)
(unless (fboundp 'process-live-p)
  (defun process-live-p (process)
    (let ((status (process-status process)))
      (if (or (eq status 'run)
              (eq status 'open)
              (eq status 'listen)
              (eq status 'connect)
              (eq status 'stop)) t))))

;; define-error was introduced from 24.3
;; following code was from:
;;   http://emacs.stackexchange.com/questions/3905/define-error-for-older-emacs
(unless (fboundp 'define-error)
  (defun define-error (name message &optional parent)
    "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
    (unless parent (setq parent 'error))
    (let ((conditions
           (if (consp parent)
               (apply #'nconc
                      (mapcar (lambda (parent)
                                (cons parent
                                      (or (get parent 'error-conditions)
                                          (error "Unknown signal `%s'" parent))))
                              parent))
             (cons parent (get parent 'error-conditions)))))
      (put name 'error-conditions
           (delete-dups (copy-sequence (cons name conditions))))
      (when message (put name 'error-message message)))))

(when (not (fboundp 'make-variable-frame-local))
  (defun make-variable-frame-local (variable) variable))

(define-obsolete-function-alias 'string-to-int 'string-to-number "22.1")

(provide 'utils-24compat)
