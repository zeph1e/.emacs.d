;;-*- mode: emacs-lisp; -*-
;; To fix issue in dictionaries-common:
;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=968955
(setq ispell-menu-map-needed t)

(defun my:normalize-nil-face-attributes (orig-fun face frame &rest args)
  "Replace :foreground nil and :background nil with 'unspecified."
  (let ((plist (copy-sequence args)))
    (let ((tail plist))
      (while tail
        (when (or (eq (car tail) :foreground)
                  (eq (car tail) :background))
          (when (null (cadr tail))
            (setcar (cdr tail) 'unspecified)))
        (setq tail (cddr tail))))
    (apply orig-fun face frame plist)))

(advice-add 'set-face-attribute :around #'my:normalize-nil-face-attributes)

;; To fix issue in Emacs itself: `make-network-process' does not retry
;; the underlying `bind' syscall when it is interrupted by a signal.
;; Under WSL2 this happens often enough that a server socket (e.g. the
;; one `monet.el' opens for Claude Code IDE integration) can fail with
;; "Cannot bind server socket: Interrupted system call" even though
;; the port is free. Retry a few times before giving up.
(defun my:retry-eintr-network-process (orig-fun &rest args)
  "Retry ORIG-FUN if it fails because a syscall was interrupted (EINTR)."
  (let ((attempts 0) done result)
    (while (not done)
      (setq attempts (1+ attempts))
      (condition-case err
          (progn (setq result (apply orig-fun args))
                 (setq done t))
        (error
         (unless (and (< attempts 5)
                      (string-match-p "Interrupted system call\\'"
                                      (error-message-string err)))
           (signal (car err) (cdr err))))))
    result))

(advice-add 'make-network-process :around #'my:retry-eintr-network-process)

;; To fix issue in monet.el: `monet-start-server-function' does not
;; check whether `monet-start-server-in-directory' actually returned a
;; session before reading its port, so any failure to start the
;; server (e.g. the EINTR case above exhausting its retries) crashes
;; with a confusing "wrong-type-argument monet--session nil" instead
;; of a clear error.
(defun my:monet-signal-clear-error-on-nil-session (orig-fun key directory)
  "Signal a clear `user-error' if ORIG-FUN's session creation failed.
Without this, a failed session shows up as a confusing
\"wrong-type-argument monet--session nil\" instead."
  (condition-case _err
      (funcall orig-fun key directory)
    (wrong-type-argument
     (user-error "Failed to start monet server in %s; see *Messages* for details" directory))))

(advice-add 'monet-start-server-function :around #'my:monet-signal-clear-error-on-nil-session)
