;; 24compat.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(when (< emacs-major-version 24)


(defun process-live-p (process)
  (let ((status (process-status process)))
    (if (or (eq status 'run)
	    (eq status 'open)
	    (eq status 'listen)
	    (eq status 'connect)
	    (eq status 'stop)) t)))



) ; (when (< emacs-major-version 24)