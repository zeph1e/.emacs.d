;;; wsl.el --- WSL / WSLg helpers  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

;;; Commentary:

;; Everything that talks to the Windows host from Emacs running under WSL:
;; PowerShell scripts in misc/*.ps1, Windows monitor sizes and window focus.

;;; Code:

(require 'seq)

(defconst my:wslp
  (string-match "-[Mm]icrosoft" operating-system-release)
  "Non-nil if running on WSL.")

(defvar my:wsl-powershell-encoded-cache (make-hash-table :test #'equal)
  "Encoded misc/*.ps1 commands already built, by (SCRIPT-NAME . PRELUDE).")

(defun my:wsl-powershell-args (script-name &optional prelude)
  "Return powershell.exe arguments that run misc/SCRIPT-NAME.
PRELUDE, if non-nil, is PowerShell source put before the script.  The
encoded command is built once for each SCRIPT-NAME and PRELUDE pair."
  (list "powershell.exe" "-NoProfile" "-NonInteractive" "-EncodedCommand"
        (let ((key (cons script-name prelude)))
          (or (gethash key my:wsl-powershell-encoded-cache)
              (puthash key
                       (base64-encode-string
                        (encode-coding-string
                         (concat prelude
                                 (with-temp-buffer
                                   (insert-file-contents
                                    (expand-file-name
                                     (concat "misc/" script-name)
                                     user-emacs-directory))
                                   (buffer-string)))
                         'utf-16le)
                        t)
                       my:wsl-powershell-encoded-cache)))))

(defun my:wsl-parse-monitors (output)
  "Parse OUTPUT of misc/wsl-monitors.ps1.
Return a list of (X Y W H MM)."
  (let (monitors)
    (dolist (line (split-string output "[\r\n]+" t))
      (when (string-match
             (eval-when-compile
               (concat "\\`\\(-?[0-9]+\\) \\(-?[0-9]+\\) \\([0-9]+\\) "
                       "\\([0-9]+\\) \\([0-9]+\\)[ \t]*\\'"))
             line)
        (push (mapcar (lambda (i) (string-to-number (match-string i line)))
                      '(1 2 3 4 5))
              monitors)))
    (nreverse monitors)))

(defun my:wsl-query-monitors-async (callback)
  "Run misc/wsl-monitors.ps1 in the background and call CALLBACK.
CALLBACK gets the list of (X Y W H MM) the script printed, or nil if it
failed.  Return the process."
  (condition-case nil
      (let* ((output "")
             (proc (make-process
                    :name "wsl-monitors" :buffer nil :noquery t
                    :connection-type 'pipe
                    :command (my:wsl-powershell-args "wsl-monitors.ps1")
                    :filter (lambda (_p chunk)
                              (setq output (concat output chunk)))
                    :sentinel
                    (lambda (p _event)
                      (unless (process-live-p p)
                        (funcall callback (my:wsl-parse-monitors output)))))))
        (process-send-eof proc)
        proc)
    (error (funcall callback nil) nil)))

(defvar my:wsl-monitor-mm-cache nil
  "Alist of (GEOMETRY . MM) for the monitors found by the last query.")

(defvar my:wsl-monitor-cache-time nil
  "When `my:wsl-monitor-mm-cache' was filled, or nil if it needs a query.")

(defvar my:wsl-monitor-cache-ttl 30
  "Seconds until the monitor cache expires and is queried again.")

(defvar my:wsl-monitor-cache-updated-hook nil
  "Hook run after the monitor cache is refilled.")

(defvar my:wsl-monitor-query-process nil
  "The running PowerShell query, if any.")

(defun my:wsl-monitor-set-cache (monitors)
  "Fill the monitor cache from MONITORS, then run
`my:wsl-monitor-cache-updated-hook'.
Windows and WSLg disagree on the coordinate origin when a monitor sits above
or left of another, so monitors are translated to put their combined bounding
box at (0 . 0), as WSLg reports them."
  (setq my:wsl-monitor-query-process nil
        my:wsl-monitor-mm-cache
        (when monitors
          (let ((min-x (apply #'min (mapcar #'car monitors)))
                (min-y (apply #'min (mapcar #'cadr monitors))))
            (mapcar (lambda (m)
                      (seq-let (x y w h mm) m
                        (cons (list (- x min-x) (- y min-y) w h) mm)))
                    monitors)))
        my:wsl-monitor-cache-time (float-time))
  (run-hooks 'my:wsl-monitor-cache-updated-hook))

(defun my:wsl-monitor-mm-width (geometry)
  "Return the physical width in mm of the Windows monitor at GEOMETRY.
GEOMETRY is the (X Y W H) of an Emacs monitor.  Return nil if that monitor
is unknown, or `pending' while the PowerShell query is still running.  An
expired cache is emptied here, so the next lookup queries again without
blocking Emacs."
  (when (and my:wsl-monitor-cache-time
             (> (- (float-time) my:wsl-monitor-cache-time)
                my:wsl-monitor-cache-ttl))
    (setq my:wsl-monitor-mm-cache nil
          my:wsl-monitor-cache-time nil))
  (if my:wsl-monitor-cache-time
      (cdr (assoc geometry my:wsl-monitor-mm-cache))
    (unless (process-live-p my:wsl-monitor-query-process)
      (setq my:wsl-monitor-query-process
            (my:wsl-query-monitors-async #'my:wsl-monitor-set-cache)))
    'pending))

;; Under WSLg, real keyboard focus is decided by Windows' own window
;; manager over the RDP channel, which ignores the X11-level
;; `_NET_ACTIVE_WINDOW' request that `select-frame-set-input-focus' (and
;; so framemove) sends. Emacs's own `selected-frame' still flips, which
;; makes it look like the switch worked, but keystrokes keep going to the
;; previously focused frame. Ask Windows directly instead.
(defun my:wsl-escape-powershell-string (s)
  "Escape S for interpolation inside a double-quoted PowerShell string."
  (replace-regexp-in-string "[`$\"]" "`\\&" s))

(defun my:wsl-activate-window (title)
  "Ask Windows to give real input focus to the WSLg window titled TITLE."
  (when (and my:wslp (executable-find "powershell.exe"))
    (condition-case nil
        (let ((proc (make-process
                     :name "wsl-activate-window" :buffer nil :noquery t
                     :connection-type 'pipe
                     :command (my:wsl-powershell-args
                               "wsl-activate-window.ps1"
                               (format "$target = \"%s\"\n"
                                       (my:wsl-escape-powershell-string
                                        title))))))
          (process-send-eof proc))
      (error nil))))

(provide 'wsl)
;;; wsl.el ends here
