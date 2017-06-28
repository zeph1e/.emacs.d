;;; utils-editor.el -- modify editor behavior

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defun my:scroll-up-command (&optional arg)
  "Modify scroll-up behavior to make it move to the end of buffer."
  (interactive "P")
  (if (eq (point)(point-max))
      (signal 'end-of-buffer '())
    (condition-case e
        (scroll-up-command arg)
      (end-of-buffer (goto-char (point-max))))))

(defun my:scroll-down-command (&optional arg)
  "Modify scroll-down behaviour to make it move to the beginning of buffer."
  (interactive "P")
  (if (eq (point)(point-min))
      (signal 'beginning-of-buffer '())
    (condition-case e
        (scroll-down-command arg)
      (beginning-of-buffer (goto-char (point-min))))))

;; idea from https://www.emacswiki.org/emacs/MarkCommands
(defun my:mark-word (&optional arg allow-extend)
  "Mark word at point."
  (interactive "p\np")
  (my:mark-thing 'word arg allow-extend))

(defun my:mark-symbol (&optional arg allow-extend)
  "Mark symbol at point."
  (interactive "p\np")
  (my:mark-thing 'symbol arg allow-extend))

(defun my:mark-thing (thing arg allow-extend)
  "Mark things."
  (setq arg (if arg arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (region-active-p)))
      (set-mark
       (save-excursion
         (when (< (mark) (point))
           (setq arg (- arg)))
         (goto-char (mark))
         (forward-word arg)
         (point)))
    (let ((bounds (bounds-of-thing-at-point thing)))
      (unless (consp bounds)
        (error "No %s at point" (symbol-name thing)))
      (if (>= arg 0)
          (goto-char (car bounds))
        (goto-char (cdr bounds)))
      (push-mark (save-excursion
                   (funcall (intern (format "forward-%s" thing)) arg)
                   (point)))
      (activate-mark))))

;; kill heading spaces on kill-line : from http://emacswiki.org/emacs/DeletingWhitespace
(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

;; split horizontally first, from http://www.emacswiki.org/emacs/HorizontalSplitting
(defun my:split-window-prefer-horizonally (window)
  "If there's only one window (excluding any possibly active
minibuffer), then split the current window horizontally."
  (if (and (one-window-p t)
           (not (active-minibuffer-window)))
      (let ((split-height-threshold nil))
        (split-window-sensibly window))
    (split-window-sensibly window)))
(setq split-window-preferred-function 'my:split-window-prefer-horizonally)

(require 'whitespace)
(set-face-attribute 'whitespace-line nil :foreground nil :background "maroon4")
(set-face-attribute 'whitespace-tab nil :foreground "gray50" :background "dark slate gray")

(define-minor-mode my:whitespace-mode
  "Setup Whitespace mode for each major modes"
  :variable my:whitespace-mode
  (hack-local-variables)
  (if my:whitespace-mode
      (progn
        (cond ((derived-mode-p 'prog-mode)
               (setq-local whitespace-line-column fill-column)
               (setq-local whitespace-style
                           '(face trailing lines-tail tabs tab-mark)))
              ((derived-mode-p 'text-mode)
               (setq-local whitespace-line-column fill-column)
               (setq-local whitespace-style
                           '(face trailing tabs tab-mark)))
              (t
               (setq-local whitespace-line-column nil)
               (setq-local whitespace-style '(face trailing))))
        (whitespace-mode t))
    (whitespace-mode -1)))

;; Disable fci-mode if the window is smaller than fill-column
(defvar my:fci-mode-suppressed nil)
(make-local-variable 'my:fci-mode-suppressed)

(defvar my:fci-selected-frame nil
  "Selected frame to trace changes in frame selection.")

(defvar my:fci-selected-window nil
  "Selected window to trace changes in window selection.")

(defun my:suppress-fci-mode-in-narrow-window ()
  (let* ((window (selected-window))
         (width (window-body-width window)))
    (with-current-buffer (window-buffer window)
      (if (and (boundp fci-mode)
               fci-mode)
          (when (and (null fci-handle-truncate-lines)
                     (<= width (1+ fill-column)))
            (setq my:fci-mode-suppressed t)
            (turn-off-fci-mode))
        (when my:fci-mode-suppressed
          (setq my:fci-mode-suppressed nil)
          (turn-on-fci-mode))))))

;; Setting fci-mode variables
(with-eval-after-load 'fill-column-indicator
  (setq fci-handle-truncate-lines nil)
  (setq fci-rule-color "maroon4")
  (setq fci-rule-column nil)
  (setq fci-rule-width 1)
  (add-hook 'window-configuration-change-hook
            'my:suppress-fci-mode-in-narrow-window))

;; share clipboard even in terminal
;; http://blog.binchen.org/posts/copypaste-in-emacs.html
(defvar my:clipboard-copy-cmd nil
  "Clipboard integration command to share system clipboard in CUI-mode.")

(defvar my:clipboard-paste-cmd nil
  "Clipboard integration command to share system clipboard in CUI-mode.")

(defconst my:clipboard-copy-cmd-candidates
  '((darwin . "pbcopy")
    (cygwin . "putclip")
    (gnu/linux . ("xsel" . "-ib")))
  "Candidate commands to copy region to system-wide clipboard in CUI-mode.")

(defconst my:clipboard-paste-cmd-candidates
  '((darwin . "pbpaste")
    (cygwin . "getclip")
    (gnu/linux . ("xsel". "-ob")))
  "Candidate command to paste text from system-wide clipboard in CUI-mode.")

(defun my:clipboard--get-cmd-internal (candidate-alist)
  (let ((candidate (assoc system-type candidate-alist))
        executable
        args)
    (unless candidate
      (error (format "Unsupported system-type %s" system-type)))
    (cond ((listp (cdr candidate))
           (setq executable (cadr candidate))
           (setq args (cddr candidate)))
          ((stringp (cdr candidate))
           (setq executable (cdr candidate)))
          (t (error (format "Unrecognized command : %s"
                            (cdr candidate)))))
    (unless (executable-find executable)
      (error (format "Unable to find an executable : %s"
                     executable)))
    (cdr candidate)))

(defun my:clipboard-get-cmd (type)
  "Return clipboard copy & paste command."
  (cond ((eq type 'copy)
         (or my:clipboard-copy-cmd
             (setq my:clipboard-copy-cmd
                   (my:clipboard--get-cmd-internal
                    my:clipboard-copy-cmd-candidates))))
        ((eq type 'paste)
         (or my:clipboard-paste-cmd
             (setq my:clipboard-paste-cmd
                   (my:clipboard--get-cmd-internal
                    my:clipboard-paste-cmd-candidates))))
        (t (error (format "Unsupported command type : %s" type)))))

(defun my:copy-to-clipboard ()
  "Copy selected region into clipboard."
  (interactive)
  (when (region-active-p)
    (if (and (display-graphic-p) x-select-enable-clipboard)
        (x-set-selection 'CLIPBOARD
                         (buffer-substring (region-beginning) (region-end)))
      (let* ((cmd (my:clipboard-get-cmd 'copy))
             (exec (if (listp cmd) (car cmd) cmd))
             (arg (if (listp cmd) (cdr cmd)))
             (args (list (region-beginning)
                         (region-end)
                         exec nil 0 nil)))
        (when arg
          (setq args (append args (list arg))))
        (apply #'call-process-region args)))
    (deactivate-mark)))

(defun my:paste-from-clipboard ()
  "Paste text from clipboard."
  (interactive)
  (insert
   (if (and (display-graphic-p) x-select-enable-clipboard)
       (x-get-selection 'CLIPBOARD)
     (or (let* ((cmd (my:clipboard-get-cmd 'paste))
                (exec (if (listp cmd) (car cmd) cmd))
                (arg (if (listp cmd) (cdr cmd)))
                (args (list exec nil (current-buffer) nil)))
           (with-temp-buffer
             (apply #'call-process args)
             (buffer-string)) "")))))

(provide 'utils-editor)
