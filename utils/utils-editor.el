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

(defvar my:fci-selected-window nil
  "Selected window to trace changes in window selection.")

(defun my:fci-narrow-window-p (window-or-frame)
  (let* ((window (cond ((framep window-or-frame)
                        (with-selected-frame window-or-frame
                          (selected-window)))
                       ((windowp window-or-frame) window-or-frame)
                       ((null window-or-frame)
                        (with-selected-frame (selected-frame)
                          (selected-window)))
                       (t (error (format "Invalid type %s is given"
                                         (type-of window-or-frame))))))
         (width (window-body-width window))
         (ruler (with-current-buffer (window-buffer window)
                  (or fci-rule-column
                      fill-column))))
    (<= width (1+ ruler))))

(defun my:fci-activate (&optional buffer-or-name)
  (with-current-buffer (or (and buffer-or-name
                                (get-buffer buffer-or-name))
                           (current-buffer))
    (when (and my:fci-mode-suppressed
               (not (delete nil (mapcar ; all of windows are wide enough
                                 #'(lambda (w)
                                     (my:fci-narrow-window-p w))
                                 (get-buffer-window-list
                                  (current-buffer) nil t)))))
      (turn-on-fci-mode)
      (setq-local my:fci-mode-suppressed nil))))

(defun my:fci-suppress (&optional buffer-or-name)
  (with-current-buffer (or (and buffer-or-name
                                (get-buffer buffer-or-name))
                           (current-buffer))
    (when fci-mode
      (setq-local my:fci-mode-suppressed t)
      (turn-off-fci-mode))))

(defun my:fci-activate-focused (&optional frame-or-window)
  (let* ((frame (cond ((framep frame-or-window) frame-or-window)
                      ((windowp frame-or-window) (window-frame frame-or-window))
                      (t (selected-frame))))
         (window (cond ((windowp frame-or-window) frame-or-window)
                       (t (with-selected-frame frame
                            (selected-window)))))
         (buffer-to-clear  (unless (or (null my:fci-selected-window)
                                       (eq my:fci-selected-window window))
                             (window-buffer my:fci-selected-window))))
    ;; clear fci-overlays in background buffer
    (when (and (bufferp buffer-to-clear)
               (not (eq buffer-to-clear (window-buffer (selected-window)))))
      (with-current-buffer buffer-to-clear
        (my:fci-suppress)))
    (if (and (eq my:fci-selected-window window)
             (my:fci-narrow-window-p window))
        (my:fci-suppress)
      (my:fci-activate))
    (setq my:fci-selected-window window)))

;; Setting fci-mode variables
(with-eval-after-load 'fill-column-indicator
  (setq fci-handle-truncate-lines nil)
  (setq fci-rule-color "maroon4")
  (setq fci-rule-column nil)
  (setq fci-rule-width 1)
  (add-hook 'window-configuration-change-hook
            'my:fci-activate-focused)
  (add-hook 'buffer-list-update-hook
            'my:fci-activate-focused)
  (add-to-list 'window-size-change-functions
               'my:fci-activate-focused))

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
