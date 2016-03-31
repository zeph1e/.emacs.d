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

(defadvice kill-buffer (around my:kill-buffer-modified (&optional buffer-or-name))
  "Adviced kill buffer to show diff with original file to verify the changes."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (if (and (buffer-modified-p)
             (buffer-file-name))
      (save-window-excursion
        (display-buffer (current-buffer) '(display-buffer-same-window))
        (delete-other-windows)
        (let ((diff-switches "-urN"))
          (diff (if (file-exists-p buffer-file-name) buffer-file-name null-device)
                (current-buffer) nil 'noasync))
        (when (yes-or-no-p (format "Buffer %s modified; kill anyway? " (buffer-name)))
          (set-buffer-modified-p nil)
          ad-do-it))
      ad-do-it)))
(ad-activate 'kill-buffer)

;; My white space mode:
;;  - highlight trailing spaces
;:  - highlight tabs: http://www.emacswiki.org/emacs/ShowWhiteSpace
(defface my:tab-face '((t :background "orchid"))
  "Used for tab highlighting."
  :group 'basic-faces)
(define-minor-mode my:whitespace-mode
"Shows trailing whitespaces."
  nil nil nil
  (setq show-trailing-whitespace t)
  (font-lock-add-keywords nil '(("\t" . 'my:tab-face))))

(provide 'utils-editor)
