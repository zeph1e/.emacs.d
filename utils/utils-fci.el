;; utils-fci.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

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
  (setq fci-handle-truncate-lines nil
        fci-rule-color "maroon4"
        fci-rule-column nil
        fci-rule-width 1)

  ;; adding hooks to enable fci only in selected window's buffer
  (add-hook 'window-configuration-change-hook
            'my:fci-activate-focused)
  (add-hook 'buffer-list-update-hook
            'my:fci-activate-focused)
  (add-to-list 'window-size-change-functions
               'my:fci-activate-focused)
  (add-hook 'focus-in-hook
            'my:fci-activate-focused))


(provide 'utils-fci)
