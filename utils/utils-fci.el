;; utils-fci.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;; Disable fci-mode if the window is smaller than fill-column
(defvar my:fci-mode-suppressed nil)

(defvar my:fci-previous-window nil
  "Previous window to trace changes in window selection.")

(defvar my:fci-previous-buffer nil
  "Previous buffer to trace change in selected window's buffer.")

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
         (buffer-to-clear
          (when (and (windowp my:fci-previous-window)
                     (buffer-live-p my:fci-previous-buffer)
                     (or (not (eq my:fci-previous-window window))
                         (not (eq my:fci-previous-buffer
                                  (window-buffer window)))))
            my:fci-previous-buffer)))
    ;; Suppress fci in background buffer
    (when (and (bufferp buffer-to-clear)
               (not (eq buffer-to-clear (window-buffer window))))
      (with-current-buffer buffer-to-clear
        (my:fci-suppress)))
    ;; Handle when widows size got narrowed too much or widen enough
    (if (and (eq my:fci-previous-window window)
             (my:fci-narrow-window-p window))
        (my:fci-suppress)
      (my:fci-activate))

    ;; Only record the focus changes in fci-aware buffers
    (with-current-buffer (window-buffer window)
      (when (or my:fci-mode-suppressed
                fci-mode)
        (setq my:fci-previous-window window
              my:fci-previous-buffer (current-buffer))))))


;; Fix the problem of distorted popup of company
(defun my:company-fci-workaround (command)
  (cond ((string= command "show") (turn-off-fci-mode))
        ((string= command "hide") (turn-on-fci-mode))))
(advice-add 'company-call-frontends :before #'my:company-fci-workaround)

;; Fix the problem of distorted popup of flyspell-popup-correct
(defun my:flyspell-fci-workaround (orig-fun &rest args)
  (condition-case nil
      (if fci-mode
          (progn
            (turn-off-fci-mode)
            (unwind-protect
                (apply orig-fun args)
              (turn-on-fci-mode)))
        (error "trap"))
    ;; raise buffer-read-only signal again
    (buffer-read-only (signal 'buffer-read-only (list (current-buffer))))
    (error (apply orig-fun args))))
(advice-add 'flyspell-popup-correct :around #'my:flyspell-fci-workaround)

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
