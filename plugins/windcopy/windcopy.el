;;; windcopy.el --- Extension of windswap. To copy buffer to moving point.

(require 'windswap)
(require 'cl-lib)

(defgroup windcopy nil
  "Like windmove, but swaps buffers while moving point."
  :group 'frames)

;;;###autoload
(defun windcopy-default-keybindings (&rest modifiers)
  "Set up keybindgs for `windcopy'"
  (interactive)
  (unless modifiers (setq modifiers (list 'control 'shift)))
  (global-set-key (vector (append modifiers '(left)))  'windcopy-left)
  (global-set-key (vector (append modifiers '(right))) 'windcopy-right)
  (global-set-key (vector (append modifiers '(up)))    'windcopy-up)
  (global-set-key (vector (append modifiers '(down)))  'windcopy-down))

(defun windcopy--check-window (window)
  "Report a user error if WINDOW cannot be swapped."
  (when (window-minibuffer-p window)
    (user-error "Can't copy the minibuffer window"))
  (when (window-dedicated-p window)
    (user-error "Dedicated windows can't be copied")))

(defun windcopy--do-copy (direction arg)
  "Try to copy in DIRECTION."
  (let ((initial (selected-window)))
    (windcopy--check-window initial)
    (let ((new (windswap--find-other-window direction arg)))
      (unless new
	(user-error "No copyable window %s from select window" direction))
      (windcopy--check-window new)
      (select-window new)
      (set-window-buffer new (window-buffer initial)))))

;;;###autoload
(defun windcopy-left (&optional arg)
  "Copy buffer to right window"
  (interactive "P")
  (windcopy--do-copy 'left arg))

;;;###autoload
(defun windcopy-right (&optional arg)
  "Copy buffer to right window"
  (interactive "P")
  (windcopy--do-copy 'right arg))

;;;###autoload
(defun windcopy-up (&optional arg)
  "Copy buffer to right window"
  (interactive "P")
  (windcopy--do-copy 'up arg))

;;;###autoload
(defun windcopy-down (&optional arg)
  "Copy buffer to right window"
  (interactive "P")
  (windcopy--do-copy 'down arg))

(provide 'windcopy)
