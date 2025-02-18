;;-*- mode: emacs-lisp; -*-
(use-package compat
  :config
  (defun process-kill-without-query (process &optional flag)
    (set-process-query-on-exit-flag process nil) t)
  (when (>= (string-to-number emacs-version) 24.3)
    ;; compat issue in 29.1
    (defun redraw-modeline (&optional all)
      (force-mode-line-update all)))
  )
