;;-*- mode: emacs-lisp; -*-
(use-package compat
  :config
  (defun process-kill-without-query (process &optional flag)
    (set-process-query-on-exit-flag process nil) t)
  (defalias 'redraw-modeline 'force-mode-line-update
      "Alias, redraw-modeline is removed around version 29")
  )
