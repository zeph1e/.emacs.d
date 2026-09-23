;;; debug.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.


(use-package profiler
  :ensure nil
  :pin manual
  :config
  (defvar my:profiler-active nil)
  (defun my:profiler-toggle ()
    (interactive)
    (if my:profiler-active
        (progn
          (profiler-stop)
          (setq my:profiler-active nil)
          (profiler-report))
      (call-interactively #'profiler-start)
      (setq my:profiler-active t)))
  :bind
  (:map my:global-key-map
   ("<pause>" . my:profiler-toggle)))
