(use-package diff
  :pin manual
  :hook
  (diff-mode . (lambda ()
                 (setq-local whitespace-style
                             '(face trailing tabs tab-mark))
                 (whitespace-mode 1))))
