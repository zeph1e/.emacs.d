(use-package whitespace
  :pin manual
  :init
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
      (whitespace-mode -1))))
