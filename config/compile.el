(use-package compile
  :pin manual
  :config
  (defun my:compile ()
    (interactive)
    (if (bufferp compilation-last-buffer)
        (recompile)
      (call-interactively #'compile)))
  :bind
  (:map my:global-key-map
   ("<f7>" . my:compile)))
