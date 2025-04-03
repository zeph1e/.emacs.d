(use-package flyspell
  :ensure-system-package
  (ispell . "sudo apt install -y ispell")
  :config
  (defun my:toggle-flyspell-mode ()
    (interactive)
    (if flyspell-mode
        (flyspell-mode -1)
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (flyspell-mode))
      (when (called-interactively-p)
        (flyspell-buffer))))
  :bind
  (:map my:global-key-map
   ("<f8>" . my:toggle-flyspell-mode)))

(use-package flyspell-popup
  :bind
  (:map my:global-key-map
  ("C-;" . flyspell-popup-correct)))
