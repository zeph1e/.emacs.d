;;; flyspell.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package flyspell
  :ensure-system-package (ispell . "sudo apt install -y ispell")
  :config
  ;; To make flyspell ignore urls
  ;; https://emacs.stackexchange.com/q/5415
  (defun my:flyspell-ignore-url ()
    "Function used for `flyspell-generic-check-word-predicate'
to ignore url stuff"
    (not (thing-at-point 'url)))

  (defun my:toggle-flyspell-mode ()
    (interactive)
    (if flyspell-mode
        (flyspell-mode -1)
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (flyspell-mode))
      (put major-mode 'flyspell-mode-predicate 'my:flyspell-ignore-url)
      (when (called-interactively-p)
        (flyspell-buffer))))
  :bind
  (:map my:global-key-map
   ("<f8>" . my:toggle-flyspell-mode)
   ("C-<f8>" . flyspell-buffer)))

(use-package flyspell-popup
  :bind
  (:map my:global-key-map
  ("C-;" . flyspell-popup-correct)))
