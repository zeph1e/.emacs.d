;;; utils-screenshot.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(setq ispell-program-name "aspell"
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      ispell-extra-args '("--sug-mode=fast" "--lang=en_US" "--run-together" "--run-together-limit=5" "--run-together-min=2"))

(define-minor-mode my:flyspell-mode
  "Enable flyspell-mode."
  :variable my:flyspell-mode
  (if my:flyspell-mode
      (progn
        (if (derived-mode-p 'prog-mode) (flyspell-prog-mode)
          (flyspell-mode))
        (if (called-interactively-p) (flyspell-buffer)))
    (flyspell-mode -1)))

(provide 'utils-spell)
