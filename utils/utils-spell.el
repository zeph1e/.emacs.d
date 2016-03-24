;;; utils-screenshot.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(setq ispell-program-name "aspell"
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=5" "--run-together-min=2"))

(provide 'utils-spell)
