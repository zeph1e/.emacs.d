;;-*- mode: emacs-lisp; -*-
(defalias 'redraw-modeline 'force-mode-line-update
  "Alias, redraw-modeline is removed around version 29")

;; To fix issue in dictionaries-common:
;; https://www.ramimassoud.com/til/fix-debian-emacs-ispell
(setq ispell-menu-map-needed t)
