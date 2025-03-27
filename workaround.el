;;-*- mode: emacs-lisp; -*-
(defalias 'redraw-modeline 'force-mode-line-update
  "Alias, redraw-modeline is removed around version 29")

;; To fix issue in dictionaries-common:
;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=968955
(setq ispell-menu-map-needed t)
