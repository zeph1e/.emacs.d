;;; ansi.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package ansi-color
  :ensure nil
  :pin manual
  :config
  (setq ansi-color-names-vector
           ["black" "tomato" "chartreuse1" "gold1"
            "DodgerBlue3" "MediumOrchid1" "cyan" "white"])
  (setq ansi-color-map (ansi-color-make-color-map)))
