;;; utils-ansi.el -- set ansi color

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(eval-after-load 'ansi-color
  '(progn
     (setq ansi-color-names-vector
           ["black" "tomato" "chartreuse1" "gold1"
            "DeepSkyBlue1" "Mediu8mOrchid1" "cyan" "white"])
     (setq ansi-color-map (ansi-color-make-color-map))))

