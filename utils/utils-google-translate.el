;;; utils-google-translate.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(eval-after-load 'google-translate
  '(progn
     (setq google-translate-default-source-language "auto")
     (setq google-translate-default-target-language "ko")))
(global-set-key (kbd "M-+") 'google-translate-at-point)
