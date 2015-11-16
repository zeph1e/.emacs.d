;;; utils-google-translate.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(eval-after-load 'google-translate
  '(progn
     (setq google-translate-default-source-language "auto")
     (setq google-translate-default-target-language "ko")
     (setq google-translate-translation-directions-alist
           '(("en" . "ko") ("ko" . "en") ("de" . "ko") ("ko" . "de") ("ko" . "ja")))))
(global-set-key (kbd "M-+") 'google-translate-at-point)
(global-set-key (kbd "C-+") 'google-translate-smooth-translate)
