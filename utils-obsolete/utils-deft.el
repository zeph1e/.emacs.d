;; utils-deft.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(with-eval-after-load 'deft
  (setq deft-default-extension "org"
        deft-directory "~/Documents/deft"
        deft-recursive t
        deft-text-mode 'org-mode))

(global-set-key (kbd "M-g d") #'deft)
(global-set-key (kbd "M-g M-d") #'deft)
(global-set-key (kbd "C-x RET C-d") #'deft)

(provide 'utils-deft)
