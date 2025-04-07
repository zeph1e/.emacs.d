;;; utils-win32.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(require 'utils-helm)
(require 'utils-keybinding)
(require 'utils-screenshot)

;; To resolve encoding conflict of shell on windows
(eval-after-load "shell"
  '(when (eq system-type 'windows-nt)
    (defadvice shell (around shell-w32-encoding (&optional buffer))
      (interactive)
      (let ((coding-system-for-read 'korean-cp949))
        ad-do-it))
      (ad-activate 'shell)))

;; On windows, resolve key conflict with windows IME
(when (eq system-type 'windows-nt)
  (let ((map my:keys-mode-keymap)
        (korean-font "맑은 고딕-10"))
    ;; windows keybinding
    (define-key map (kbd "C-<kanji>") 'set-mark-command)
    (define-key map (kbd "<kana>")    'toggle-input-method) ; windows 10, S-<space> as <kana>

    (set-fontset-font "fontset-default" '(#x1100 . #xffdc) korean-font)
    (set-fontset-font "fontset-default" '(#xe0bc . #xf66e) korean-font))

  (setq-default screenshot-schemes
                '(("local" :dir "d:/Pictures/")
                  ("current-directory :dir default-directory")))
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-personal-dictionary "~/.ispell"))

  (when (executable-find "plink")
    (require 'tramp)
    (setq-default tramp-default-method "plink"))

  (setq projectile-indexing-method 'alien)

  )

(provide 'utils-win32)

