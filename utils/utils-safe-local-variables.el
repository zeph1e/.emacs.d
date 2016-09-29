;;; utils-safe-local-variables.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

;; I hate emacs ask about following variables are ok
;; while I don't want put cutomized variables part into
;; init.el

(defconst my:safe-local-variables
  '((cscope-option-do-not-update-database . t)
    (buffer-read-only . t)
    (eval . (google-set-c-style))
    ))

(setq safe-local-variable-values
      (append safe-local-variable-values my:safe-local-variables))

(provide 'utils-safe-local-variables)
