;;; utils-macros.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;; http://www.emacswiki.org/emacs/LoadingLispFiles
(defmacro with-library (symbol &rest body)
      `(when (require ,symbol nil t)
         ,@body))

(provide 'utils-macros)
