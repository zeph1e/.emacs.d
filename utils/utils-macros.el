;;; utils-macros.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;; http://www.emacswiki.org/emacs/LoadingLispFiles
(defmacro with-library (symbol &rest body)
      `(when (require ,symbol nil t)
         ,@body))

;; https://www.emacswiki.org/emacs/BufferLocalKeys
(defun buffer-local-set-key (key func)
      (interactive "KSet key on this buffer: \naCommand: ")
      (let ((name (format "%s-magic" (buffer-name))))
        (eval
         `(define-minor-mode ,(intern name)
            "Automagically built minor mode to define buffer-local keys."))
        (let* ((mapname (format "%s-map" name))
               (map (intern mapname)))
          (unless (boundp (intern mapname))
            (set map (make-sparse-keymap)))
          (eval
           `(define-key ,map ,key func)))
        (funcall (intern name) t)))

(provide 'utils-macros)
