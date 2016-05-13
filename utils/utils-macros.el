;;; utils-macros.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;; http://www.emacswiki.org/emacs/LoadingLispFiles
(defmacro with-library (symbol &rest body)
      `(when (require ,symbol nil t)
         ,@body))

(defmacro define-c-style (name parent offset use-tab &rest args)
  "Define c style"
  `(c-add-style  ,name
                 (list ,parent
                       `(c-basic-offset . ,,offset)
                       `(indent-tabs-mode . ,,use-tab)
                       (cons `c-offsets-alist
                             (let ((plist (quote (,@args)))
                                   result)
                               (while plist
                                 (let ((keyname (symbol-name (car plist)))
                                       (val (cadr plist)))
                                   (setq plist (cddr plist))
                                   (when (eq (string-to-char keyname) ?:)
                                     (push `(,(intern (substring keyname 1)) . ,val) result))))
                               result)))))

(provide 'utils-macros)
