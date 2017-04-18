;;; Utils-company.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defmacro my:add-company-backend-to-hook (backend lib hooks &rest body)
  "Macro to add company backends to certain mode hook."
  `(eval-after-load (quote ,lib)
     (dolist (hook ,hooks)
       (add-hook hook (lambda ()
                        ,@body
                        (add-to-list (make-local-variable `company-backends)
                                     (quote ,backend)))))))

(my:add-company-backend-to-hook company-web-html web-mode
                                '(web-mode-hook))
(my:add-company-backend-to-hook company-web-html sgml-mode
                                '(sgml-mode-hook html-mode-hook))
(provide 'utils-company)
