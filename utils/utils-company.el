;;; utils-company.el

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
(my:add-company-backend-to-hook company-css web-mode
                                '(web-mode-hook))
(my:add-company-backend-to-hook company-tern web-mode
                                '(web-mode-hook) (tern-mode t))
(my:add-company-backend-to-hook company-tern js2-mode
                                '(js2-mode-hook) (tern-mode t))

(provide 'utils-company)
