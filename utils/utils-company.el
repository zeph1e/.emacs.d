;;; utils-company.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defmacro my:install-company-backends (backend hook &rest body)
  "Add company backends to hooks

\(fn COMPANY-BACKEND HOOK INIT-BODY...)"
  `(add-hook (quote ,hook)
               (lambda ()
                 (add-to-list (make-local-variable `company-backends)
                              (quote ,backend))
                 ,@body
                 )))

(eval-after-load 'company
  '(progn
     (setq-default company-backends
                   '(company-capf company-files company-ispell
                     (company-keywords company-yasnippet)
                     (company-dabbrev-code company-gtags company-etags company-abbrev)
                     company-dabbrev))

     ;; Add keys company-active-map
     (let ((map company-active-map))
       (define-key map (kbd "C-p") 'company-select-previous)
       (define-key map (kbd "C-n") 'company-select-next)
       (define-key map (kbd "C-v") 'company-next-page)
       (define-key map (kbd "M-v") 'company-previous-page))

     ;; install company backends to certain major modes
     ;; web
     (my:install-company-backends company-web-html web-mode-hook)
     (my:install-company-backends company-web-html sgml-mode-hook)
     (my:install-company-backends company-css web-mode-hook)
     (my:install-company-backends company-css css-mode-hook)
     (my:install-company-backends company-tern web-mode-hook (tern-mode t))
     (my:install-company-backends company-tern js2-mode-hook (tern-mode t))
     (my:install-company-backends company-nxml nxml-mode-hook)

     ;; lisp
     (my:install-company-backends company-elisp emacs-lisp-mode-hook)
     (my:install-company-backends company-elisp lisp-interaction-mode-hook)
     (my:install-company-backends company-elisp ielm-mode-hook)

     ;; C/C++
     (my:install-company-backends company-c-headers c-mode-hook)
     (my:install-company-backends company-c-headers c++-mode-hook)
     (my:install-company-backends company-c-headers objc-mode-hook)

     ))


(provide 'utils-company)
