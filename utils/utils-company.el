;;; utils-company.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defmacro my:install-company-backends (backend hook &rest body)
  "Add company backends to hooks

\(fn COMPANY-BACKEND HOOK INIT-BODY...)"
  `(add-hook (quote ,hook)
               (lambda ()
                 (add-to-list (make-local-variable `company-backends)
                              (quote (,backend :with company-capf)))
                 ,@body
                 )))

(eval-after-load 'company
  '(progn
     (setq-default company-ispell-available t)
     (setq-default company-backends
                   '(my:company-ispell company-capf
                     (company-yasnippet
                      company-dabbrev-code
                      company-gtags
                      company-etags
                      :with company-keywords)
                     company-files
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

     ;; python
     (my:install-company-backends company-anaconda python-mode-hook)
     ))


(defun my:company-ispell (command &optional arg &rest ignored)
  "`company-ispell' wrapper to enable it only for text."
  (interactive (list 'interactive))
  (let ((face (face-at-point))
        (face-to-activate '(font-lock-doc-face
                            font-lock-comment-face
                            font-lock-string-face
                            flyspell-duplicate
                            flyspell-incorrect)))
    (when (or (derived-mode-p 'text-mode)
            (member face face-to-activate))
      (company-ispell command arg ignored))))

(provide 'utils-company)
