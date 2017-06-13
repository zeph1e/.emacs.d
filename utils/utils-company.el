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
  (let ((face (face-at-point 'word))
        (face-to-activate '(font-lock-doc-face
                            font-lock-comment-face
                            font-lock-string-face
                            flyspell-duplicate
                            flyspell-incorrect)))
    (when (or (derived-mode-p 'text-mode)
              (member face face-to-activate)
              (when (eq face 'whitespace-trailing)
                (save-excursion
                  (backward-word)
                  (member (face-at-point 'word) face-to-activate))))
      (company-ispell command arg ignored))))

;;
;; company-c-headers settings
;;
(defun my:flatten (x)
  (cond ((null x) nil)
        ((listp x) (append (my:flatten (car x)) (my:flatten (cdr x))))
        (t (list x))))

(defun my:filter-list (condp list)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) list)))

(defun my:company-find-headers-qt ()
  "Find include paths of qt headers."
  (when (executable-find "qmake")
    (split-string
     (shell-command-to-string "qmake -query QT_INSTALL_HEADERS"))))

(defun my:company-find-headers-std (lang)
  "Find compiler specific include paths."
  (let ((compiler (or (getenv "CC")
                      (executable-find "gcc")
                      (executable-find "clang")
                      (and (interactive-p)
                           (error "No compiler found!")))))
    (with-temp-buffer
      (when (zerop (call-process compiler nil (current-buffer) nil
                                 (concat "-x" (downcase lang))
                                 "-E" "-v" "-"))
        (my:filter-list (lambda (s) (and (string-prefix-p "/" s)
                                         (file-directory-p s)))
                        (mapcar (lambda (s) (file-truename (string-trim s)))
                                (split-string (buffer-string) "\n")))))))

(defun my:company-find-headers-subdir (parent-directory pattern)
  "Searches sub-directories to add to include path."
  (when (file-directory-p parent-directory)
    (my:filter-list 'file-directory-p
                    (directory-files parent-directory t pattern))))

(with-eval-after-load 'company-c-headers
  (setq-default company-c-headers-path-system
                (delete-dups
                 (my:flatten
                  (list (mapcar (lambda (s) (string-remove-suffix "/" s))
                                (my:filter-list 'file-directory-p
                                                company-c-headers-path-system))
                        (mapcar (lambda (s) (my:company-find-headers-subdir
                                             s "[A-Za-z0-9-_]+\\-[0-9.]+"))
                                company-c-headers-path-system)
                        (my:company-find-headers-std "c")
                        (my:company-find-headers-std "c++")
                        (my:company-find-headers-qt)
                        (mapcar (lambda (s) (my:company-find-headers-subdir
                                             s "Qt[A-za-z]+"))
                                (my:company-find-headers-qt)))))))

(defun my:company-fci-workaround (command)
  (cond ((string= command "show") (turn-off-fci-mode))
        ((string= command "hide") (turn-on-fci-mode))))
(advice-add 'company-call-frontends :before #'my:company-fci-workaround)

(provide 'utils-company)
