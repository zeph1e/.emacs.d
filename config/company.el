;;; company.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package company
  :bind
  (:map my:global-key-map
   ("C-M-;" . company-complete)
   :map company-active-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next)
   ("C-v" . company-next-page)
   ("M-v" . company-previous-page))
  :init
  (global-company-mode)
  :config
  (defmacro my:install-company-backends (backend hook &rest body)
    "Add company backends to hooks

\(fn COMPANY-BACKEND HOOK INIT-BODY...)"
    `(add-hook (quote ,hook)
               (lambda ()
                 (add-to-list (make-local-variable `company-backends)
                              (quote ,backend))
                 ,@body
                 )))

  (defun my:company-ispell (command &optional arg &rest ignored)
    "`company-ispell' wrapper to enable it only for text."
    (interactive (list 'interactive))
    (let ((face (save-excursion (backward-word) (face-at-point 'word)))
          (face-to-activate '(font-lock-doc-face
                              font-lock-comment-face
                              font-lock-string-face
                              flyspell-duplicate
                              flyspell-incorrect)))
      (when (or (derived-mode-p 'text-mode)
                (member face face-to-activate))
        (company-ispell command arg ignored))))

  (setq-default company-ispell-available t
                company-backends
                '((my:company-ispell :with company-files)
                  (company-capf company-yasnippet company-dabbrev-code
                   company-keywords company-dabbrev)))

  ;; install company backends to certain major modes
  ;; web
  (my:install-company-backends company-web-html web-mode-hook)
  (my:install-company-backends company-web-html sgml-mode-hook)
  (my:install-company-backends company-css web-mode-hook)
  (my:install-company-backends company-css css-mode-hook)
  (my:install-company-backends company-tern web-mode-hook (tern-mode t))
  (my:install-company-backends company-nxml nxml-mode-hook)

  (my:install-company-backends company-tern js-mode-hook (tern-mode t))

  ;; C/C++
  (my:install-company-backends company-c-headers c-mode-hook)
  (my:install-company-backends company-c-headers c++-mode-hook)
  (my:install-company-backends company-c-headers objc-mode-hook)

  ;; python
  (my:install-company-backends company-anaconda python-mode-hook)
  :hook
  (after-init . global-company-mode)
  :custom
  (company-tooltip-align-annotations t))

(use-package company-c-headers
  :config
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
  (setq-default company-c-headers-path-system
                (delete-dups
                 (flatten-list
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

(use-package company-statistics
  :hook
  (after-init . company-statistics-mode))

(use-package company-web)
(use-package company-tern
  :ensure nil
  :after (tern))
