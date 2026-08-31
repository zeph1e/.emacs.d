;;; company.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package company
  :bind
  (:map my:global-key-map
   ("C-;" . company-complete)
   :map company-active-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next)
   ("C-v" . company-next-page)
   ("M-v" . company-previous-page))
  :init
  (global-company-mode)
  :config
  (defvar-local my:in-string-or-comment--cache nil)

  (defun my:in-string-or-comment-p ()
    "Returns non-nil when completing string or comment."
    (let ((key (cons (buffer-chars-modified-tick) (point))))
      (if (equal (car my:in-string-or-comment--cache) key)
          (cdr my:in-string-or-comment--cache)
        (let ((result
               (or (nth 8 (syntax-ppss))
                   ;; Fix for cc-mode derivation
                   (when (and (boundp 'c-buffer-is-cc-mode) c-buffer-is-cc-mode)
                     (let* ((bol (line-beginning-position)) (pos (1- (point))))
                       (when (<= bol pos)
                         (or (eq (get-text-property pos 'face)
                                 'font-lock-string-face)
                             (let ((qpos (save-excursion
                                           (search-backward "\"" bol t))))
                               (and qpos
                                    (eq (get-text-property qpos 'face)
                                        'font-lock-warning-face))))))))))
          (setq my:in-string-or-comment--cache (cons key result))
          result))))

  (defmacro my:make-context-aware (backend &optional inverse)
    "Add context-aware :around advice to BACKEND.

In `prog-mode' or a mode in `my:custom-prog-mode-list':
- INVERSE nil: BACKEND runs in code only.  It skips strings and comments.
- INVERSE non-nil: BACKEND runs in strings and comments only.  It skips code.

In all other modes:
- INVERSE nil: BACKEND never runs.
- INVERSE non-nil: BACKEND always runs."
    (let* ((advice-fun (intern (format "my:%s--context-advice" backend))))
      `(progn
         (ignore-error
             (require ',backend))
         (defun ,advice-fun (orig-fun command &optional arg &rest args)
           ,(format "Context-aware :around advice for %s." backend)
           (when (xor ,inverse
                      (and (or (derived-mode-p 'prog-mode)
                               (memq major-mode my:custom-prog-mode-list))
                           (not (my:in-string-or-comment-p))))
             (apply orig-fun command arg args)))
         (advice-add ',backend :around #',advice-fun))))

  (my:make-context-aware company-capf)
  (my:make-context-aware company-keywords)
  (my:make-context-aware company-dabbrev-code)
  (my:make-context-aware company-ispell t)
  (my:make-context-aware company-files t)
  (my:make-context-aware company-dabbrev t)

  (defun my:company-files--complete (_prefix)
    "Fix not to gather entries in sub-directories."
    (let* ((full-prefix (company-files--grab-existing-name))
           (dir (file-name-directory full-prefix))
           (file (file-name-nondirectory full-prefix))
           (key (list file
                      (expand-file-name dir)
                      (nth 5 (file-attributes dir))))
           (completion-ignore-case read-file-name-completion-ignore-case))
      (unless (or (company-file--keys-match-p
                   key (car company-files--completion-cache))
                  (not (company-files--connected-p dir)))
        (let ((candidates (company-files--directory-files dir file)))
          (setq company-files--completion-cache
                (cons key candidates))))
      (all-completions file (cdr company-files--completion-cache))))
  (advice-add 'company-files--complete :override #'my:company-files--complete)

  (defconst my:company-backends-alist
    '((web-mode . (company-web-html company-css))
      ((css-mode less-css-mode) . company-css)
      ((c-mode c++-mode objc-mode) . company-c-headers)
      (python-mode . company-anaconda))
    "alist to specify list of company backends for each major modes.
They get inserted in front of `company-backends'.")

  (letrec ((handle-entry
            (lambda (pair)
              (let ((mode (car pair))
                    (backends (cdr pair)))
                (if (listp mode)
                    (mapc handle-entry
                          (mapcar #'(lambda (m) (cons m backends))
                                  mode))
                  (add-hook (intern (concat (symbol-name mode) "-hook"))
                            #'(lambda ()
                                (add-to-list
                                 (make-local-variable 'company-backends)
                                 backends))))))))
    (mapc handle-entry my:company-backends-alist))
  :hook
  (after-init . global-company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-backends
   '((company-capf company-keywords company-dabbrev-code
                   :with company-yasnippet)
     company-files company-ispell company-dabbrev)))

(use-package company-c-headers
  :ensure-system-package
  (gcc . "sudo apt install -y build-essential")
  :config
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
      (when (stringp compiler)
        (with-temp-buffer
          (when (zerop (call-process compiler nil (current-buffer) nil
                                     (concat "-x" (downcase lang))
                                     "-E" "-v" "-"))
            (seq-filter (lambda (s) (and (string-prefix-p "/" s)
                                         (file-directory-p s)))
                        (mapcar (lambda (s) (file-truename (string-trim s)))
                                (split-string (buffer-string) "\n"))))))))

  (defun my:company-find-headers-subdir (parent-directory pattern)
    "Searches sub-directories to add to include path."
    (when (file-directory-p parent-directory)
      (seq-filter #'file-directory-p
                  (directory-files parent-directory t pattern))))
  (setq-default company-c-headers-path-system
                (delete-dups
                 (flatten-list
                  (list (mapcar (lambda (s) (string-remove-suffix "/" s))
                                (seq-filter #'file-directory-p
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

(use-package company-box
  :config
  ;; company-box icon customization
  (add-to-list 'company-box-icons-functions
               #'(lambda (candidate)
                   (cond
                    ((eq company-backend 'company-ispell) 'SpellCheck)
                    ((eq company-backend 'company-files)
                     (if (file-directory-p
                          (expand-file-name
                           candidate (file-name-directory company-prefix)))
                         'Folder 'File)))))
  (letrec ((icon-image
            (lambda (file)
              (let ((location (expand-file-name (concat "misc/res/" file)
                                                user-emacs-directory)))
                `(image :type png :file ,location :ascent center)))))
    (setq company-box-icons-images
          (append company-box-icons-images
                  `((SpellCheck . ,(funcall icon-image "SpellCheck.png"))))))
  :hook (company-mode . company-box-mode))

(use-package company-statistics
  :hook
  (after-init . company-statistics-mode))

(use-package company-web)
