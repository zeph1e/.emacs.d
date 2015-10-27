;;; utils-auto-complete.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(defconst my:achead-std-begin-exp "^#include .+ starts here:$")
(defconst my:achead-std-end-exp "^End of search list.$")
(defvar my:achead-std-begin-exp-found nil)

(defun my:achead-find-qt-headers ()
  (if (executable-find "qmake")           ; qt headers
      (split-string (shell-command-to-string "qmake -query QT_INSTALL_HEADERS"))))

;; grap some include paths from compiler
(defun my:achead-find-std-headers (lang)
  (interactive "sLang:")
  (let ((buffer-name "*my:achead-find-std*")
        (compiler (or (getenv "CC")
                      (executable-find "gcc")
                      (and (interactive-p) (error "No compiler found!")))))
    (when (and compiler
               (= 0 (call-process compiler
                                  nil buffer-name nil
                                  (concat "-x" (downcase (or (and (stringp lang) lang)
                                                             (signal 'wrong-type-argument (list lang)))))
                                  "-E" "-v" "-")))
      (with-current-buffer (get-buffer buffer-name)
        (let ((lines (split-string (buffer-string) "\n"))
              result
              skip-line)
          (save-excursion
            (goto-char 0)
            (dolist (line lines)
              (setq skip-line nil)
              (if (string-match my:achead-std-begin-exp line)
                  (progn (setq my:achead-std-begin-exp-found t)
                         (setq skip-line t))
                (if (string-match my:achead-std-end-exp line)
                    (setq skip-line t))
                (if (and (not skip-line) my:achead-std-begin-exp-found)
                    (dolist (w (split-string line))
                      (if (and (> (length w) 0) (string-prefix-p "/" w))
                          (push w result)))))))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer))
          result)))))

;; include subdirectories like gtk+-2.0 or QtCore for auto-complete-c-headers
(defconst my:achead-subdir-regexp "[A-Za-z0-9-_]+\\-[0-9.]+\\|Qt[A-Za-z]+")
(defun my:achead-find-subdirs (search-paths)
  (let* (includedirs)
    (dolist (d search-paths)
      (and (file-directory-p d)
           (string-prefix-p "/" d)
           (dolist (subd (directory-files d nil my:achead-subdir-regexp))
             (and (file-directory-p (expand-file-name subd d))
                  (push (expand-file-name subd d) includedirs)))))
    includedirs))

(eval-after-load 'auto-complete-c-headers
  (let ((paths (append (my:achead-find-std-headers "c")
                       (my:achead-find-std-headers "c++")
                       (my:achead-find-qt-headers))))
    (dolist (path (append paths (my:achead-find-subdirs paths)))
      (add-to-list 'achead:include-directories path))))
