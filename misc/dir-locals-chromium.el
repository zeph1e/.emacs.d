;; place this into chromium directory
((nil . ((indent-tabs-mode . nil)
         (c-basic-offset . 4)
         (fill-column . 80)
         (cscope-option-do-not-update-database . t)))
 (c-mode . ((eval . (google-set-c-style))
            (eval . (require 'magit))
            (eval . (require 'company-c-headers))
            (eval . (add-to-list 'company-c-headers-path-user
                                 (concat
                                  (file-name-directory
                                   (let ((d (dir-locals-find-file ".")))
                                     (cond ((stringp d) d)
                                           ((listp d) (car d))
                                           (t "")))) "src")))
            (buffer-read-only . t)))
 (c++-mode . ((eval . (google-set-c-style))
              (eval . (require 'magit))
              (eval . (require 'company-c-headers))
              (eval . (add-to-list 'company-c-headers-path-user
                                   (concat
                                    (file-name-directory
                                     (let ((d (dir-locals-find-file ".")))
                                       (cond ((stringp d) d)
                                             ((listp d) (car d))
                                             (t "")))) "src")))
              (buffer-read-only . t)))
 (css-mode . ((css-indent-offset . 2)))
 (js2-mode . ((js2-basic-offset . 2))))
