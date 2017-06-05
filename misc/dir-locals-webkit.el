;; place this into chromium/src/third_party/WebKit
((nil . ((indent-tabs-mode . nil)
         (c-basic-offset . 4)
         (fill-column . 120)))
 (c-mode . ((c-file-style . "webkit")
            (eval . (add-to-list 'company-c-headers-path-user
                                 (concat
                                  (file-name-directory
                                   (let ((d (dir-locals-find-file ".")))
                                     (cond ((stringp d) d)
                                           ((listp d) (car d))
                                           (t "")))) "Source")))
            (buffer-read-only . t)))
 (c++-mode . ((c-file-style . "webkit")
              (eval . (add-to-list 'company-c-headers-path-user
                                   (concat
                                    (file-name-directory
                                     (let ((d (dir-locals-find-file ".")))
                                       (cond ((stringp d) d)
                                             ((listp d) (car d))
                                             (t "")))) "Source")))
              (buffer-read-only . t))))
