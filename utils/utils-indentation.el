;;; utils-indentation.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(c-add-style "user"
             `((c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-hanging-braces-alist
                (defun-open before)
                (defun-close before after)
                (class-open before)
                (close-close before after)
                (inexpr-class-open after)
                (inexpr-class-close before)
                (inline-open after)
                (inline-close before after)
                (block-open after)
                (block-close . c-snug-do-while)
                (extern-lang-open after)
                (extern-lang-close after)
                (statement-case-open after)
                (substatement-open after))
               (c-hanging-colons-alist
                (case-label)
                (label after)
                (access-label after)
                (member-init-intro before)
                (inher-intro))
               (c-offsets-alist
                (c . c-lineup-C-comments)
                (topmost-intro . 0)
                (topmost-intro-cont . c-lineup-topmost-intro-cont)
                (defun-open . 0)
                (defun-close . 0)
                (defun-block-intro . +)
                (class-open . 0)
                (class-close . 0)
                (inher-intro . ++)
                (inher-cont . c-lineup-multi-inher)
                (access-label . /)
                (inline-open . 0)
                (inline-close . 0)
                (namespace-open . 0)
                (namespace-close . 0)
                (innamespace . 0)
                (extern-lang-open . 0)
                (extern-lang-close . 0)
                (inextern-lang . 0)
                (func-decl-cont . ++)
                (arglist-intro . ++)
                (arglist-cont . (c-lineup-argcont c-lineup-arglist))
                (arglist-cont-nonempty . (c-lineup-argcont c-lineup-arglist))
                (label . [0])
                (case-label . +)
                (member-init-intro . ++)
                (member-init-cont . c-lineup-multi-inher)
                (statement-case-intro . +)
                (statement-case-open . 0)
                (substatement-open . 0)
                (statement-block-intro . +)
                (statement-cont
                 .
                 (,(when (fboundp 'c-no-indent-after-java-annotations)
                     'c-no-indent-after-java-annotations)
                  ,(when (fboundp 'c-lineup-assignments)
                     'c-lineup-assignments)
                  ++))
                )))

(c-add-style "webkit"
             `("user"
               (c-basic-offset . 4)
               (indent-tabs-mode . nil)
               (c-offsets-alist
                (access-label . -)
                (case-label . 0)
                (member-init-intro . +))))

(c-add-style "kernel"
             `("linux"
               (c-basic-offset . 4)
               (indent-tab-mode . t)))

(setq-default
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (python-mode . "python")
                   (other . "google"))
 c-basic-offset 4
 tab-width 4 ; tab width
 indent-tabs-mode nil ; don't insert tabs in indent
 tab-always-indent nil)

(provide 'utils-indentation)
