;;; utils-screenshot.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(setq-default screenshot-schemes
              '(("local" :dir "~/Pictures/")
                ("current-directory :dir default-directory")))

(defun my:screenshot-do-import (filename)
  "Run ImageMagick import command. Recent version of ImageMagick
changed to whole functionalities in only a command `magick'."
  (let ((magick (executable-find "magick"))
        (import (executable-find "import")))
    (if magick
        (call-process magick nil nil nil "import" filename)
      (call-process import nil nil nil filename))))

(with-eval-after-load 'screenshot
  (defalias 'screenshot-do-import 'my:screenshot-do-import))

(provide 'utils-screenshot)
