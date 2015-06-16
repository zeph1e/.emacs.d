;;; franca-idl-mode.el

(defvar franca-idl-mode-hook nil
  "Franca IDL mode hook.")

(defvar franca-idl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-e") 'franca-idl-export)
    map)
  "Keymap for Franca IDL mode.")

;; (concat "\\<"
;;    (regexp-opt '("array" "of"
;;                  "enumeration" "extends"
;;                  "struct"
;;                  "union"
;;                  "map" "to"
;;                  "typedef" "is"
;;                  "typeCollection"
;;                  "interface"
;;                  "version" "major" "minor"
;;                  "attribute" "readonly" "noSubscriptions"
;;                  "method" "in" "out" "error" "fireAndForget"
;;                  "broadcast" "selective"
;;                  "contract" "PSM" "initial" "state" "call" "signal" "on"
;;                  "package" "import" "model" "from")) "\\>")

;; (regexp-opt '("UInt8" "Int8" "UInt16" "Int16" "UInt32" "Int32" "UInt64"
;;               "Int64" "Boolean" "Float" "Double" "String" "ByteBuffer"))


;; FIXME: auto move close brace
(defconst franca-idl-font-lock-keywords
  (list
   '("\\<\\(?:PSM\\|a\\(?:rray\\|ttribute\\)\\|broadcast\\|c\\(?:all\\|ontract\\)\\|e\\(?:numeration\\|rror\\|xtends\\)\\|f\\(?:ireAndForget\\|rom\\)\\|i\\(?:mport\\|n\\(?:itial\\|terface\\)\\|[ns]\\)\\|m\\(?:a\\(?:jor\\|p\\)\\|ethod\\|inor\\|odel\\)\\|noSubscriptions\\|o\\(?:ut\\|[fn]\\)\\|package\\|readonly\\|s\\(?:elective\\|ignal\\|t\\(?:ate\\|ruct\\)\\)\\|t\\(?:o\\|ype\\(?:Collection\\|def\\)\\)\\|\\(?:un\\|vers\\)ion\\)\\>" . font-lock-keyword-face) ; keywords

   '("\\<\\(?:B\\(?:oolean\\|yteBuffer\\)\\|Double\\|Float\\|Int\\(?:16\\|32\\|64\\|8\\)\\|String\\|UInt\\(?:16\\|32\\|64\\|8\\)\\)\\>" . font-lock-type-face) ; primitive types

   )
  "Keyword highlighting for Franca IDL mode.")

(defun franca-idl-indent-line ()
  "Indent current line as Franca IDL code.

fidl will be indented like:

package org.franca.examples.demo

import org.franca.examples.demo.BasicTypes.* from \"basic_types.fidl\"

<** @description : Interface providing common playback functionality
                   (applicable to currently active player).         **>
interface PlayerAPI {
    versions { major 5 minor 0 }

    <** @description : Currently active player. All other players will rejected any requests. **>
    attribute tPlayer activePlayer

    <** @description : Prior to using a player, it has to be activated using this request. **>
    method setActivePlayer {
        in { tPlayer player }
        out { tResultCode resultCode }
    }

    <** @description : Response for attaching an ouput device to a control context. **>
    broadcast attachOutput {
        out {
            tOutputInfoList outputInfoList
            tResultCode resultCode
        }
    }
 ...
"
  (interactive)
  (beginning-of-line)
  (if (bobp) (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at-string "}")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0) (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at-string "}")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at-string "{")
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(defun looking-at-string (string)
  (condition-case nil
      (numberp (search-forward string)) (error nil)))


(defvar franca-idl-syntax-table
  (let ((st (make-syntax-table)))
    st)
  "Syntax table for Franca IDL mode.")

;;;###autoload
(defun franca-idl-mode()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Franca-IDL"
        major-mode 'franca-idl-mode)
  (set-syntax-table franca-idl-syntax-table)
  (use-local-map franca-idl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(franca-idl-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'franca-idl-indent-line)
  (run-hooks 'franca-idl-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fidl\\'" . franca-idl-mode))

(provide 'franca-idl-mode)
