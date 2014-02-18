;; Joxa mode

(defvar joxa-mode-hook nil)

;; syntax table
(defvar joxa-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?` "'  " table)
    (modify-syntax-entry ?' "'  " table)
    table))

;; mode map
(defvar joxa-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map) map))

;; syntax highlighting
(defvar joxa-font-lock-keywords
  (eval-when-compile
    `(
      ;; keywords
      (,(concat "("
                (regexp-opt '("use" "fn" "do" "case" "when"
                              "try" "try*" "catch" "require"
                              "receive" "let" "let*") t)
                "\\>")
       (1 font-lock-keyword-face))

      ;; namespace
      (,(concat "(" (regexp-opt '("ns") t) "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face nil t))

      ;; BIFs
      (,(concat "("
                (regexp-opt '("$filename" "$namespace"
                              "$line-number" "$function-name"
                              "apply" "quote" "string" "list"
                              "tuple" "binary" "error") t)
                "\\>")
       (1 font-lock-builtin-face))

      ;; functions
      (,(concat "("
                (regexp-opt '("definline" "defn+" "defn") t)
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                "\\(\\sw+\\)?")

       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))

      ;; type/spec
      (,(concat "("
                (regexp-opt '("deftype" "deftype+" "defspec") t)
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-type-face nil t))

      ;; macros
      (,(concat "(" (regexp-opt '("defmacro" "defmacro+") t) "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-constant-face))

      ;; variables
      (,(concat "(" (regexp-opt '("define") t) "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face))

      ;; atoms
      ("\\:\\(\\sw\\|\\s_\\)+\\(\\>\\|\\_>\\)" 0 font-lock-constant-face)

      )))

(define-derived-mode joxa-mode lisp-mode "Joxa Editing Mode" "Major mode for editing Joxa files"
  (interactive)
  (setq major-mode 'joxa-mode)
  (setq mode-name "Joxa")
  (set-syntax-table joxa-mode-syntax-table)
  (use-local-map joxa-mode-map)
  (setq font-lock-defaults '(joxa-font-lock-keywords
                             nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
                             (font-lock-mark-block-function . mark-defun)
                             (font-lock-syntactic-face-function
                              . lisp-font-lock-syntactic-face-function)))
  ;; enable show-paren-mode
  (make-variable-buffer-local 'show-paren-mode)
  (show-paren-mode t)
  (run-hooks 'joxa-mode-hook))

(put 'ns 'lisp-indent-function 1)
(put 'definline 'lisp-indent-function 'defun)
(put 'defn 'lisp-indent-function 'defun)
(put 'defn+ 'lisp-indent-function 'defun)
(put 'fn 'lisp-indent-function 1)
(put 'defmacro 'doc-string-elt 3)
(put 'defmacro+ 'doc-string-elt 3)
(put 'define 'doc-string-elt 3)
(put 'deftype 'lisp-indent-function 'defun)
(put 'deftype+ 'lisp-indent-function 'defun)
(put 'defspec 'lisp-indent-function 'defun)
(put 'receive 'lisp-indent-function 0)
(put 'try* 'lisp-indent-function 0)
(put 'catch 'lisp-indent-function 1)
(put 'do 'lisp-indent-function 0)
(put 'use 'lisp-indent-function 0)
(put 'require 'lisp-indent-function 0)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jxa\\'" . joxa-mode))

(provide 'joxa-mode)
