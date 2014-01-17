;; This is an absolute, minimal, better-then-nothing mode for joxa. At
;; some point we will build something better.

(defvar joxa-mode-hook nil)
(add-to-list 'auto-mode-alist '("\\.jxa\\'" . joxa-mode))

(defvar joxa-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\< "(>" table)
    (modify-syntax-entry ?\> ")<" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "(}" table)
    table))

(defvar joxa-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "{" 'paredit-open-curly)
    (define-key map "}" 'paredit-close-curly)
    map)
  "Joxa mode keymap")

(defconst joxa-font-lock-keywords
  (list
   (list (concat "("
                 (regexp-opt '("module"
                               "deftype"
                               "defspec"
                               "definline"
                               "defmacro+"
                               "defmacro"
                               "defn+"
                               "defn"
                               "__try"
                               "use"
                               "ns"
                               "fn") t)

                 "\\>")
         '(1 font-lock-keyword-face))
   )
  "Joxa keywords")

(define-derived-mode joxa-mode lisp-mode "Joxa Editing Mode" "Major mode for editing Joxa files"
  (interactive)
  (setq major-mode 'joxa-mode)
  (setq mode-name "Joxa")
  (set-syntax-table joxa-mode-syntax-table)
  (use-local-map joxa-mode-map)
  (font-lock-add-keywords 'joxa-mode
                          joxa-font-lock-keywords)
  (run-hooks 'joxa-mode-hook))

(provide 'joxa-mode)
