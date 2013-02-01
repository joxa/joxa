;; This is an absolute, minimal, better-than-nothing mode for joxa. At
;; some point we will build something better.

(define-derived-mode joxa-mode lisp-mode
  "Joxa Editing Mode" "Major mode for editing Joxa files"

  (font-lock-add-keywords 'joxa-mode
                          '(("module" . font-lock-keyword-face)
                            ("deftype" . font-lock-keyword-face)
                            ("defspec" . font-lock-keyword-face)
                            ("definline" . font-lock-keyword-face)
                            ("defmacro+" . font-lock-keyword-face)
                            ("defmacro" . font-lock-keyword-face)
                            ("defn+" . font-lock-keyword-face)
                            ("defn" . font-lock-keyword-face)
                            ("__try" . font-lock-keyword-face)
                            ("use" . font-lock-keyword-face)
                            ("ns" . font-lock-keyword-face)
                            ("fn" . font-lock-keyword-face)))

  (define-key joxa-mode-map "{" 'paredit-open-curly)
  (define-key joxa-mode-map "}" 'paredit-close-curly)

  (modify-syntax-entry ?< "(>" )
  (modify-syntax-entry ?> ")<" )

  (modify-syntax-entry ?[ "(]" )
  (modify-syntax-entry ?] ")[" )

  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){"))


;; Uncomment this if you want parenface and paredit (which you should)
;; (add-hook 'joxa-mode-hook '(lambda ()
;;                              (paredit-mode)
;;                              (require 'parenface)
;;                              (set-face-foreground 'paren-face "#073642")))

(add-to-list 'auto-mode-alist '("\\.jxa\\'" . joxa-mode))

(provide 'joxa)
