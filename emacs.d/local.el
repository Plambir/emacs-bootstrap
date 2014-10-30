(defvar-local bit-meta-keywords nil)

(setq bit-meta-keywords
      '(("\\<struct\\>\\|\\<end\\>\\|\\<extends\\>" . font-lock-keyword-face)
   ("\\<.?int\\(8\\|16\\|32\\)\\>\\|\\<string\\>" . font-lock-type-face)
   ("@\\w*" . font-lock-function-name-face)
   ("#include" . font-lock-preprocessor-face)
  )
)

(define-derived-mode bit-meta-mode fundamental-mode
  (setq font-lock-defaults '(bit-meta-keywords))
  (setq mode-name "bit meta")
)

(add-to-list 'auto-mode-alist '("\\.meta\\'" . bit-meta-mode))
