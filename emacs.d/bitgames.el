;;;; meta
(defvar bit-meta-keyworlds
      '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-comment-face)
        ("^struct\\>\\|^end\\>\\|\\<extends\\>\\|^enum\\>\\|^func\\>" . font-lock-keyword-face)
        ("\\<.?int\\(8\\|16\\|32\\|64\\)\\>\\|\\<string\\>\\|\\<float\\>\\|\\<double\\>\\|\\<bool\\>" . font-lock-type-face)
        ("@[a-zA-Z0-9_]*:*" . font-lock-function-name-face)
        ("#include" . font-lock-preprocessor-face)
        ("#.*" . font-lock-comment-face)))

(defvar bit-meta-imenu-expression
  '(("struct"    "^struct *\\(.*\\)" 1)
    ("enum"      "^enum *\\(.*\\)"   1)
    ("func"      "^func *\\(.*\\)"   1)))

(define-derived-mode bit-meta-mode prog-mode
  (setq font-lock-defaults '(bit-meta-keyworlds))
  (setq mode-name "bit meta")
  (set (make-local-variable 'imenu-generic-expression) bit-meta-imenu-expression))

(add-to-list 'auto-mode-alist '("\\.meta\\'" . bit-meta-mode))

;;;; bhl
(define-derived-mode bhl-mode go-mode
  (setq mode-name "bhl mode")
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

(add-to-list 'auto-mode-alist '("\\.bhl\\'" . bhl-mode))

;;;; configs
(add-to-list 'auto-mode-alist '("\\.conf\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\.js\\'" . js-mode))
