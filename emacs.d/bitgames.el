;;;; meta
(setq myKeywords
      '(("^struct\\>\\|^end\\>\\|\\<extends\\>\\|^enum\\>\\|^func\\>" . font-lock-keyword-face)
        ("\\<.?int\\(8\\|16\\|32\\|64\\)\\>\\|\\<string\\>\\|\\<float\\>\\|\\<double\\>|\\<bool\\>" . font-lock-type-face)
        ("@cs_\\w*:*" . font-lock-function-name-face)
        ("@flt_\\w*:*" . font-lock-function-name-face)
        ("@\\w*:*" . font-lock-function-name-face)
        ("#include" . font-lock-preprocessor-face)
        ("#.*" . font-lock-comment-face)
        )
      )

(defvar bit-meta-imenu-expression
  '(("struct"    "^struct *\\(.*\\)" 1)
    ("enum"      "^enum *\\(.*\\)"   1)
    ("func"      "^func *\\(.*\\)"   1)))

(define-derived-mode bit-meta-mode prog-mode
  (setq font-lock-defaults '(myKeywords))
  (setq mode-name "bit meta")
  (set (make-local-variable 'imenu-generic-expression) bit-meta-imenu-expression)
  (setq tab-width 2))

(add-to-list 'auto-mode-alist '("\\.meta\\'" . bit-meta-mode))

;;;; bhl
(define-derived-mode bhl-mode go-mode
  (setq mode-name "bhl mode")
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

(add-to-list 'auto-mode-alist '("\\.bhl\\'" . bhl-mode))

;;;; configs
(setq myKeywords-conf
      '(
        ("^def\\>\\|^end\\>" . font-lock-keyword-face)
        ("$\\(\\w\\|_\\)*" . font-lock-type-face)
        ("<%\\ ?\\(\\w\\|_\\)*\\|%>" . font-lock-preprocessor-face)
        )
      )

(defvar bit-meta-conf-imenu-expression
  '(("def"        "^def *\\(.*\\)" 1)))

(define-derived-mode bit-meta-conf-mode c++-mode
  (setq font-lock-defaults '(myKeywords-conf))
  (setq mode-name "bit meta conf")
  (setq-local electric-indent-mode nil)
  (setq comment-start "//"
        comment-end   "")
  (set (make-local-variable 'imenu-generic-expression) bit-meta-conf-imenu-expression)
  (setq tab-width 2))

(add-to-list 'auto-mode-alist '("\\.conf\\.js\\'" . bit-meta-conf-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\.js\\'" . bit-meta-conf-mode))
