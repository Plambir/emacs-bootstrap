(defun load-if-exists (file)
  (unwind-protect (load file) nil))

(load-if-exists "~/.emacs.d/local")

;;;; Auto Insert
(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-alist
	  '(
		;; C/C++ Header
		((".*\\.\\(h\\|hpp\\)$" . "C/C++ Header")
		 (upcase
		  (concat
		   (file-name-nondirectory
			(file-name-sans-extension buffer-file-name))
		   "_"
		   (file-name-extension buffer-file-name)
		   "__"))
		 "#ifndef " str "\n#define " str "\n\n" _ "\n\n#endif /* " str " */"
		)
		;; shell
		((sh-mode . "Shell mode")
		  nil
		  "#!/bin/sh\n"
		  _
		)
		;; Python
		((".*\\.py$" . "Python")
		 nil
		 "# -*- coding: utf-8 -*-\n\n"
		 _
		 )
    ;; tern_project
    (("\\.tern-project$" . "js2-mode")
     nil
"{
  \"libs\": [
    \"browser\"
  ],
  \"loadEagerly\" : [
  ],
  \"plugins\": {
    \"requirejs\": {
      \"baseURL\": \"./\",
      \"paths\": {}
    }
  }
}"
_)
  ))

;;;; java script
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.tern-project\\'" . js2-mode))
(add-hook 'js2-mode-hook '(lambda ()
                            (flycheck-mode t)
                            (tern-mode t)
                            (setq tab-width 2)))

(add-hook 'js2-mode-hook  'skewer-mode)
(add-hook 'css-mode-hook  'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(setq company-tern-property-marker "")
(setq company-tern-meta-as-single-line t)

;;;; php
(add-hook 'php-mode-hook '(lambda ()
                            (setq c-basic-offset 2)))

;;;; octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;;; LaTeX
(add-hook 'LaTeX-mode-hook '(lambda ()
                              (LaTeX-math-mode)
                              (flyspell-mode)
                              (visual-line-mode)))

;;;; C/C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; This hack fixes indentation for C++11's "enum class" in Emacs.
;; http://stackoverflow.com/questions/6497374/emacs-cc-mode-indentation-problem-with-c0x-enum-class/6550361#6550361

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

(defun c-irony-on ()
  (if (member major-mode '(c++-mode c-mode objc-mode))
      (progn
        (irony-mode t)
        (irony-cdb-autosetup-compile-options))))

(add-hook 'c++-mode-hook 'c-irony-on)
(add-hook 'c-mode-hook 'c-irony-on)
(add-hook 'objc-mode-hook 'c-irony-on)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
