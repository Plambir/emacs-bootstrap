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
  ))

;;;; java script
(add-hook 'js2-mode-hook '(lambda () (ac-js2-mode)
                            (flycheck-mode)))
; (setq ac-js2-external-libraries '("full/path/to/a-library.js"))

;;;; octave
(require 'ac-octave)
(add-to-list 'ac-modes 'octave-mode)
(add-hook 'octave-mode-hook
          '(lambda () (setq ac-sources
                            (append '(ac-source-octave) ac-sources))))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;;; LaTeX
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)
(add-hook 'LaTeX-mode-hook '(lambda ()
                              (LaTeX-math-mode)
                              (flyspell-mode)
                              (visual-line-mode)
                              (setq ac-sources
                                    (append '(ac-source-math-unicode
                                              ac-source-math-latex
                                              ac-source-latex-commands)
                                            ac-sources))))

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
