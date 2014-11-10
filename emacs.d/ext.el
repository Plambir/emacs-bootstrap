(defun load-if-exists (file)
  (unwind-protect (load "~/.emacs.d/local") nil))

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
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
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
