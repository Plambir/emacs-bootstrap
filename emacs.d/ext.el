(defun load-if-exists (file)
  (when (file-exists-p file)
    (load file)))

(load-if-exists "~/.emacs.d/local.el")

;;;; Auto Insert
(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert 'other)
(setq auto-insert-query nil)
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
