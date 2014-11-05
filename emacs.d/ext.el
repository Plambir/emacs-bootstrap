(defun load-if-exists (file)
  (when (file-exists-p file)
    (load file)))

(load-if-exists "~/.emacs.d/local.el")
