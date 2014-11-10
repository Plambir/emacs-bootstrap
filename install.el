;; Compile startup scripts
(byte-recompile-directory "~/.emacs.d")

(package-install-file (concat (file-name-directory (expand-file-name (buffer-file-name))) "plugins/apm.el"))

(defun byte-compile-file-if-need (file)
  (when (file-exists-p file)
    (when (not (file-exists-p (concat file "c")))
      (byte-compile-file file))))

(byte-compile-file-if-need "~/.emacs.d/init.el")
(byte-compile-file-if-need "~/.emacs.d/customize.el")
(byte-compile-file-if-need "~/.emacs.d/ext.el")
(byte-compile-file-if-need "~/.emacs.d/local.el")

(kill-emacs)
