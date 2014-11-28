;;; apm.el --- Another project manager for emacs

;; Copyright (C) 2014 Alexander Prusov

;; Author: Alexander Prusov <alexprusov@gmail.com>
;; Version: 2.0.0
;; Created: 7.11.2014
;; Keywords: project
;; Homepage: https://github.com/Plambir/emacs-bootstrap

;;; Commentary:
;; Simple project manager for setup compile comand and open project directory.
;;
;; Use `apm-find-project' for open project and up settings.
;; Use `apm-compile' for compile opened project.
;; Use `apm-minor-mode' for keymap settings.
;; For add project use code like this:
;; (setq apm-projects '((make-apm-project :path "~/path/to/project"
;;                                        :open-action '(find-file "TODO.txt")
;;                                        :local-vars '((nil . ((compile-command . "make -k")
;;                                                              (other-settings . t)))))))
;; By default open-action is `ido-find-file'
;; See how to setup local-vars in documentation for `dir-locals-set-class-variables'

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; The MIT License (MIT)
;;
;; Copyright (c) 2014 Alexander Prusov
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Change Log:
;; 2.0.0 - Use `dir-locals-set-directory-class' for up project settings
;; 1.1.0 - Use minor mode only for keymap
;; 1.0.6 - Fix up settings
;; 1.0.5 - Up settings in find project
;; 1.0.4 - Fix apply settings.
;; 1.0.3 - Fix compilation
;; 1.0.2 - Initializing `apm-projects' via defvar
;; 1.0.1 - Add example of settings
;; 1.0.0 - Initial version

;;; Code:
(require 'cl-lib)
(require 'ido)

(defvar apm-mode-map (make-sparse-keymap)
  "APM mode map.")

(defvar apm-projects '()
  "You projects")

(defstruct apm-project path local-vars open-action)

(defun apm-local-find-project (path)
  (let ((path (expand-file-name path))
        (projects apm-projects)
        (result nil)
        (proj-path ""))
    (while (and (not result) projects)
      (setq proj-path (expand-file-name (apm-project-path (eval (car projects)))))
      (if (string-prefix-p proj-path path)
          (setq result (car projects))
        (setq projects (cdr projects))))
    (if (not result)
        nil
      (eval result))))

(defun apm-local-get-project-dir (path)
  (let ((project (apm-local-find-project path)))
        (if project
            (apm-project-path project)
          path)))

(defun apm-compile (command &optional comint)
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (with-temp-buffer
    (setq default-directory (concat (apm-local-get-project-dir default-directory) "/"))
    (compile command comint)))

(defun apm-compile-close ()
  (interactive)
  (if (get-buffer "*compilation*")
      (progn
        (delete-windows-on (get-buffer "*compilation*"))
        (kill-buffer "*compilation*"))))

(defun apm-local-get-projects-path ()
  (let ((path '())
        (projects apm-projects))
    (while projects
      (setq path (cons (expand-file-name (apm-project-path (eval (car projects)))) path))
      (setq projects (cdr projects)))
    (nreverse path)))

(defun apm-find-project ()
  (interactive)
  (let ((projects-list (apm-local-get-projects-path)))
    (with-temp-buffer
      (setq default-directory
            (if ido-mode
                (ido-completing-read "Project: " projects-list)
                (completing-read "Project: " projects-list)))
      (let ((project (apm-local-find-project default-directory)))
        (if project
            (progn
              (let ((open-action (apm-project-open-action project)))
                (if open-action
                    (if (listp open-action)
                        (eval open-action)
                      (call-interactively open-action))
                  (if ido-mode
                      (ido-find-file)
                    (call-interactively 'find-file))))))))))

(defun apm-local-set-local-vars (isset)
  (let ((projects apm-projects))
    (while projects
      (let ((project (eval (car projects))))
        (let ((project-path (expand-file-name (apm-project-path project)))
              (local-vars (apm-project-local-vars project)))
          (let ((dir-var-name (concat "apm-dir-locals-" (replace-regexp-in-string "[^a-zA-Z]" "" project-path) "-variables")))
            (if (and isset local-vars)
                (progn
                  (dir-locals-set-class-variables (intern dir-var-name) local-vars)
                  (dir-locals-set-directory-class project-path (intern dir-var-name))
                  (let ((add-local-vars (cdr (car local-vars))))
                  (while add-local-vars
                    (setq safe-local-variable-values (cons (car add-local-vars) safe-local-variable-values))
                    (setq add-local-vars (cdr add-local-vars)))))
              (progn
                (dir-locals-set-class-variables (intern dir-var-name) '())
                (dir-locals-set-directory-class project-path (intern dir-var-name))
                (let ((rm-local-vars (cdr (car local-vars))))
                  (while rm-local-vars
                    (setq safe-local-variable-values (delete (car rm-local-vars) safe-local-variable-values))
                    (setq rm-local-vars (cdr rm-local-vars))))
                )))))
      (setq projects (cdr projects)))))

;;;###autoload
(define-minor-mode apm-minor-mode
  "APM mode."
  :lighter " APM"
  :keymap apm-mode-map
  :group apm
  (if apm-minor-mode
      (apm-local-set-local-vars t)
    (apm-local-set-local-vars nil)))

(define-key apm-mode-map (kbd "C-c c") 'apm-compile)
(define-key apm-mode-map (kbd "C-c q") 'apm-compile-close)
(define-key apm-mode-map (kbd "C-c C-f") 'apm-find-project)

;;;###autoload
(define-globalized-minor-mode global-apm-minor-mode
  apm-minor-mode
  (lambda ()
    (if (not (minibufferp (current-buffer)))
        (apm-minor-mode t)
      nil))
  :group 'apm)

(provide 'apm)
;;; apm.el ends here
