;;; apm.el --- Another project manager for emacs

;; Copyright (C) 2014 Alexander Prusov

;; Author: Your Name <yourname@example.com>
;; Version: 1.0.1
;; Maintainer: Someone Else <someone@example.com>
;; Created: 14 Jul 2010
;; Keywords: project
;; Homepage: https://github.com/Plambir/emacs-bootstrap

;;; Commentary:
;; Simple project manager for setup compile comand and open project directory.
;;
;; Use `apm-find-project' for open project.
;; Use `apm-compile' for compile opened project.
;; Use `apm-minor-mode' for auto up setting for project files.
;; Project files - it's always files from project dir.
;; For add project use code like this:
;; (setq apm-projects '((make-apm-project :path "~/path/to/project"
;;                                        :settings '((setq compile-command "make -k")
;;                                                    (setq other-settings t)))))

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
;; 1.0.1 - Add example of settings
;; 1.0.0 - Initial version

;;; Code:
(require 'cl-lib)
(require 'ido)

(defvar apm-mode-map (make-sparse-keymap)
  "APM mode map.")

(defstruct apm-project path settings)

(setq-default apm-projects '())

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

(defun apm-local-apply-settings ()
  (let ((project (apm-local-find-project default-directory)))
    (if project
        (let ((settings (apm-project-settings project)))
          (while settings
            (unwind-protect (eval (car settings)) (setq settings '()))
            (setq settings (cdr settings)))))))

(defun apm-compile (command &optional comint)
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (with-temp-buffer
    (setq default-directory (apm-local-get-project-dir default-directory))
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
    path))

(defun apm-find-project ()
  (interactive)
  (let ((projects-list (apm-local-get-projects-path)))
    (cd (ido-completing-read "Project: " projects-list))
    (ido-find-file)))

;;;###autoload
(define-minor-mode apm-minor-mode
  "APM mode."
  :lighter " APM"
  :keymap apm-mode-map
  :group apm
  (if apm-minor-mode
      (add-hook 'change-major-mode-hook 'apm-local-apply-settings nil t)
    (remove-hook 'change-major-mode-hook 'apm-local-apply-settings t)))


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
