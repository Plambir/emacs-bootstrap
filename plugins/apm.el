;;; apm.el --- Another project manager for emacs

;; Copyright (C) 2014 Alexander Prusov

;; Author: Alexander Prusov <alexprusov@gmail.com>
;; Version: 2.4.2
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
;;                                        :global-vars '((setq compile-command "make -k")
;;                                                       (setq other-settings t))
;;                                        :local-vars '((nil . ((first-settings . nil)
;;                                                              (second-settings . t))))
;;                                        :project-files '("*.cpp" "*.h" "Makefile" "README.md")
;;                                        :ignore-dirs '("*build*" "*bin*" "*assets*")
;;                                        )))
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
;; 2.4.2 - Improve `apm-find-project'
;; 2.4.1 - Improve `apm-find-file-in-project'
;; 2.4.0 - Add more features for `apm-find-file-in-project'
;; 2.3.2 - Improve apm-find-file-in-project
;; 2.3.1 - Minor fix
;; 2.3.0 - Replace find-file if you in project
;; 2.2.5 - Remove ugly code
;; 2.2.4 - Improve code
;; 2.2.3 - Remove irony autoload .clang_complete
;;         (use irony-cdb-autosetup-compile-options in hooks)
;; 2.2.2 - Support new version irony-mode
;; 2.2.1 - Refactoring
;; 2.2.0 - Add support irony-mode (autoload .clang_complete file from project directory)
;; 2.1.2 - Refactoring
;; 2.1.1 - Fix apply global vars. Fix multiple apply local vars.
;; 2.1.0 - Add `global-vars' for change global emacs settings
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

(defvar apm-project-local-vars-is-set nil)

(defvar apm-find-args "-type f -printf '%P\\n'"
  "Find args for `apm-find-file-in-project'.")

(defcustom apm-replace-find-file t
  "If non-nil apm mode replace binding `C-x C-f' on `apm-find-file-in-project'
and bind C-u C-x C-f on default `find-file' function."
  :type 'boolean
  :group 'apm)

(defcustom apm-default-ignore-files '(".*"
                                      "*~"
                                      "\#*\#"
                                      "*.elc"
                                      "*.o"
                                      "*.ko"
                                      "*.obj"
                                      "*.elf"
                                      "*.gch"
                                      "*.pch"
                                      "*.lib"
                                      "*.a"
                                      "*.la"
                                      "*.lo"
                                      "*.dll"
                                      "*.so"
                                      "*.so.*"
                                      "*.dylib"
                                      "*.exe"
                                      "*.out"
                                      "*.app"
                                      "*.i*86"
                                      "*.x86_64"
                                      "*.slo"
                                      "*.so"
                                      "*.mod"
                                      "*.lai"
                                      "*.hex"
                                      "*.py[cod]"
                                      "*.mo"
                                      "*.pot"
                                      "*.log"
                                      "*.class"
                                      "*.[jwe]ar"
                                      "CMakeCache.txt"
                                      "cmake_install.cmake"
                                      "install_manifest.txt"
                                      "*.pro.user"
                                      "*.pro.user.*"
                                      "*.qbs.user"
                                      "*.qbs.user.*"
                                      "*.moc"
                                      "moc_*.cpp"
                                      "qrc_*.cpp"
                                      "ui_*.h"
                                      "*.autosave"
                                      "*.qmlproject.user"
                                      "*.qmlproject.user.*"
                                      "*.aux"
                                      "*.lof"
                                      "*.log"
                                      "*.lot"
                                      "*.fls"
                                      "*.out"
                                      "*.toc"
                                      "*.dvi"
                                      "*.bbl"
                                      "*.bcf"
                                      "*.blg"
                                      "*-blx.aux"
                                      "*-blx.bib"
                                      "*.brf"
                                      "*.run.xml"
                                      "*.png"
                                      "*.jpeg"
                                      "*.jpg"
                                      "*.fla"
                                      "*.swf"
                                      "*.apk"
                                      "*.ogg"
                                      "*.ttf"
                                      )
  "Default ignore files for `apm-find-file-in-project'."
  :type '(repeat (string :tag "File mask:"))
  :group 'apm)

(defcustom apm-default-ignore-dirs '("*/.*/*"
                                     "*__pycache__/*"
                                     "*docs/_build/*"
                                     "*.xcodeproj/*"
                                     "*.xcworkspace/*"
                                     "*CMakeFiles*")
  "Default ignore directories for `apm-find-file-in-project'."
  :type '(repeat (string :tag "Path mask:"))
  :group 'apm)

(defstruct apm-project
  path             ;; project dir
  local-vars       ;; like .dir-locals.el
  open-action      ;; execute lisp code after open project from `apm-find-project'
  global-vars      ;; apply lisp code after open project from `apm-find-project'
  ignore-files     ;; local ignore files for `apm-find-file-in-project'
  ignore-dirs      ;; local ignore dirs for `apm-find-file-in-project'
  project-files    ;; show only this files in `apm-find-file-in-project'
  project-dirs)    ;; show only this dirs in `apm-find-file-in-project'

(defun apm--find-project (path)
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

(defun apm--get-project-dir (path)
  (let ((project (apm--find-project path)))
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
    (setq default-directory (concat (apm--get-project-dir default-directory) "/"))
    (compile command comint)))

(defun apm-compile-close ()
  (interactive)
  (when (get-buffer "*compilation*")
    (delete-windows-on (get-buffer "*compilation*"))
    (kill-buffer "*compilation*")))

(defun apm--get-projects-path ()
  (let ((path '())
        (projects apm-projects))
    (while projects
      (setq path (cons (expand-file-name (apm-project-path (eval (car projects)))) path))
      (setq projects (cdr projects)))
    (nreverse path)))

(defun apm-find-project ()
  (interactive)
  (let ((projects-list (apm--get-projects-path)))
    (with-temp-buffer
      (setq default-directory (completing-read "Project: " projects-list nil t nil nil (car projects-list)))
      (let ((project (apm--find-project default-directory)))
        (when project
          (apm--apply-global-vars)
          (let ((open-action (apm-project-open-action project)))
            (if open-action
                (if (listp open-action)
                    (eval open-action)
                  (call-interactively open-action))
              (call-interactively 'find-file))))))))

(defun apm--change-safe-local-variable-values (local-vars action)
  (let ((vars local-vars))
    (while vars
      (if (stringp (car (car vars)))
          (apm--change-safe-local-variable-values (cdr (car vars)) action)
        (let ((change-local-vars (cdr (car vars))))
          (while change-local-vars
            (setq safe-local-variable-values (funcall action (car change-local-vars) safe-local-variable-values))
            (setq change-local-vars (cdr change-local-vars)))))
      (setq vars (cdr vars)))))

(defun apm--set-local-vars (isset)
  (if (not (equal apm-project-local-vars-is-set isset))
      (let ((projects apm-projects))
        (while projects
          (let ((project (eval (car projects))))
            (let ((project-path (expand-file-name (apm-project-path project)))
                  (local-vars (apm-project-local-vars project)))
              (let ((dir-var-name (concat
                                   "apm-dir-locals-"
                                   (replace-regexp-in-string "[^a-zA-Z]" "" project-path)
                                   "-variables")))
                (if (and isset local-vars)
                    (progn
                      (dir-locals-set-class-variables (intern dir-var-name) local-vars)
                      (dir-locals-set-directory-class project-path (intern dir-var-name))
                      (apm--change-safe-local-variable-values local-vars 'cons))
                  (dir-locals-set-class-variables (intern dir-var-name) '())
                  (dir-locals-set-directory-class project-path (intern dir-var-name))
                  (apm--change-safe-local-variable-values local-vars 'delete)))))
          (setq projects (cdr projects)))
        (setq apm-project-local-vars-is-set isset))))

(defun apm--format-find-flags (name-or-path logic flag-list &optional set-not)
  (concat (if set-not "-not") " -" name-or-path " '" (car flag-list) "' "
          (let (flags)
            (setq flag-list (cdr flag-list))
            (dolist (element flag-list flags)
              (setq flags (concat flags (concat "-" logic " " (if set-not "-not") " -" name-or-path " '" element "' ")))))))

(defun apm--ignore-flags (&optional project)
  (let ((files apm-default-ignore-files)
        (dirs apm-default-ignore-dirs))
    (if (or files
            dirs
            (and project (apm-project-ignore-files project))
            (and project (apm-project-ignore-dirs project)))
        (concat
         " \\( "
         (apm--format-find-flags "name" "and" files t)
         (apm--format-find-flags "path" "and" dirs t)
         (when project
           (apm--format-find-flags "name" "and" (apm-project-ignore-files project) t)
           (apm--format-find-flags "path" "and" (apm-project-ignore-dirs project) t))
         "\\) ")
      " ")))

(defun apm--show-flags (&optional project)
  (if (or (and project (apm-project-project-files project))
          (and project (apm-project-project-dirs project)))
      (concat
       " \\( "
       (apm--format-find-flags "name" "or" (apm-project-project-files project))
       (apm--format-find-flags "path" "or" (apm-project-project-dirs project))
       "\\) ")
    " "))

(defun apm--get-project-files ()
  (let ((project (apm--find-project default-directory)))
    (when project
      (with-temp-buffer
        (setq default-directory (concat (apm-project-path project) "/"))
        (split-string (shell-command-to-string (concat "PATH='" (getenv "PATH")
                                                       "' "
                                                       "find . "
                                                       (apm--ignore-flags project)
                                                       (apm--show-flags project)
                                                       apm-find-args " "))
                        "\n" t)))))

(defun apm-find-file-in-project (&optional arg)
  (interactive "P")
  (if (not apm-replace-find-file)
      (call-interactively 'find-file)
    (let ((project (apm--find-project default-directory)))
      (if (and project (not arg))
          (with-temp-buffer
            (setq default-directory (concat (apm-project-path project) "/"))
            (find-file (completing-read "Find file: " (apm--get-project-files) nil t nil nil (car (apm--get-project-files)))))
        (call-interactively 'find-file)))))

(defun apm--apply-global-vars ()
  (let ((project (apm--find-project default-directory)))
    (when project
      (with-temp-buffer
        (setq default-directory (concat (apm-project-path project) "/"))
        (let ((global-vars (apm-project-global-vars project)))
          (while global-vars
            (eval (car global-vars))
            (setq global-vars (cdr global-vars))))))))

(defun apm-minor-mode-not-for-minibuffer ()
  (when (not (minibufferp (current-buffer)))
    (apm-minor-mode t)))

;;;###autoload
(define-minor-mode apm-minor-mode
  "APM mode."
  :lighter " APM"
  :keymap apm-mode-map
  :group apm
  (if apm-minor-mode
      (apm--set-local-vars t)
    (apm--set-local-vars nil)))

(define-key apm-mode-map (kbd "C-c c") 'apm-compile)
(define-key apm-mode-map (kbd "C-c q") 'apm-compile-close)
(define-key apm-mode-map (kbd "C-c C-f") 'apm-find-project)
(define-key apm-mode-map (kbd "C-x C-f") 'apm-find-file-in-project)

;;;###autoload
(define-globalized-minor-mode global-apm-minor-mode
  apm-minor-mode
  apm-minor-mode-not-for-minibuffer
  :group 'apm)

(provide 'apm)
;;; apm.el ends here
