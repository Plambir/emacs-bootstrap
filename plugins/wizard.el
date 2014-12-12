;;; wizard.el --- Wizard for move and edit

;; Copyright (C) 2014 Alexander Prusov

;; Author: Alexander Prusov <alexprusov@gmail.com>
;; Version: 1.0.1
;; Created: 13.11.2014
;; Keywords: edit vi
;; Homepage: https://github.com/Plambir/emacs-bootstrap

;;; Commentary:
;; `wizard-move-to-char'      - afoo|bar -> a -> afooba|r
;; `wizard-move-back-to-char' - afoo|bar -> a -> |afoobar
;; `wizard-up-to-char'      - afoo|bar -> a -> afoob|ar
;; `wizard-up-back-to-char' - afoo|bar -> a -> a|foobar
;; `wizard-repeat-move-or-up' - repeat last move and up command
;;
;; `wizard-zap-to-char'         - afoo|bar -> a -> afoo|r
;; `wizard-zap-back-to-char'    - afoo|bar -> a -> |bar
;; `wizard-zap-up-to-char'      - afoo|bar -> a -> afoo|ar
;; `wizard-zap-up-back-to-char' - afoo|bar -> a -> a|bar

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
;; 1.0.1 - Improve code
;; 1.0.0 - Initial version

;;; Code:
(require 'cl-lib)

(defvar wizard-mode-map (make-sparse-keymap)
  "Wizard mode map.")

(defvar-local wizard-last-command nil)

(defvar-local wizard-last-char nil)

(defun wizard-local-get-char-from-arg (char &optional no-save-char)
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (if (not no-save-char) (setq wizard-last-char char))
  char)

(defun wizard-move-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg) (read-char "Move to char: " t)))
  (setq char (wizard-local-get-char-from-arg char))
  (setq wizard-last-command 'wizard-move-to-char)
  (search-forward (char-to-string char) (if (>= arg 0) (line-end-position) (line-beginning-position)) t arg))

(defun wizard-move-back-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg) (read-char "Move back to char: " t)))
  (setq char (wizard-local-get-char-from-arg char))
  (setq wizard-last-command 'wizard-move-back-to-char)
  (search-backward (char-to-string char) (if (>= arg 0) (line-beginning-position) (line-end-position)) t arg))

(defun wizard-up-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg) (read-char "Up to char: " t)))
  (setq char (wizard-local-get-char-from-arg char))
  (setq wizard-last-command 'wizard-up-to-char)
  (if (/= (point) (line-end-position))
      (progn
        (forward-char)
        (search-forward (char-to-string char) (if (>= arg 0) (line-end-position) (line-beginning-position)) t arg)
        (backward-char))))

(defun wizard-up-back-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg) (read-char "Up back to char: " t)))
  (setq char (wizard-local-get-char-from-arg char))
  (setq wizard-last-command 'wizard-up-back-to-char)
  (if (/= (point) (line-beginning-position))
      (progn
        (backward-char)
        (search-backward (char-to-string char) (if (>= arg 0) (line-beginning-position) (line-end-position)) t arg)
        (forward-char))))

(defun wizard-repeat-move-or-up (arg)
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (if (and wizard-last-char wizard-last-command)
      (funcall wizard-last-command arg wizard-last-char)))

(defun wizard-zap-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg) (read-char "Move to char: " t)))
  (setq char (wizard-local-get-char-from-arg char t))
  (kill-region
   (point)
   (progn
     (search-forward (char-to-string char) (if (>= arg 0) (line-end-position) (line-beginning-position)) t arg)
     (point))))

(defun wizard-zap-back-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg) (read-char "Move to char: " t)))
  (setq char (wizard-local-get-char-from-arg char t))
  (kill-region
   (point)
   (progn
     (search-backward (char-to-string char) (if (>= arg 0) (line-beginning-position) (line-end-position)) t arg)
     (point))))

(defun wizard-zap-up-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg) (read-char "Up to char: " t)))
  (setq char (wizard-local-get-char-from-arg char))
  (setq wizard-last-command 'wizard-up-to-char)
  (if (/= (point) (line-end-position))
      (kill-region (point)
                   (progn
                     (forward-char)
                     (search-forward (char-to-string char) (if (>= arg 0) (line-end-position) (line-beginning-position)) t arg)
                     (backward-char)
                     (point)))))

(defun wizard-zap-up-back-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg) (read-char "Up back to char: " t)))
  (setq char (wizard-local-get-char-from-arg char))
  (setq wizard-last-command 'wizard-up-back-to-char)
  (if (/= (point) (line-beginning-position))
      (kill-region (point)
                   (progn
                     (backward-char)
                     (search-backward (char-to-string char) (if (>= arg 0) (line-beginning-position) (line-end-position)) t arg)
                     (forward-char)
                     (point)))))

(define-key wizard-mode-map (kbd "C-c f")   'wizard-move-to-char)
(define-key wizard-mode-map (kbd "C-c F")   'wizard-move-back-to-char)
(define-key wizard-mode-map (kbd "C-c t")   'wizard-up-to-char)
(define-key wizard-mode-map (kbd "C-c T")   'wizard-up-back-to-char)
(define-key wizard-mode-map (kbd "C-c d f") 'wizard-zap-to-char)
(define-key wizard-mode-map (kbd "C-c d F") 'wizard-zap-back-to-char)
(define-key wizard-mode-map (kbd "C-c d t") 'wizard-zap-up-to-char)
(define-key wizard-mode-map (kbd "C-c d T") 'wizard-zap-up-back-to-char)
(define-key wizard-mode-map (kbd "C-c z")   'wizard-repeat-move-or-up)

(defun wizard-minor-mode-not-for-minibuffer ()
  (if (not (minibufferp (current-buffer)))
      (wizard-minor-mode t)
    nil))

;;;###autoload
(define-minor-mode wizard-minor-mode
  "Wizard mode."
  :keymap wizard-mode-map
  :group wizard)

;;;###autoload
(define-globalized-minor-mode global-wizard-minor-mode
  wizard-minor-mode
  wizard-minor-mode-not-for-minibuffer
  :group 'wizard)

(provide 'wizard)
;;; wizard.el ends here
