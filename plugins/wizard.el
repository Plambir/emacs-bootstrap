;;; wizard.el --- Wizard for move and edit

;; Copyright (C) 2014 Alexander Prusov

;; Author: Alexander Prusov <alexprusov@gmail.com>
;; Version: 1.0.0
;; Created: 13.11.2014
;; Keywords: project
;; Homepage: https://github.com/Plambir/emacs-bootstrap

;;; Commentary:
;; `wizard-move-to-char'      - afoo|bar -> a -> afooba|r
;; `wizard-move-to-back-char' - afoo|bar -> a -> |afoobar
;; `wizard-role-to-char'      - afoo|bar -> a -> afoob|ar
;; `wizard-role-to-back-char' - afoo|bar -> a -> a|foobar
;; `wizard-repeat-move-or-role' - repeat last move and role command

;; `wizard-zap-to-char'       - afoo|bar -> a -> afoo|r
;; `wizard-zap-to-back-char'  - afoo|bar -> a -> |bar
;; `wizard-zap-inner-pair'    - h(fo|o)h -> ( or ) -> h()h
;; `wizard-zap-append-pair'   - h(fo|o)h -> ( or ) -> hh
;; `wizard-zap-inner-tag'     - h<hello>fo|o</hello>h -> hello -> h<hello>|</hello>h
;; `wizard-zap-append-tag'    - h<hello>fo|o</hello>h -> hello -> hh

;; `wizard-mark-word'         - mark word at point
;; `wizard-mark-inner-pair' and `wizard-mark-append-pair' selected text in/with pair
;; `wizard-mark-inner-tag' and `wizard-mark-append-tag' selected text in/with tag

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
;; 1.0.0 - Initial version

;;; Code:
(require 'cl-lib)

(defvar wizard-mode-map (make-sparse-keymap)
  "Wizard mode map.")

(defvar-local wizard-last-command nil)

(defvar-local wizard-last-char nil)

(defun wizard-move-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg) (read-char "Move to char: " t)))
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (setq wizard-last-char char)
  (setq wizard-last-command 'wizard-move-to-char)
  (search-forward (char-to-string char) (if (>= arg 0) (line-end-position) (line-beginning-position)) nil arg))

(defun wizard-move-to-back-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg) (read-char "Move back to char: " t)))
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (setq wizard-last-char char)
  (setq wizard-last-command 'wizard-move-to-back-char)
  (search-backward (char-to-string char) (if (>= arg 0) (line-beginning-position) (line-end-position)) nil arg))

(defun wizard-repeat-move-or-role (arg)
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (if (and wizard-last-char wizard-last-command)
      (funcall wizard-last-command arg wizard-last-char)))

(define-key global-map (kbd "C-; C-t") nil)
(define-key global-map (kbd "C-; C-t") 'wizard-move-to-back-char)
(define-key global-map (kbd "C-; C-z") 'wizard-repeat-move-or-role)

;;;###autoload
(define-minor-mode wizard-minor-mode
  "Wizard mode."
  :keymap wizard-mode-map
  :group wizard)

;;;###autoload
(define-globalized-minor-mode global-wizard-minor-mode
  apm-minor-mode
  (lambda ()
    (if (not (minibufferp (current-buffer)))
        (wizard-minor-mode t)
      nil))
  :group 'wizard)

(provide 'wizard)
;;; apm.el ends here
