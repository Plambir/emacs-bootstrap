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
		((".*\\.sh" . "Shell mode")
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
    ;; tern_project
    (("\\.tern-project$" . "js2-mode")
     nil
"{
  \"libs\": [
    \"browser\"
  ],
  \"loadEagerly\" : [
  ],
  \"plugins\": {
    \"requirejs\": {
      \"baseURL\": \"./\",
      \"paths\": {}
    }
  }
}"
_)
  ))

;;;; python
(add-hook 'python-mode-hook (lambda () (anaconda-mode t)))

;;;; java script
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.tern-project\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda ()
                           (flycheck-mode t)
                           (tern-mode t)))

(add-hook 'js2-mode-hook  'skewer-mode)
(add-hook 'css-mode-hook  'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))

(setq company-tern-property-marker "")
(setq company-tern-meta-as-single-line t)

;;;; php
(add-hook 'php-mode-hook (lambda ()
                           (setq c-basic-offset 2)))

;;;; octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;;; LaTeX
(add-hook 'LaTeX-mode-hook (lambda ()
                             (LaTeX-math-mode)
                             (flyspell-mode)
                             (visual-line-mode)))

;;;; C/C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; http://stackoverflow.com/questions/8549351/c11-mode-or-settings-for-emacs
(add-hook
 'c++-mode-hook
 '(lambda()
    ;; We could place some regexes into `c-mode-common-hook', but note that their evaluation order
    ;; matters.
    (font-lock-add-keywords
     nil '(;;  new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
           ;; c++11 string literals
           ;;       L"wide string"
           ;;       L"wide string with UNICODE codepoint: \u2018"
           ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
           ("\\<\\([LuU8]+\\)\"[[:ascii:][:nonascii:]]*?\"" 1 font-lock-keyword-face)
           ;;       R"(user-defined literal)"
           ;;       R"( a "quot'd" string )"
           ;;       R"delimiter(The String Data" )delimiter"
           ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
           ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\([[:ascii:][:nonascii:]]*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}([[:ascii:][:nonascii:]]*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter
           ;; user-defined types (rather project-specific)
           ;;("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
           ;;("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
    ) t)

;; This hack fixes indentation for C++11's "enum class" in Emacs.
;; http://stackoverflow.com/questions/6497374/emacs-cc-mode-indentation-problem-with-c0x-enum-class/6550361#6550361

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]*"))))

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

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(defun c-irony-on ()
  (if (member major-mode '(c++-mode c-mode objc-mode))
      (progn
        (irony-mode t)
        (irony-cdb-autosetup-compile-options)
        (if (irony-cdb--autodetect-compile-options)
            (flycheck-mode)))))

(add-hook 'c++-mode-hook 'c-irony-on)
(add-hook 'c-mode-hook 'c-irony-on)
(add-hook 'objc-mode-hook 'c-irony-on)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;;; reStructureText
(add-hook 'rst-mode-hook
          (lambda () (set (make-local-variable 'yas-indent-line) 'fixed)
            (auto-fill-mode)
            (flyspell-mode)))

;;;; http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;; http://pragmaticemacs.com/emacs/dont-search-swipe/
;;advise swiper to recenter on exit
(defun bjm-swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter)
  )
(advice-add 'swiper :after #'bjm-swiper-recenter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toggle between most recent buffers                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/SwitchingBuffers#toc5
(defun switch-to-previous-buffer ()
  "Switch to most recent buffer. Repeated calls toggle back and forth between the most recent two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; set key binding
(global-set-key (kbd "C-`") 'switch-to-previous-buffer)

;;;; On show-trailing-whitespace only for programming mode
(defun on-show-trailing-whitespace ()
  "Set show-trailing-whitespace in true"
  (interactive)
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'on-show-trailing-whitespace)

;;;; GLSL
(add-to-list 'auto-mode-alist '("\\.fsh\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vsh\\'" . glsl-mode))

;;;; ZSH
(add-to-list 'auto-mode-alist '("zsh.*" . sh-mode))
