(eval-when-compile
    (require 'use-package))

;;;; iedit
(use-package iedit
  :ensure t
  :init
  (setq iedit-toggle-key-default nil)
  :bind (("C-; C-e" . iedit-mode)))

;;;; projectile
(use-package projectile
  :ensure t
  :functions projectile-mode
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-; p") 'projectile-command-map)
  (setq projectile-enable-caching t))

;;;; dashboard
(use-package dashboard
  :ensure t
  :functions dashboard-setup-startup-hook
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 3)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-items '((recents  . 10)
                          (projects . 10))))

;;;; avy
(use-package avy
  :ensure t
  :bind
  (("C-; SPC"     . avy-goto-char-timer)
   ("C-; w"       . avy-goto-word-or-subword-1)
   ("C-; c"       . avy-goto-char)
   ("C-; C-<SPC>" . avy-goto-line)
   :map isearch-mode-map
   ("C-j" . avy-isearch)))

;;;; multiple cursor
;; https://github.com/knu/mc-extras.el/blob/master/mc-rect.el
;; this version ignore empty line and deactivate rectangle mark
(defun my-config--mc/rect-rectangle-to-multiple-cursors (start end)
  "Turn rectangle-mark-mode into multiple-cursors mode, keeping selections."
  (interactive "*r")
  (let* ((current-line (line-beginning-position))
         (reversed (= (current-column)
                      (min
                       (save-excursion
                         (goto-char end)
                         (current-column))
                       (save-excursion
                         (goto-char start)
                         (current-column)))))
         (mark-row `(lambda (startcol endcol)
                      (let ((markcol  ,(if reversed 'endcol 'startcol))
                            (pointcol ,(if reversed 'startcol 'endcol)))
                        (move-to-column markcol)
                        (push-mark (point))
                        (move-to-column pointcol)
                        (setq transient-mark-mode (cons 'only transient-mark-mode))
                        (activate-mark)
                        (setq deactivate-mark nil)))))
    (apply-on-rectangle
     '(lambda (startcol endcol)
        (if (= (point) current-line)
            (funcall mark-row startcol endcol)
          (mc/save-excursion
           (funcall mark-row startcol endcol)
           (if (string-match "[^ ]" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (mc/create-fake-cursor-at-point)))))
     start end))
  (deactivate-mark)
  (mc/maybe-multiple-cursors-mode))

(use-package ace-mc
  :ensure t
  :commands apply-on-rectangle
  :bind
  (("C-; m" . ace-mc-add-multiple-cursors)
   ("C-; M"   . ace-mc-add-single-cursor)
   :map rectangle-mark-mode-map
   ("C-; C-m" . my-config--mc/rect-rectangle-to-multiple-cursors)))

(use-package winum
  :ensure t
  :config
  (winum-mode t)
  (winum-set-keymap-prefix (kbd "C-;")))

;;;; move-text
(use-package move-text
  :ensure t
  :functions move-text-default-bindings
  :config
  (move-text-default-bindings))

(use-package anaconda-mode
  :ensure t
  :hook python-mode)

;;;; company
(use-package company
  :ensure t
  :bind
  (("C-; C-/" . company-files)
   :map company-mode-map
   ("M-TAB" . company-complete)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)))

(use-package company-anaconda
  :ensure t)

(use-package company-go
  :ensure t)

(use-package company-tern
  :ensure t)

;;;; flycheck
(use-package flycheck
  :hook ((python-mode . flycheck-mode))
  :ensure t)

;;;; popwin
(use-package popwin
  :ensure t
  :commands popwin-mode
  :config
  (global-set-key (kbd "C-; W") popwin:keymap)
  (popwin-mode t))

;;;; prog mode
(defun my-config--on-show-trailing-whitespace-prog-mode-hook ()
  "Set show-trailing-whitespace in true"
  (interactive)
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'my-config--on-show-trailing-whitespace-prog-mode-hook)

(use-package fic-mode
  :ensure t
  :hook prog-mode)

;;;; dired
;; from https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
(defun my-config--dired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (my-config--dired-sort))

;;;; org
(use-package org
  :ensure t
  :bind (()
         :map org-mode-map
         ("C-c a l" . org-timeline)
         ("C-c a t" . org-show-todo-tree)
         ("C-c a d" . org-check-deadlines)))

;;;; dump-jump
(use-package dumb-jump
  :ensure t
  :bind (("C-c g" . dumb-jump-go)
         ("C-c p" . dumb-jump-back)))

;;;; helm
(defun my-config--helm-toggle-show-hide-files ()
  (interactive)
  (setq helm-ff-skip-boring-files (not helm-ff-skip-boring-files))
  (helm-refresh))

(defun my-config--helm-chose-imenu-or-imenu-in-all-buffers (arg)
  (interactive "P")
  (if arg
      (helm-imenu-in-all-buffers)
    (helm-imenu)))

(use-package helm
  :ensure t
  :bind (("C-x C-b" . helm-mini)
         ("C-x b"   . helm-mini)
         ("C-; TAB" . my-config--helm-chose-imenu-or-imenu-in-all-buffers)
         ("C-; C-g" . helm-find-files)
         ("C-; C-l" . helm-occur)
         ("C-; C-r" . helm-bookmarks)
         ("C-; C-y" . helm-show-kill-ring))
  :config
  (require 'helm-config)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (with-eval-after-load 'helm-mode
    (define-key helm-find-files-map (kbd "C-c a") #'my-config--helm-toggle-show-hide-files)))

(use-package wgrep-helm
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;;;; global keybinds
(use-package bindings
  :bind (("C-`" . mode-line-other-buffer)
         ("C-§" . mode-line-other-buffer)))

(use-package simple
  :bind (("M-SPC" . cycle-spacing)))

(use-package flyspell
  :bind (:map flyspell-mode-map ("C-;" . nil)))

(use-package rect
  :bind (("C-<return>" . rectangle-mark-mode)))

(use-package visual-regexp-steroids
  :ensure t
  :bind (("C-; r" . vr/query-replace)))

(use-package goto-last-change
  :ensure t
  :bind (("C-; C-c" . goto-last-change)))

(use-package ibuffer
  :bind (("C-; C-b" . ibuffer)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package calc
  :bind (("C-; =" . quick-calc)))

(use-package undo-tree
  :ensure t
  :bind (("C-; C-u" . undo-tree-visualize)))

;;;; smarter move
;; from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;;;; mac os x
(defun fix-mac-os ()
  (if (string= system-type "darwin")   ; Mac OS X
      (set-face-attribute 'default nil :height 140)))

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;;;; fonts
;;https://www.reddit.com/r/emacs/comments/1xe7vr/check_if_font_is_available_before_setting/
(defun try-set-font (use-font-name)
  (when (member use-font-name (font-family-list))
    (set-face-attribute 'default nil :font use-font-name)))

;;;; omnisharp and C#
(use-package omnisharp
  :ensure t
  :hook (csharp-mode . omnisharp-mode)
  :config
  (setq omnisharp-imenu-support t))

(eval-after-load "omnisharp"
  '(defun omnisharp--project-root () ()))

(defun my-config--csharp-mode-hook ()
  (define-key csharp-mode-map (kbd "C-c g") 'omnisharp-go-to-definition)
  (define-key csharp-mode-map (kbd "C-c C-g") 'omnisharp-find-usages-with-ido)
  (define-key csharp-mode-map (kbd "C-c p") 'pop-tag-mark)
  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq truncate-lines t)
  (setq tab-width 2)
  (setq default-tab-width 2)
  (setq c-basic-offset 2)
  (electric-pair-mode 1))

(add-hook 'csharp-mode-hook 'my-config--csharp-mode-hook)

;;;; json
(defun my-config--json-mode-hook ()
  (flycheck-mode t))

(use-package json-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  (add-hook 'json-mode-hook 'my-config--json-mode-hook))

;;;; golang
(defun my-config--go-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-go))
  (flycheck-mode t)
  (company-mode t))

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map ("C-c" . godef-jump))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'my-config--go-mode-hook))

;;;; LaTeX
(defun my-config--LaTeX-mode-hook ()
  (LaTeX-math-mode)
  (flyspell-mode)
  (visual-line-mode))

(use-package auctex
  :defer t
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'my-config--LaTeX-mode-hook))

;;;; C/C++ and irony
(use-package irony
  :ensure t
  :hook (irony-mode . irony-cdb-autosetup-compile-options))

(use-package flycheck-irony
  :ensure t)

(use-package company-irony
  :ensure t
  :hook (irony-mode . company-irony-setup-begin-commands))

(use-package company-irony-c-headers
  :ensure t)

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

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
  (add-to-list 'c-offsets-alist '(statement-cont . align-enum-class-closing-brace)))

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

;;;; other mods
(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode))
(use-package yasnippet
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)
(use-package diff-hl
  :ensure t)
(use-package lua-mode
  :ensure t)
(use-package git-commit
  :ensure t)
(use-package glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.fsh\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vsh\\'" . glsl-mode)))
(use-package cmake-mode
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(add-to-list 'auto-mode-alist '("zsh.*" . sh-mode))

;;;; java
(defun my-config--java-mode-hook ()
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode t))

(add-hook 'java-mode-hook 'my-config--java-mode-hook)

;;;; compilation buffer
;; http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(use-package ansi-color
  :ensure t
  :config
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

;; https://emacswiki.org/emacs/ModeCompile
;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))

(setq compilation-exit-message-function 'compilation-exit-autoclose)

;;;; symbol-overlay
(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-i" . symbol-overlay-put)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-n" . symbol-overlay-jump-next)))

;;;; doom-modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode t)
  :config
  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 20)
  ;; How wide the mode-line bar should be (only respected in GUI Emacs).
  (setq doom-modeline-bar-width 12)
  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are expereicing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  ;; Whether display icons in mode-line or not.
  (setq doom-modeline-icon nil)
  ;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)
  ;; Whether display color icons for `major-mode'. It respects
  ;; `doom-modeline-icon' and `all-the-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)
  ;; Whether display icons for buffer states. It respects `doom-modeline-icon'.
  (setq doom-modeline-buffer-state-icon t)
  ;; Whether display buffer modification icon. It respects `doom-modeline-icon'
  ;; and `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-buffer-modification-icon t)
  ;; Whether display minor modes in mode-line or not.
  (setq doom-modeline-minor-modes nil)
  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)
  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)
  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 12)
  ;; Whether display perspective name or not. Non-nil to display in mode-line.
  (setq doom-modeline-persp-name t)
  ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
  (setq doom-modeline-lsp t)
  ;; Whether display github notifications or not. Requires `ghub` package.
  (setq doom-modeline-github nil)
  ;; The interval of checking github.
  (setq doom-modeline-github-interval (* 30 60))
  ;; Whether display environment version or not
  (setq doom-modeline-env-version t)
  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)
  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python")
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")
  ;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
  (setq doom-modeline-mu4e nil)
  ;; Whether display irc notifications or not. Requires `circe' package.
  (setq doom-modeline-irc nil)
  ;; Function to stylize the irc buffer names.
  (setq doom-modeline-irc-stylize 'identity))

(load "~/.emacs.d/bitgames")
(load "~/.emacs.d/customize")

(unwind-protect (load "~/.emacs.d/local") nil)

;;;; for mac os x
(fix-mac-os)
(try-set-font "Liberation Mono")
(try-set-font "Source Code Pro")
(if (string= system-type "darwin")
    (try-set-font "Source Code Variable"))
