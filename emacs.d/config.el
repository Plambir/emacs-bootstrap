(load "~/.emacs.d/customize")

(eval-when-compile
    (require 'use-package))

;;; Base
(use-package emacs
  :custom
  (custom-file "~/.emacs.d/customize.el")
  (backup-by-copying t)
  (native-comp-deferred-compilation t)
  (package-native-compile t)
  (backup-directory-alist '((".*" . "~/.emacs.d/backup/")))
  (browse-url-browser-function 'browse-url-chromium)
  (c-basic-offset 2)
  (c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "user")))
  (c-offsets-alist
   '((inline-open . 0)
     (statement-case-open . 0)
     (substatement-open . 0)
     (case-label . +)
     (arglist-close . 0)
     (innamespace . -)))
  (create-lockfiles nil)
  (current-language-environment "UTF-8")
  (default-input-method "russian-computer")
  (delete-old-versions t)
  (delete-selection-mode t)
  (display-battery-mode t)
  (display-line-numbers-widen t)
  (display-time-24hr-format t)
  (display-time-mode t)
  (gdb-many-windows t)
  (echo-keystrokes 0.1)
  (electric-layout-mode nil)
  (electric-pair-mode t)
  (fill-column 80)
  (global-display-line-numbers-mode t)
  (global-subword-mode t)
  (global-hl-line-mode nil)
  (global-linum-mode nil)
  (indent-tabs-mode nil)
  (indicate-empty-lines t)
  (inhibit-startup-screen t)
  (ispell-dictionary "en")
  (kept-new-versions 6)
  (kept-old-versions 2)
  (max-specpdl-size 500000)
  (menu-bar-mode nil)
  (mouse-yank-at-point t)
  (reb-re-syntax 'string)
  (scroll-bar-mode nil)
  (show-paren-mode t)
  (show-paren-style 'expression)
  (show-trailing-whitespace nil)
  (size-indication-mode t)
  (standard-indent 2)
  (tab-width 2)
  (tool-bar-mode nil)
  (typescript-indent-level 2)
  (vc-make-backup-files t)
  (version-control t)
  (visible-bell t)
  (vr/match-separator-use-custom-face t)
  (wgrep-auto-save-buffer t)
  (which-function-mode t)
  (x-stretch-cursor t)
  (column-number-mode t)
  (compilation-scroll-output 'first-error)
  (auto-revert-verbose nil)
  (global-auto-revert-mode t)
  (initial-buffer-choice (lambda ()
                           (switch-to-buffer "*dashboard*")
                           (dashboard-refresh-buffer)))
  (auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))
  (use-dialog-box nil)
  :custom-face
  (default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 98 :width normal))))
  (aw-leading-char-face ((t (:foreground "red" :weight extra-bold :height 2.0))))
  (mode-line-highlight ((t (:underline t))))
  (symbol-overlay-default-face ((t (:background "black")))))

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
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode t))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-footer-icon "")
  (dashboard-footer-messages '())
  (dashboard-startup-banner 3)
  (dashboard-show-shortcuts t)
  (dashboard-items '((recents  . 10)
                     (projects . 10))))

;;;; iflipb
(use-package iflipb
  :ensure t
  :config
  (setq iflipb-wrap-around t)
  :bind
  ([C-tab] . iflipb-next-buffer)
  ([C-iso-lefttab] . iflipb-previous-buffer)
  ([C-S-tab] . iflipb-previous-buffer))

(use-package bindings
  :bind
  ("C-`" . mode-line-other-buffer)
  ("C-§" . mode-line-other-buffer))

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

(use-package multiple-cursors
  :ensure t
  :custom
  (mc/edit-lines-empty-lines 'ignore))

(use-package ace-mc
  :ensure t
  :commands apply-on-rectangle
  :bind
  (("C-; m" . ace-mc-add-multiple-cursors)
   ("C-; M" . ace-mc-add-single-cursor)))

(with-eval-after-load 'rect
  (require 'multiple-cursors)
  (define-key rectangle-mark-mode-map (kbd "C-; C-m") #'my-config--mc/rect-rectangle-to-multiple-cursors))

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

;;;; Completion
(use-package corfu
  :ensure t
  :custom
  (corfu-max-width 150)
  (corfu-auto t)
  (corfu-scroll-margin 5)
  (corfu-popupinfo-delay '(1.5 . 0.5))
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (corfu-popupinfo-mode)
  (global-corfu-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package cape
  :after eglot
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package dabbrev
  :bind (("M-/" . cape-dabbrev))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;;; flycheck
(use-package flycheck
  :ensure t
  :hook ((python-mode . flycheck-mode))
  :custom
  (flycheck-standard-error-navigation nil))

;;;; prog mode
(defun my-config--on-show-trailing-whitespace-prog-mode-hook ()
  "Set show-trailing-whitespace in true"
  (interactive)
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'my-config--on-show-trailing-whitespace-prog-mode-hook)

(use-package fic-mode
  :ensure t
  :custom-face
  (fic-author-face ((((class color))
                     (:underline
                      (:color foreground-color :style line)
                      :foreground "orangered" :background nil))
                    (t
                     (:underline
                      (:color foreground-color :style line)))))
  (fic-face ((((class color))
              (:weight bold :foreground "red" :background nil))
             (t
              (:weight bold))))
  :custom
  (fic-background-color nil)
  (fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE"))
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
  :custom
  (org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
  (org-ellipsis "...")
  (org-log-done t)
  (org-src-fontify-natively t)
  (org-capture-bookmark nil)
  :bind
  (:map org-mode-map
        ("C-c a l" . org-timeline)
        ("C-c a t" . org-show-todo-tree)
        ("C-c a d" . org-check-deadlines)
        ("C-c p" . org-mark-ring-goto)))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq my-org-roam-directory "~/.org-roam")
  (if (not (file-directory-p my-org-roam-directory)) (make-directory my-org-roam-directory))
  :custom
  (org-roam-node-display-template (concat "${title} " (propertize "${tags}" 'face 'org-tag)))
  (org-roam-directory (file-truename my-org-roam-directory))
  (org-roam-completion-everywhere t)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n o" . org-id-get-create)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n T" . org-roam-tag-remove)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(use-package org-roam-ui
  :ensure t)

;;;; Consult
(use-package consult
  :ensure t
  :bind (("C-; C-s" . consult-line)
         ("C-; C-y" . consult-yank-from-kill-ring)
         ("C-; TAB" . consult-imenu)
         ("M-y" . consult-yank-pop)
         ("C-; C-r" . consult-bookmark)
         ("C-x C-b" . consult-buffer)
         ("C-; C-l" . consult-line-thing-at-point)
         ("C-c k" . consult-kmacro)
         ("C-; C-g" . consult-ripgrep))
  :config
  (consult-customize
   consult-buffer consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-.")
  (consult-customize
   consult-xref
   :preview-key '(:debounce 0.4 any))
  (progn
    (defalias 'consult-line-thing-at-point 'consult-line)
    (consult-customize
     consult-line-thing-at-point
     :initial (thing-at-point 'symbol)))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-org-roam
   :ensure t
   :init
   (require 'consult-org-roam)
   (consult-org-roam-mode 1)
   :custom
   (consult-org-roam-grep-func #'consult-ripgrep)
   (consult-org-roam-buffer-narrow-key ?r)
   (consult-org-roam-buffer-after-buffers t)
   :bind (("C-c n e" . consult-org-roam-file-find)
          ("C-c n r" . consult-org-roam-search)
          :map org-mode-map
          ("C-c n b" . consult-org-roam-backlinks)
          ("C-c n B" . consult-org-roam-forward-links)))

(use-package consult-flycheck
  :ensure t
  :after consult
  :bind (("C-; !" . consult-flycheck)))

(defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(use-package embark
  :ensure t
  :after consult
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :custom
  (embark-indicators
          '(embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))
  :config
  (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :config
  (defun marginalia--mode-state (mode) ;;https://github.com/minad/marginalia/issues/155#issuecomment-1478580206
    "Return MODE state string."
    (if (and (boundp mode) (symbol-value mode))
        #(" [On]" 1 5 (face marginalia-on))
      #(" [Off]" 1 6 (face marginalia-off))))
  (defun marginalia--annotate-minor-mode-command (orig cand)
    "Annotate minor-mode command CAND with mode state."
    (concat
     (when-let* ((sym (intern-soft cand))
                 (mode (if (and sym (boundp sym))
                           sym
                         (lookup-minor-mode-from-indicator cand))))
       (marginalia--mode-state mode))
     (funcall orig cand)))
  (advice-add #'marginalia-annotate-command
              :around #'marginalia--annotate-minor-mode-command))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;;; Other
(use-package bookmark
  :custom-face (bookmark-face ((t (:background nil)))))

(use-package wgrep
  :ensure t)

(use-package simple
  :bind (("M-SPC" . cycle-spacing)
         ("C-x K" . kill-this-buffer)))

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
  :custom
  (ibuffer-never-show-predicates '("^\\*[^s]") nil (ibuf-ext))
  :bind (("C-; C-b" . ibuffer)))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

(use-package calc
  :bind (("C-; =" . quick-calc)))

(use-package undo-tree
  :ensure t
  :custom
  (global-undo-tree-mode t)
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo_tree/")))
  (undo-tree-visualizer-diff t)
  :bind (("C-; C-u" . undo-tree-visualize)))

(use-package winner
  :ensure t
  :bind (("C-c /" . winner-undo)
         ("C-c _" . winner-redo))
  :config
  (winner-mode +1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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
      (progn (try-set-font "Liberation Mono")
             (try-set-font "Source Code Pro")
             (if (string= system-type "darwin")
                 (try-set-font "Source Code Variable"))
             (set-face-attribute 'default nil :height 140))))

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;;;; fonts
;;https://www.reddit.com/r/emacs/comments/1xe7vr/check_if_font_is_available_before_setting/
(defun try-set-font (use-font-name)
  (when (member use-font-name (font-family-list))
    (set-face-attribute 'default nil :font use-font-name)))

;;;; omnisharp and C#
(use-package omnisharp ; https://github.com/OmniSharp/omnisharp-roslyn/wiki/Configuration-Options
  :ensure t
  :hook (csharp-mode . omnisharp-mode)
  :config
  (setq omnisharp-imenu-support t))

(use-package csharp-mode
  :ensure t
  :config
  (setq csharp-mode-indent t)
  (setq csharp-tree-sitter-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode)))

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-config--csharp-mode-hook ()
  (define-key csharp-mode-map (kbd "C-c g") 'omnisharp-go-to-definition)
  (define-key csharp-mode-map (kbd "C-c C-g") 'omnisharp-find-usages)
  (define-key csharp-mode-map (kbd "C-c p") 'pop-tag-mark)
  (local-set-key (kbd "C-c r r") 'omnisharp-rename)
  (local-set-key (kbd "C-c r a") 'omnisharp-run-code-action-refactoring)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq truncate-lines t)
  (c-set-style "ellemtel")
  (setq c-syntactic-indentation t)
  (setq tab-width 2)
  (setq default-tab-width 2)
  (setq c-basic-offset 2)
  (setq tab-width 2)
  (electric-pair-local-mode 1)
  (electric-indent-mode 1)
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

;;;; vue
(use-package vue-mode
  :ensure t)

;;;; golang
(defun my-config--go-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-go))
  (flycheck-mode t)
  (company-mode t))

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map ("C-c" . godef-jump))
  :custom
  (gofmt-command "goimports")
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

;;;; C/C++
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

;;;; PHP
(use-package php-mode
  :ensure t)

(defun my-config--php-mode-hook ()
  (setq c-basic-offset 2
        tab-width 2
        indent-tabs-mode nil))

(add-hook 'php-mode-hook 'my-config--php-mode-hook)

;;;; yasnippet
(use-package yasnippet
  :ensure t
  :custom
  (yas-global-mode t)
  (yas-prompt-functions '(yas-ido-prompt yas-no-prompt))
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"
          "~/.emacs.d/local_snippets")))

(use-package yatemplate
  :ensure t
  :config
  (auto-insert-mode t))

;;;; other mods
(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode))
(use-package yaml-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)
(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-draw-borders t)
  (diff-hl-flydiff-delay 2.0)
  (diff-hl-flydiff-mode t)
  (global-diff-hl-mode t))
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
  (when (daemonp)
    (exec-path-from-shell-initialize))
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

;;;; shell
(defun my-config--sh-mode-hook ()
  (setq sh-basic-offset 2
        indent-tabs-mode nil))

(add-hook 'sh-mode-hook 'my-config--sh-mode-hook)

(add-to-list 'auto-mode-alist '("zsh.*" . sh-mode))

;;;; JavaScript
(use-package js
  :custom
  (js-indent-level 2))

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
  :custom-face
  (doom-modeline-bar ((t (:inherit mode-line))))
  (doom-modeline-inactive-bar ((t (:inherit mode-line-inactive))))
  :config
  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 1)
  (if (string= system-type "darwin")
      (progn (set-face-attribute 'mode-line nil :height 140)
             (set-face-attribute 'mode-line-inactive nil :height 140))
    (progn (set-face-attribute 'mode-line nil :height 100)
           (set-face-attribute 'mode-line-inactive nil :height 100)))
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
  ;; Whether display icon for persp name. Nil to display a # sign. It respects `doom-modeline-icon'
  (setq doom-modeline-persp-name-icon nil)
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
  (setq doom-modeline-irc-stylize 'identity)
  (setq doom-modeline-env-load-string "...")
  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil))


;;;; Theme
(use-package spacemacs-theme
  :defer t
  :ensure t
  :init
  (add-hook 'after-make-frame-functions
	      (lambda (&optional frame)
            (when frame
              (select-frame frame))
            (load-theme 'spacemacs-dark t)
            (fix-mac-os)))
  (load-theme 'spacemacs-dark t)
  (fix-mac-os))

;;;; csv-mode
(use-package csv-mode
  :ensure t)

;;;; for mac os x
(fix-mac-os)

;;;; eglot
(use-package eglot
  :ensure t
  :bind
  (("C-c g" . xref-find-definitions)
   ("C-c p" . xref-pop-marker-stack)
   ("C-C C-G" . xref-find-references))
  :hook
  ((c-mode . eglot-ensure)
   (c++-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(c-mode . ("clangd"))
               '(c++-mode . ("clangd"))))

(use-package flycheck-eglot
  :ensure t
  :after eglot
  :config
  (global-flycheck-eglot-mode))

;;;; GDScript
(use-package gdscript-mode
  :ensure t
  :config
  (setq gdscript-gdformat-save-and-format t)
  (setq gdscript-godot-executable "/bin/godot"))

;;;; Arduino
(defun my-config--arduino-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-arduino))
  (flycheck-mode t)
  (company-mode t))

(use-package arduino-mode
  :ensure t
  :config
  (add-hook 'arduino-mode-hook 'my-config--arduino-mode-hook))

;;;; Renpy
(use-package renpy
  :ensure t)

;;;; reverse-im
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

(use-package magit
  :ensure t)

(load "~/.emacs.d/bitgames")

(unwind-protect (load "~/.emacs.d/local") nil)
