(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(ahs-modes
   '(actionscript-mode apache-mode bat-generic-mode c++-mode c-mode csharp-mode css-mode dos-mode emacs-lisp-mode html-mode ini-generic-mode java-mode javascript-mode js-mode lisp-interaction-mode lua-mode latex-mode makefile-mode makefile-gmake-mode markdown-mode moccur-edit-mode nxml-mode nxhtml-mode outline-mode perl-mode cperl-mode php-mode python-mode rc-generic-mode reg-generic-mode ruby-mode sgml-mode sh-mode squirrel-mode text-mode tcl-mode visual-basic-mode js2-mode))
 '(auto-insert 'other)
 '(auto-insert-query nil)
 '(auto-revert-verbose nil)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))
 '(aw-dispatch-always t)
 '(aw-ignore-current t)
 '(backup-by-copying t)
 '(backup-directory-alist '((".*" . "~/.emacs.d/backup/")))
 '(browse-url-browser-function 'browse-url-chromium)
 '(c-basic-offset 2)
 '(c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "user")))
 '(c-offsets-alist
   '((inline-open . 0)
     (statement-case-open . 0)
     (substatement-open . 0)
     (case-label . +)
     (arglist-close . 0)
     (innamespace . -)))
 '(cc-other-file-alist
   '(("\\.cc\\'"
      (".hh" ".h"))
     ("\\.hh\\'"
      (".cc" ".C"))
     ("\\.c\\'"
      (".h"))
     ("\\.m\\'"
      (".h"))
     ("\\.h\\'"
      (".cpp" ".cc" ".C" ".CC" ".cxx" ".c" ".m"))
     ("\\.C\\'"
      (".H" ".hh" ".h"))
     ("\\.H\\'"
      (".C" ".CC"))
     ("\\.CC\\'"
      (".HH" ".H" ".hh" ".h"))
     ("\\.HH\\'"
      (".CC"))
     ("\\.c\\+\\+\\'"
      (".h++" ".hh" ".h"))
     ("\\.h\\+\\+\\'"
      (".c++"))
     ("\\.cpp\\'"
      (".h" ".hh" ".hpp"))
     ("\\.hpp\\'"
      (".cpp"))
     ("\\.cxx\\'"
      (".hxx" ".hh" ".h"))
     ("\\.hxx\\'"
      (".cxx"))))
 '(column-number-mode t)
 '(company-backends
   '((company-irony-c-headers company-irony)
     company-anaconda company-bbdb company-nxml company-css company-cmake company-capf company-omnisharp
     (company-dabbrev-code company-gtags company-etags company-keywords)
     company-oddmuse company-files company-dabbrev))
 '(company-idle-delay 0.2)
 '(company-tooltip-align-annotations t)
 '(compilation-scroll-output 'first-error)
 '(create-lockfiles nil)
 '(current-language-environment "UTF-8")
 '(custom-file "~/.emacs.d/customize.el")
 '(default-input-method "russian-computer")
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(diff-hl-draw-borders t)
 '(diff-hl-flydiff-delay 2.0)
 '(diff-hl-flydiff-mode t)
 '(display-battery-mode t)
 '(display-line-numbers-widen t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(dumb-jump-max-find-time 5)
 '(echo-keystrokes 0.1)
 '(electric-layout-mode nil)
 '(electric-pair-mode t)
 '(fic-background-color nil)
 '(fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE"))
 '(fill-column 80)
 '(flycheck-checkers
   '(ada-gnat asciidoc cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd elixir emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck haml handlebars haskell-ghc haskell-hlint html-tidy javascript-jshint javascript-eslint javascript-gjslint json-jsonlint less lua make perl perl-perlcritic php php-phpmd php-phpcs puppet-parser puppet-lint python-flake8 python-pylint python-pycompile racket rpm-rpmlint rst rst-sphinx ruby-rubocop ruby-rubylint ruby ruby-jruby rust sass scala scala-scalastyle scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby irony))
 '(flycheck-standard-error-navigation nil)
 '(gdb-many-windows t)
 '(global-apm-minor-mode t)
 '(global-auto-highlight-symbol-mode t)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-diff-hl-mode t)
 '(global-display-line-numbers-mode t)
 '(global-hl-line-mode nil)
 '(global-linum-mode nil)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(gofmt-command "goimports")
 '(helm-M-x-fuzzy-match t)
 '(helm-always-two-windows t)
 '(helm-autoresize-mode t)
 '(helm-boring-buffer-regexp-list
   '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*Compile" "\\`\\*Ibuffer" "\\`\\*helm" "\\`\\*Messages" "\\`\\*Customize"))
 '(helm-boring-file-regexp-list
   '("^\\..*" "\\.hi$" "\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.svn/" "\\.hg/" "\\.git/" "\\.bzr/" "CVS/" "_darcs/" "_MTN/" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$" "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$" "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$" "\\.cs\\.meta$" "\\^.git$"))
 '(helm-buffer-max-length 50)
 '(helm-ff-skip-boring-files t)
 '(helm-follow-mode-persistent t)
 '(helm-full-frame nil)
 '(helm-grep-file-path-style 'relative)
 '(helm-grep-save-buffer-name-no-confirm t)
 '(helm-imenu-fuzzy-match t)
 '(helm-lsp-treemacs-icons nil)
 '(helm-mode t)
 '(helm-mode-fuzzy-match t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-split-window-inside-p t)
 '(ibuffer-never-show-predicates '("^\\*[^s]") nil (ibuf-ext))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en")
 '(js-indent-level 2)
 '(js2-include-jslint-globals nil)
 '(js2-indent-switch-body t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-headerline-breadcrumb-icons-enable nil)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-imenu-enable nil)
 '(lsp-ui-sideline-show-code-actions nil)
 '(max-specpdl-size 500000)
 '(mc/edit-lines-empty-lines 'ignore)
 '(menu-bar-mode nil)
 '(mouse-yank-at-point t)
 '(org-agenda-files "~/.agenda_files")
 '(org-ellipsis "…")
 '(org-log-done t)
 '(org-src-fontify-natively t)
 '(popwin:popup-window-height 18)
 '(popwin:special-display-config
   '(("*Miniedit Help*" :noselect t)
     (help-mode)
     (completion-list-mode :noselect t)
     (compilation-mode :noselect t :dedicated t :stick t)
     (grep-mode :noselect t)
     (occur-mode :noselect t)
     ("*Pp Macroexpand Output*" :noselect t)
     ("*Shell Command Output*")
     ("*vc-diff*")
     ("*vc-change-log*")
     (" *undo-tree*" :width 60 :position right)
     ("^\\*anything.*\\*$" :regexp t)
     ("*slime-apropos*")
     ("*slime-macroexpansion*")
     ("*slime-description*")
     ("*slime-compilation*" :noselect t)
     ("*slime-xref*")
     ("*Flycheck errors*")
     (sldb-mode :stick t)
     (slime-repl-mode)
     (slime-connection-list-mode)
     ("^\\*helm.*\\*$" :regexp t :height 0.3)
     ("*compilation*" :noselect nil :dedicated t :stick t)))
 '(powerline-default-separator nil)
 '(powerline-display-hud t)
 '(powerline-display-mule-info t)
 '(powerline-height nil)
 '(powerline-utf-8-separator-left 32)
 '(powerline-utf-8-separator-right 32)
 '(reb-re-syntax 'string)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(show-trailing-whitespace nil)
 '(size-indication-mode t)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo_tree/")))
 '(undo-tree-visualizer-diff t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(vr/match-separator-use-custom-face t)
 '(wgrep-auto-save-buffer t)
 '(which-function-mode t)
 '(x-stretch-cursor t)
 '(yas-global-mode t)
 '(yas-prompt-functions '(yas-ido-prompt yas-no-prompt)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 98 :width normal))))
 '(aw-leading-char-face ((t (:foreground "red" :weight extra-bold :height 2.0))))
 '(doom-modeline-bar ((t (:inherit mode-line))))
 '(doom-modeline-inactive-bar ((t (:inherit mode-line-inactive))))
 '(helm-selection ((t (:inherit highlight))))
 '(helm-selection-line ((t (:inherit helm-selection))))
 '(mode-line-highlight ((t (:underline t))))
 '(symbol-overlay-default-face ((t (:background "light goldenrod")))))
