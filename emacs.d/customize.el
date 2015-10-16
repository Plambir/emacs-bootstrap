(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(ahs-modes
   (quote
    (actionscript-mode apache-mode bat-generic-mode c++-mode c-mode csharp-mode css-mode dos-mode emacs-lisp-mode html-mode ini-generic-mode java-mode javascript-mode js-mode lisp-interaction-mode lua-mode latex-mode makefile-mode makefile-gmake-mode markdown-mode moccur-edit-mode nxml-mode nxhtml-mode outline-mode perl-mode cperl-mode php-mode python-mode rc-generic-mode reg-generic-mode ruby-mode sgml-mode sh-mode squirrel-mode text-mode tcl-mode visual-basic-mode js2-mode)))
 '(auto-insert (quote other))
 '(auto-insert-query nil)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/backup/" t))))
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup/"))))
 '(browse-url-browser-function (quote browse-url-chromium))
 '(c-basic-offset 2)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "user"))))
 '(c-offsets-alist
   (quote
    ((inline-open . 0)
     (statement-case-open . 0)
     (substatement-open . 0)
     (case-label . +)
     (arglist-close . 0)
     (innamespace . -))))
 '(cc-other-file-alist
   (quote
    (("\\.cc\\'"
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
      (".cxx")))))
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-irony company-anaconda company-tern company-bbdb company-nxml company-css company-eclim company-xcode company-ropemacs company-cmake company-capf
                   (company-dabbrev-code company-gtags company-etags company-keywords)
                   company-oddmuse company-files company-dabbrev)))
 '(company-idle-delay 0.2)
 '(compilation-scroll-output (quote first-error))
 '(create-lockfiles nil)
 '(current-language-environment "UTF-8")
 '(cursor-type (quote bar))
 '(custom-file "~/.emacs.d/customize.el")
 '(default-input-method "russian-computer")
 '(delete-old-versions t)
 '(diff-hl-draw-borders t)
 '(diff-hl-flydiff-delay 0.3)
 '(diff-hl-flydiff-mode t)
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(echo-keystrokes 0.1)
 '(electric-layout-mode nil)
 '(electric-pair-mode t)
 '(fic-background-color nil)
 '(fic-highlighted-words (quote ("FIXME" "TODO" "BUG" "NOTE")))
 '(fill-column 80)
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoc cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd elixir emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck haml handlebars haskell-ghc haskell-hlint html-tidy javascript-jshint javascript-eslint javascript-gjslint json-jsonlint less lua make perl perl-perlcritic php php-phpmd php-phpcs puppet-parser puppet-lint python-flake8 python-pylint python-pycompile racket rpm-rpmlint rst rst-sphinx ruby-rubocop ruby-rubylint ruby ruby-jruby rust sass scala scala-scalastyle scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby irony)))
 '(flycheck-standard-error-navigation nil)
 '(gdb-many-windows t)
 '(global-apm-minor-mode t)
 '(global-auto-complete-mode t)
 '(global-auto-highlight-symbol-mode t)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-diff-hl-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(global-wizard-minor-mode t)
 '(helm-follow-mode-persistent t)
 '(ibuffer-never-show-predicates (quote ("^\\*[^s]")) nil (ibuf-ext))
 '(ido-everywhere t)
 '(ido-ignore-buffers (quote ("\\` " ido-custom-filter-function)))
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-command-overrides
   (quote
    ((enable exact "execute-extended-command")
     (enable prefix "wl-")
     (enable-old prefix "Info-")
     (enable exact "webjump")
     (enable regexp "\\`\\(find\\|load\\|locate\\)-library\\'")
     (disable prefix "org-")
     (disable prefix "magit-")
     (disable prefix "tmm-")
     (enable regexp "\\`\\(load\\|enable\\|disable\\|describe\\|custom-theme-visit\\)-theme\\'"))))
 '(ido-ubiquitous-mode t)
 '(ido-use-faces nil)
 '(ido-vertical-mode t)
 '(iedit-toggle-key-default (kbd "C-; C-e"))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   ";; Bindings:
;; C-x z[z]          - repeat
;; M-TAB             - auto complete
;; C-; SPC           - ace jump (word)
;; C-; u             - undo tree visualize
;; C-; C-s           - isearch-symbol-at-point
;; C-; r             - regexp builder
;; C-; C-c           - goto last change
;; C-; C-/           - auto complete for filename
;; C-; d s           - save session
;; C-; d r           - read session
;; C-; d c           - clear session
;; C-x C-b           - ibuffer
;; C-; f d           - cycle ispell languages
;; C-; f m           - flyspell mode
;; C-; f p           - flyspell prog mode
;; C-; f w           - ispell word
;; C-c +/-           - increment/decrement
;; SHIFT-RET         - smart open line
;; CONTROL-SHIFT-RET - smart open line above
;; C-c C-k           - copy line
;; M-SHIFT-UP        - move line up
;; M-SHIFT-DOWN      - move line down
;; M-SPACE           - delete spaces, save only one
;; M-^               - delete identation
;; C-x r k           - kill the text of the region-rectangle
;; C-x r M-w         - save the text of the region-rectangle
;; C-x r d           - delete the text of the region-rectangle
;; C-x r y           - yank the last killed rectangle with its upper left corner at point
;; C-x r o           - insert blank space to fill the space of the region-rectangle
;; C-x r N           - insert line numbers along the left edge of the region-rectangle
;; C-x r c           - clear the region-rectangle by replacing all of its contents with spaces
;; C-x <SPC>         - toggle Rectangle Mark mode
;; C-x r t           - replace rectangle contents with string on each line
;; C-x r I           - insert string on each line of the rectengle
;; C-x (             - start defining the keyboard macro
;; C-x )             - stop defining the keyboard macro
;; C-x e             - execute the keyboard macro
;; C-x C-k ?         - show help for keyboard macro
;; C-x C-a           - edit auto highlight symbols
;; C-; C-f           - find related file (.h <-> .cpp)
;; C-; C-a           - align regexp
;; C-=               - expand region
;; C-- C-=           - reverse expand region
;; C-; C-d           - delete matching lines
;; C-; C-l           - helm-occur
;; C-x TAB           - indent region
;; C-; TAB           - imenu
;; C-; C-g           - helm do grep
;; C-; C-r           - helm register
;; C-; b             - helm mini
;; C-; C-y           - helm show kill ring
;; C-; C-;           - ace window jump
;; C-x r <SPC>       - save position
;; C-x r j           - jump to position
;; C-x r m           - save bookmark
;; C-x r b           - jump to bookmark
;; C-x r l           - list of bookmarks
;; M-r               - reposition point in window (no scroll)
;; C-M-a             - beginning of defun
;; C-M-e             - end of defun
;; C-:               - helm company
;; C-c C-d           - helm pydoc
;; M-\\               - remove all spaces
;; C-; C-e           - iedit mode
;; C-'               - in iedit mode is show all edit lines
;; C-c C-o           - open external link in org-mode
;; M-^               - join line
;; C-x C-o           - delete black lines
;; C-; C-m           - multiple cursor in each line in the region
;; C-<return>        - rectangle-mark-mode

;; In swiper:
;;   C-'             - avy jump
;;   C-7             - multiple-cursors

;; M-x name-last-kbd-macro - name the last-defined keyboard macro.
;; M-x calculator
;; M-x run-skewer


")
 '(ispell-dictionary "en")
 '(js2-include-jslint-globals nil)
 '(js2-indent-switch-body t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(mc/edit-lines-empty-lines (quote ignore))
 '(mode-line-format
   (quote
    ("%n%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(mouse-yank-at-point t)
 '(org-src-fontify-natively t)
 '(popwin:popup-window-height 25)
 '(popwin:special-display-config
   (quote
    (("*Miniedit Help*" :noselect t)
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
     (sldb-mode :stick t)
     (slime-repl-mode)
     (slime-connection-list-mode)
     ("^\\*helm.*\\*$" :regexp t :height 0.3))))
 '(reb-re-syntax (quote string))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote ((".*" . "~/.emacs.d/undo_tree/"))))
 '(undo-tree-visualizer-diff t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(wrap-region-global-mode t nil (wrap-region))
 '(x-stretch-cursor t)
 '(yas-global-mode t nil (yasnippet))
 '(yas-prompt-functions (quote (yas-dropdown-prompt yas-ido-prompt yas-no-prompt)))
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets")) nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "unknown" :slant normal :weight normal :height 83 :width normal))))
 '(ahs-definition-face ((t (:background "light goldenrod yellow" :foreground "black" :underline t))))
 '(ahs-face ((t (:background "light goldenrod yellow" :foreground "black"))))
 '(dropdown-list-face ((t (:inherit default :background "lightgray" :foreground "dimgray"))))
 '(dropdown-list-selection-face ((t (:inherit dropdown-list :foreground "white" :background "steelblue"))))
 '(helm-bookmark-file ((t (:foreground "DodgerBlue3"))))
 '(helm-bookmark-info ((t (:foreground "dark green"))))
 '(helm-bookmark-w3m ((t (:foreground "orange"))))
 '(helm-buffer-directory ((t (:foreground "blue" :weight bold))))
 '(helm-ff-directory ((t (:foreground "blue" :weight bold))))
 '(helm-ff-executable ((t (:foreground "dark green"))))
 '(helm-grep-finish ((t (:foreground "dark green"))))
 '(helm-locate-finish ((t (:foreground "dark green"))))
 '(helm-match ((t (:inherit isearch))))
 '(helm-moccur-buffer ((t (:foreground "DeepSkyBlue4" :underline t))))
 '(helm-selection ((t (:inherit highlight))))
 '(helm-selection-line ((t (:inherit helm-selection))))
 '(helm-source-header ((t (:background "white smoke" :foreground "black" :weight bold :height 1.3 :family "Sans Serif"))))
 '(highlight ((t (:background "lemon chiffon"))))
 '(isearch ((t (:background "peach puff"))))
 '(linum ((t (:inherit (shadow default) :background "gray91"))))
 '(mode-line ((t (:background "grey90" :foreground "grey20"))))
 '(mode-line-highlight ((t (:background "gray75"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey40" :weight light))))
 '(show-paren-match ((t (:background "khaki"))))
 '(show-paren-mismatch ((t (:background "tomato" :foreground "white"))))
 '(trailing-whitespace ((t (:background "RosyBrown1")))))
