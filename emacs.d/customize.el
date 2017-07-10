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
 '(auto-revert-verbose nil)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/backup/" t))))
 '(aw-dispatch-always t)
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
    ((company-irony-c-headers company-irony)
     company-anaconda company-bbdb company-nxml company-css company-eclim company-xcode company-cmake company-capf
     (company-dabbrev-code company-gtags company-etags company-keywords)
     company-oddmuse company-files company-dabbrev)))
 '(company-idle-delay 0.2)
 '(compilation-scroll-output (quote first-error))
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
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(dumb-jump-max-find-time 5)
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
 '(global-hl-line-mode nil)
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(helm-always-two-windows t)
 '(helm-autoresize-mode t)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*Compile" "\\`\\*Ibuffer" "\\`\\*helm" "\\`\\*Messages" "\\`\\*Customize")))
 '(helm-boring-file-regexp-list
   (quote
    ("\\.hi$" "\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.svn/" "\\.hg/" "\\.git/" "\\.bzr/" "CVS/" "_darcs/" "_MTN/" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$" "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$" "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$" "\\.cs\\.meta$" "\\.git$")))
 '(helm-buffer-max-length 50)
 '(helm-ff-skip-boring-files t)
 '(helm-follow-mode-persistent t)
 '(helm-full-frame nil)
 '(helm-grep-file-path-style (quote relative))
 '(helm-grep-save-buffer-name-no-confirm t)
 '(helm-imenu-fuzzy-match t)
 '(helm-mode t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-split-window-in-side-p t)
 '(ibuffer-never-show-predicates (quote ("^\\*[^s]")) nil (ibuf-ext))
 '(ido-auto-merge-work-directories-length -1)
 '(ido-everywhere nil)
 '(ido-ignore-buffers (quote ("\\` " ido-custom-filter-function)))
 '(ido-ignore-directories
   (quote
    ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`__pycache__/")))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`__pycache__/")))
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
 '(ido-use-faces nil)
 '(iedit-toggle-key-default (kbd "C-; C-e"))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   ";; Bindings:
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-x z[z] |repeat         |M-TAB    |auto complete     |C-; SPC |avy goto input |
;; |         |               |         |                  |        |chars          |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-; u    |undo tree      |C-; r    |visual regexp     |C-; C-c |goto last      |
;; |         |visualize      |         |replace           |        |change         |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-; C-/  |auto complete  |C-; d s  |save session      |C-; d r |read session   |
;; |         |for file name  |         |                  |        |               |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-; d c  |clear session  |C-x C-b  |ibuffer           |C-; f d |cucle ispell   |
;; |         |               |         |                  |        |languages      |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-; f m  |flyspell mode  |C-; f p  |flyspell prog mode|C-; f w |ispell word    |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-c +/-  |increment /    |S-RET    |spart open line   |C-S-RET |smart open line|
;; |         |decrement      |         |                  |        |above          |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-c C-k  |copy line      |M-S-UP   |move text up      |M-S-DOWN|move text down |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |M-SPC    |smart delete   |[C-u] M-^|join line to      |C-x C-o |delete blank   |
;; |         |space          |         |[down] up         |        |line around    |
;; |         |               |         |                  |        |cursor         |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-x r k  |kill text in   |C-x r M-w|save text from    |C-x r d |delete text    |
;; |         |rectangle      |         |rectangle         |        |from rectangle |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-x r y  |yank last      |C-x r o  |insert blank space|C-x r N |insert line    |
;; |         |killed         |         |to rectangle      |        |numbers in     |
;; |         |rectangle      |         |                  |        |rectangle      |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-x r c  |clear rectangle|C-x r t  |replace rectangle |C-RET   |rectangle mode |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-; C-m  |multiple cursor|C-x (    |start defining    |C-x )   |stop defining  |
;; |         |in each line in|         |macro             |        |macro          |
;; |         |region         |         |                  |        |               |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-x e    |run macro      |C-x C-k ?|help for macro    |M-n     |ahs forward    |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |M-p      |ahs backward   |C-; C-f  |.h <-> .cpp       |C-; C-a |align regexp   |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-=      |expand region  |C-- C-=  |reverse expand    |C-; C-d |delete matching|
;; |         |               |         |region            |        |lines          |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-; C-l  |helm-occur     |C-x TAB  |indent region     |C-; C-i |helm-imenu     |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-; C-g  |helm find file |C-; C-r  |helm bookmarks    |C-; C-y |helm kill ring |
;; |         |[C-u] C-s grep |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-; C-;  |ace window jump|M-r      |reposition point  |C-M-a   |begin of defun |
;; |         |               |         |in window         |        |               |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-M-e    |end of defun   |C-; C-e  |iedit mode        |C-'     |show all iedit |
;; |         |               |         |                  |        |line           |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-x v [  |diff hl prev   |C-x v ]  |diff hl next hunk |C-z ?   |help for popwin|
;; |         |hunk           |         |                  |        |bindings       |
;; +---------+---------------+---------+------------------+--------+---------------+
;; |C-; o t  |org todo list  |C-; o a  |org agenda list   |        |               |
;; |         |               |         |                  |        |               |
;; +---------+---------------+---------+------------------+--------+---------------+
;; Org-Mode: | C-c C-s | scheduled task  | C-c C-d | set deadline   |
;;           | C-c a l | org timeline    | C-c a t | show todo tree |
;;           | C-c a d | check deadlines |
;;----------------------------------------------------------------------------------
;; Multiple Cursor : | C-; m m | create cursor | C-; m r | remove all cursors |
;;                   | C-; m e | start edit    |
;; +-------------------+-----------------------------+---------------------------+
;; |      Company      |           Python            |         Org-mode          |
;; +---+---------------+-------+---------------------+-------+-------------------+
;; |C-:|helm company   |C-c C-d|helm pydoc           |C-c C-o|open external link |
;; +---+---------------+-------+---------------------+-------+-------------------+
;; +-------------------+-------------------------+-------------------------+
;; |     Ido           |          Helm           |          isearch        |
;; +---+---------------+---+---------------------+---+---------------------+
;; |C-d|open dired     |C-j|jump to current line |C-j|avy jump             |
;; +---+---------------+---+---------------------+---+---------------------+
;; |                   |M-e|edit bookmark        |M-c|case [in]sensitive   |
;; +-------------------+---+---------------------+---+---------------------+
;; |                   |C-d|delete bookmark      |   |                     |
;; +-------------------+---+---------------------+---+---------------------+
;; +-------------------------------------------------------------------+
;; |                                M-x                                |
;; +-------------------+-----------------------------------------------+
;; |calculator         | emacs calculator                              |
;; +-------------------+-----------------------------------------------+
;; |run-skewer         | run skewer - plugin for javascrip live coding |
;; +-------------------+-----------------------------------------------+
;; |name-last-kbd-macro| name the last-defined keyboard macro          |
;; +-------------------+-----------------------------------------------+
;; |find-name-dired    | find in directory by filename                 |
;; +-------------------+--+--------------------------------------------+
;; |clone-indirect-buffer | clone buffer                               |
;; +-------------------+--+--------------------------------------------+
;; Ace Window:
;; x - delete window
;; m - swap (move) window
;; c - split window fairly, either vertically or horizontally
;; v - split window vertically
;; b - split window horizontally
;; n - select the previous window
;; i - maximize window (select which window)
;; o - maximize current window

")
 '(ispell-dictionary "en")
 '(js-indent-level 2)
 '(js2-include-jslint-globals nil)
 '(js2-indent-switch-body t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(max-specpdl-size 500000)
 '(mc/edit-lines-empty-lines (quote ignore))
 '(mouse-yank-at-point t)
 '(org-agenda-files "~/.agenda_files")
 '(org-ellipsis "…")
 '(org-log-done t)
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (modern-cpp-font-lock typescript-mode json-mode yasnippet wrap-region visual-regexp-steroids undo-tree smooth-scrolling smex smartrep skewer-mode rainbow-mode powerline popwin php-mode multiple-cursors move-text markdown-mode lua-mode iedit ido-vertical-mode ido-ubiquitous hl-line+ helm-pydoc helm-company haskell-mode groovy-mode graphviz-dot-mode goto-last-change glsl-mode git-commit fuzzy flycheck-irony fic-mode expand-region diff-hl corral company-tern company-irony company-anaconda cmake-mode auto-highlight-symbol auctex apm ace-window ace-jump-mode 2048-game)))
 '(popwin:popup-window-height 18)
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
     ("^\\*helm.*\\*$" :regexp t :height 0.3)
     ("*compilation*" :noselect nil :dedicated t :stick t))))
 '(powerline-default-separator nil)
 '(powerline-display-hud t)
 '(powerline-display-mule-info t)
 '(powerline-height nil)
 '(powerline-utf-8-separator-left 32)
 '(powerline-utf-8-separator-right 32)
 '(reb-re-syntax (quote string))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace nil)
 '(size-indication-mode t)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(undo-tree-auto-save-history nil)
 '(undo-tree-history-directory-alist (quote ((".*" . "~/.emacs.d/undo_tree/"))))
 '(undo-tree-visualizer-diff t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(vr/match-separator-use-custom-face t)
 '(wgrep-auto-save-buffer t)
 '(which-function-mode t)
 '(wrap-region-global-mode t nil (wrap-region))
 '(x-stretch-cursor t)
 '(yas-global-mode t)
 '(yas-prompt-functions (quote (yas-ido-prompt yas-no-prompt)))
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 98 :width normal))))
 '(ahs-definition-face ((t (:background "light goldenrod yellow" :foreground "black" :underline t))))
 '(ahs-face ((t (:background "light goldenrod yellow" :foreground "black"))))
 '(aw-leading-char-face ((t (:foreground "red" :weight extra-bold :height 2.0))))
 '(dropdown-list-face ((t (:inherit default :background "lightgray" :foreground "dimgray"))))
 '(dropdown-list-selection-face ((t (:inherit dropdown-list :foreground "white" :background "steelblue"))))
 '(fringe ((t (:background "gainsboro"))))
 '(helm-bookmark-file ((t (:foreground "DodgerBlue3"))))
 '(helm-bookmark-info ((t (:foreground "dark green"))))
 '(helm-bookmark-w3m ((t (:foreground "orange"))))
 '(helm-buffer-directory ((t (:foreground "blue" :weight bold))))
 '(helm-ff-directory ((t (:foreground "blue" :weight bold))))
 '(helm-ff-dotted-directory ((t (:foreground "black"))))
 '(helm-ff-dotted-symlink-directory ((t (:foreground "DarkOrange"))))
 '(helm-ff-executable ((t (:foreground "dark green" :weight bold))))
 '(helm-grep-finish ((t (:foreground "dark green"))))
 '(helm-locate-finish ((t (:foreground "dark green"))))
 '(helm-match ((t (:inherit isearch))))
 '(helm-moccur-buffer ((t (:foreground "DeepSkyBlue4" :underline t))))
 '(helm-selection ((t (:inherit highlight))))
 '(helm-selection-line ((t (:inherit helm-selection))))
 '(helm-source-header ((t (:background "white smoke" :foreground "black" :weight bold :height 1.3 :family "Sans Serif"))))
 '(highlight ((t (:background "thistle"))))
 '(hl-line ((t (:background "gold"))))
 '(isearch ((t (:background "peach puff"))))
 '(linum ((t (:inherit (shadow default) :background "gainsboro"))))
 '(mode-line ((t (:background "tan1" :foreground "gray10"))))
 '(mode-line-highlight ((t (:underline t))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey40" :weight light))))
 '(powerline-active1 ((t (:inherit mode-line :background "PeachPuff1"))))
 '(powerline-active2 ((t (:inherit mode-line :background "tan3"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "gray88"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "gray"))))
 '(show-paren-match ((t (:background "ivory2"))))
 '(show-paren-mismatch ((t (:background "tomato" :foreground "white"))))
 '(trailing-whitespace ((t (:background "RosyBrown1"))))
 '(vr/group-0 ((t (:background "gold"))))
 '(vr/group-1 ((t (:background "orange"))))
 '(vr/group-2 ((t (:background "dark orange"))))
 '(vr/match-0 ((t (:background "green yellow"))))
 '(vr/match-1 ((t (:background "yellow green"))))
 '(warning ((t (:foreground "dark violet" :weight bold)))))
