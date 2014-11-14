(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(ac-dictionary-directories (quote ("~/.emacs.d/dict/")))
 '(ac-dictionary-files (quote ("~/.emacs.d/dict/dict")))
 '(ac-math-unicode-in-math-p t)
 '(ac-use-fuzzy t)
 '(ac-use-menu-map t)
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
     (substatement-open . 0)
     (innamespace . -))))
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(create-lockfiles nil)
 '(cursor-type (quote bar))
 '(custom-file "~/.emacs.d/customize.el")
 '(delete-old-versions t)
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(electric-layout-mode nil)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(flycheck-standard-error-navigation nil)
 '(gdb-many-windows t)
 '(global-apm-minor-mode t)
 '(global-auto-complete-mode t)
 '(global-auto-highlight-symbol-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
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
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; C-x z[z]          - repeat
;; M-TAB             - auto complete
;; C-; SPC           - ace jump (word)
;; C-; y             - browse kill ring
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
;; C-; RET           - open urxvt
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

;; M-x name-last-kbd-macro - name the last-defined keyboard macro.


")
 '(ispell-dictionary "en")
 '(js2-include-jslint-globals nil)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(mouse-yank-at-point t)
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
 '(window-number-meta-mode t)
 '(window-number-mode t)
 '(x-stretch-cursor t)
 '(yas-global-mode t nil (yasnippet))
 '(yas-prompt-functions (quote (yas-ido-prompt yas-no-prompt)))
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets")) nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "unknown" :slant normal :weight normal :height 83 :width normal))))
 '(highlight ((t (:background "lemon chiffon"))))
 '(isearch ((t (:background "peach puff"))))
 '(linum ((t (:inherit (shadow default) :background "gray91"))))
 '(show-paren-match ((t (:background "khaki"))))
 '(show-paren-mismatch ((t (:background "tomato" :foreground "white"))))
 '(trailing-whitespace ((t (:background "RosyBrown1")))))
