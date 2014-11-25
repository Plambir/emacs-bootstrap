(setq-default yas-snippet-dirs nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; from http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name (Nicolas Dudebout)
;; {{{
(setq package-list '(ace-jump-mode
                     2048-game
                     browse-kill-ring
                     js2-mode
                     auto-complete
                     magit
                     ac-math
                     ac-octave
                     ac-js2
                     auctex
                     dropdown-list
                     yasnippet
                     helm
                     popwin
                     lua-mode
                     git-commit-mode
                     glsl-mode
                     undo-tree
                     expand-region
                     cmake-mode
                     haskell-mode
                     htmlize
                     muse
                     fic-mode
                     web-mode
                     php-mode
                     wrap-region
                     auto-highlight-symbol
                     org
                     isearch-symbol-at-point
                     ido-ubiquitous
                     ido-vertical-mode
                     smex
                     window-number
                     fuzzy
                     goto-last-change
                     markdown-mode
                     flycheck))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
;; }}}

(byte-recompile-directory "~/.emacs.d")

;; Off suspend
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-z") nil)

(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/init")))

;; auto puts
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
