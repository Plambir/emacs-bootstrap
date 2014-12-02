(setq-default yas-snippet-dirs nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; from http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name (Nicolas Dudebout)
;; {{{
(setq package-list '(ace-jump-mode
                     ace-window
                     2048-game
                     js2-mode
                     company
                     company-tern
                     company-anaconda
                     irony
                     company-irony
                     iedit
                     skewer-mode
                     auctex
                     dropdown-list
                     yasnippet
                     helm
                     helm-company
                     helm-pydoc
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
                     ido-ubiquitous
                     ido-vertical-mode
                     smex
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
