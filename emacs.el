(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; from http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name (Nicolas Dudebout)
;; {{{
(setq package-list '(ace-jump-mode
                     browse-kill-ring
                     auto-complete
                     yasnippet
                     lua-mode
                     glsl-mode
                     undo-tree
                     cmake-mode
                     haskell-mode
                     htmlize
                     muse
                     fic-mode
                     web-mode
                     php-mode
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
