(load "~/.emacs.d/customize")

(require 'powerline)
(defun my-powerline-default-theme ()
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size nil 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info nil 'l))
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) face1 'l))
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face2 face1)))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))
(my-powerline-default-theme)

(require 'git-commit)

(move-text-default-bindings)

(require 'hl-line+)
(hl-line-toggle-when-idle 1)
(hl-line-when-idle-interval 3)
(global-hl-line-mode 1)

(require 'company)
(define-key company-mode-map (kbd "M-TAB") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;; popup window manager
(require 'popwin)
(global-set-key (kbd "C-; C-p") popwin:keymap)

;; cycle through amounts of spacing (http://pragmaticemacs.com/emacs/cycle-spacing/)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; http://emacswiki.org/emacs/InteractivelyDoThings#toc23
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(require 'tramp)
(require 'yasnippet)
(require 'popwin)
(popwin-mode t)

(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-;") 'nil)

(require 'swiper)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(require 'multiple-cursors)
(global-set-key (kbd "C-<return>") 'rectangle-mark-mode)
(global-set-key (kbd "C-; C-m") 'mc/rect-rectangle-to-multiple-cursors)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "C-; o t") 'org-todo-list)
(global-set-key (kbd "C-; o a") 'org-agenda-list)

(require 'org)
(define-key org-mode-map (kbd "C-c a l") 'org-timeline)
(define-key org-mode-map (kbd "C-c a t") 'org-show-todo-tree)
(define-key org-mode-map (kbd "C-c a d") 'org-check-deadlines)

(defun mc-my/create-cursor ()
  (interactive)
  (mc/create-fake-cursor-at-point))

(defun mc-my/start-edit ()
  (interactive)
  (if (>= (mc/num-cursors) 1)
      (multiple-cursors-mode 1)
    (multiple-cursors-mode 0)))

(defun mc-my/remove-cursors ()
  (interactive)
  (mc/remove-fake-cursors))

(global-set-key (kbd "C-; m m") 'mc-my/create-cursor)
(global-set-key (kbd "C-; m e") 'mc-my/start-edit)
(global-set-key (kbd "C-; m r") 'mc-my/remove-cursors)

(require 'corral)
(global-set-key (kbd "M-9") 'corral-parentheses-backward)
(global-set-key (kbd "M-0") 'corral-parentheses-forward)
(global-set-key (kbd "M-[") 'corral-brackets-backward)
(global-set-key (kbd "M-]") 'corral-brackets-forward)
(global-set-key (kbd "M-{") 'corral-braces-backward)
(global-set-key (kbd "M-}") 'corral-braces-forward)
(global-set-key (kbd "M-\"") 'corral-double-quotes-backward)

(require 'ace-jump-mode)
(require 'avy)
(setq local-ace-jump-keys '(?a ?s ?d ?f ?j ?k ?l))
(setq ace-jump-word-mode-use-query-char nil)
(setq ace-jump-mode-move-keys local-ace-jump-keys)
(setq aw-keys local-ace-jump-keys)
(define-key global-map (kbd "C-; SPC") 'avy-goto-char-timer)
(define-key global-map (kbd "C-; w") 'avy-goto-word-or-subword-1)
(define-key global-map (kbd "C-; c") 'avy-goto-char)
(define-key global-map (kbd "C-; C-<SPC>") 'avy-goto-line)
(define-key global-map (kbd "C-; C-;") 'ace-window)

(require 'undo-tree)
(define-key global-map (kbd "C-; u") 'undo-tree-visualize)

(require 'visual-regexp)
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-; r") 'vr/query-replace)

(require 'goto-last-change)
(define-key global-map (kbd "C-; C-c") 'goto-last-change)

;; Swap query-replace and query-replace-regexp
(define-key global-map (kbd "M-%") 'query-replace-regexp)
(define-key global-map (kbd "C-M-%") 'query-replace)

(define-key global-map (kbd "C-; C-/") 'company-files)

(define-key global-map (kbd "C-; d s") (lambda () (interactive)(desktop-save "~/")))
(define-key global-map (kbd "C-; d r") 'desktop-read)
(define-key global-map (kbd "C-; d c") 'desktop-clear)

(define-key global-map (kbd "C-x C-b") 'ibuffer)

(define-key global-map (kbd "C-x r I") 'string-insert-rectangle)

(define-key global-map (kbd "C-; C-f") (lambda () (interactive)(ff-find-related-file nil t)))

(define-key global-map (kbd "C-; C-a") 'align-regexp)

(define-key global-map (kbd "C-=") 'er/expand-region)

(define-key global-map (kbd "C-; C-d") 'delete-matching-lines)

(define-key global-map (kbd "C-; C-w") 'whitespace-cleanup)

(require 'helm)
(require 'helm-regexp)
(require 'helm-imenu)

(defun local-helm-imenu-transformer (candidates)
  (cl-loop for (k . v) in candidates
           for types = (or (helm-imenu--get-prop k)
                           (list "Function" k))
           for bufname = (buffer-name (marker-buffer v))
           for disp1 = (mapconcat
                        (lambda (x)
                          (propertize
                           x 'face (cond ((string= x "Class")
                                          'font-lock-keyword-face)
                                         ((string= x "Variables")
                                          'font-lock-variable-name-face)
                                         ((string= x "Function")
                                          'font-lock-function-name-face)
                                         ((string= x "Types")
                                          'font-lock-type-face))))
                        types helm-imenu-delimiter)
           for disp = (propertize disp1 'help-echo bufname)
           collect
           (cons disp (cons k v))))

(defun global-helm-imenu-transformer (candidates)
  (cl-loop for (k . v) in candidates
           for types = (cons (buffer-name (marker-buffer v))
                             (or (helm-imenu--get-prop k)
                                 (list "Function" k)))
           for bufname = (buffer-name (marker-buffer v))
           for disp1 = (mapconcat
                        (lambda (x)
                          (propertize
                           x 'face (cond ((string= x "Class")
                                          'font-lock-keyword-face)
                                         ((string= x "Variables")
                                          'font-lock-variable-name-face)
                                         ((string= x "Function")
                                          'font-lock-function-name-face)
                                         ((string= x "Types")
                                          'font-lock-type-face))))
                        types helm-imenu-delimiter)
           for disp = (propertize disp1 'help-echo bufname)
           collect
           (cons disp (cons k v))))

(setq helm-source-imenu
      (helm-make-source "[:Imenu:]" 'helm-imenu-source
        :fuzzy-match helm-imenu-fuzzy-match
        :candidate-transformer 'local-helm-imenu-transformer))

(setq helm-source-imenu-all
      (helm-make-source "[:Imenu in all buffers:]" 'helm-imenu-source
        :candidates 'helm-imenu-candidates-in-all-buffers
        :fuzzy-match helm-imenu-fuzzy-match
        :candidate-transformer 'global-helm-imenu-transformer))

(defun chose-helm-imenu-or-helm-imenu-in-all-buffers (arg)
  (interactive "P")
  (if arg
      (helm-imenu-in-all-buffers)
    (helm-imenu)))

(define-key global-map (kbd "C-; TAB") 'chose-helm-imenu-or-helm-imenu-in-all-buffers)
(define-key global-map (kbd "C-; C-g") 'helm-find-files)
(define-key global-map (kbd "C-; C-l") 'helm-occur)
(define-key global-map (kbd "C-; C-r") 'helm-bookmarks)
(define-key global-map (kbd "C-; C-y") 'helm-show-kill-ring)

(define-key company-mode-map   (kbd "C-:") 'helm-company)
(define-key company-active-map (kbd "C-:") 'helm-company)

(require 'python)
(define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)

(define-key global-map (kbd "C-; C-e") 'iedit-mode)

;;;; ispell
;; from http://www.emacswiki.org/FlySpell
(setq langs '("en" "ru"))
(setq lang-ring (make-ring (length langs)))
(dolist (elem langs) (ring-insert lang-ring elem))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(define-key global-map (kbd "C-; f d") 'cycle-ispell-languages)
(define-key global-map (kbd "C-; f m") 'flyspell-mode)
(define-key global-map (kbd "C-; f p") 'flyspell-prog-mode)
(define-key global-map (kbd "C-; f w") 'ispell-word)

;;;; '+' and '-' for digital
;; from http://emacsredux.com/blog/2013/07/25/increment-and-decrement-integer-at-point/
(require 'thingatpt)

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
      (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))

(define-key global-map (kbd "C-c +") 'increment-integer-at-point)
(define-key global-map (kbd "C-c -") 'decrement-integer-at-point)

;; change bind for auto-highlight-symbol-mode
(require 'auto-highlight-symbol)
(define-key auto-highlight-symbol-mode-map (kbd "M-p") 'ahs-backward)
(define-key auto-highlight-symbol-mode-map (kbd "M-n") 'ahs-forward)

;;;; fic-mode
(add-hook 'prog-mode-hook 'fic-mode)

;;;; smart-open-line and smart-open-line-above
;; from http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; from http://emacsredux.com/blog/2013/06/15/open-line-above/
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(define-key global-map [(shift return)] 'smart-open-line)
(define-key global-map [(control shift return)] 'smart-open-line-above)

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

;;;; copy line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (point)
                  (line-end-position))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(define-key global-map (kbd "C-; C-k") 'copy-line)

;;;; ido filter function
(defvar-local ido-show-buffer-regexp "\\*\\(scratch\\|info\\|grep\\|compilation\\)\\*")
(defvar-local ido-hide-buffer-regexp "\\(^\\*\\|TAGS$\\)")
(defvar-local ido-hide-dired-buffers t)

(defun ido-custom-filter-function (name)
  (or (and (not (string-match ido-show-buffer-regexp name))
           (string-match ido-hide-buffer-regexp name))
      (and ido-hide-dired-buffers
           (with-current-buffer name
             (equal major-mode 'dired-mode)))))

;;;; load my extension
(load "~/.emacs.d/ext")

(defun fix-mac-os ()
  (if (string= system-type "darwin")   ; Mac OS X
      (progn
        (custom-set-faces
         '(default ((t (:family "Liberation Mono"
                        :foundry "unknown"
                        :slant normal
                        :weight normal
                        :height 130
                        :width normal))))))))

(fix-mac-os)
