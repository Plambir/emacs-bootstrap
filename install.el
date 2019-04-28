(if (version< emacs-version "25.1.1")
    (progn (message-box "Use emacs 25.1.1 or above")
           (kill-emacs -1)))

(add-hook 'emacs-startup-hook #'kill-emacs)
