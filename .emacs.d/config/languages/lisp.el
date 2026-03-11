;; Autocompletion
(use-package company
  :hook (prog-mode . company-mode))

;; Common Lisp REPL
(use-package slime
  :init
  (setq inferior-lisp-program "sbcl")
  (setq slime-repl-history-file "~/.emacs.d/slime-history.eld")
  :config
  (slime-setup '(slime-fancy slime-asdf)))

(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy))

;; Paredit (structured editing for Lisp)
(use-package paredit
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          slime-repl-mode) . paredit-mode))

;; Colored parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
