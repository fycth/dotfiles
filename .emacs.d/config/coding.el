;; LSP via eglot (built-in)
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'js-ts-mode-hook #'eglot-ensure)

;; Git
(use-package magit
  :bind ("C-x g" . magit-status))
