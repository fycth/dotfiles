(use-package lsp-haskell)
;;(add-hook 'haskell-mode-hook #'lsp)
;;(add-hook 'haskell-literate-mode-hook #'lsp)

;;; configure interactive mode
(setq
    haskell-process-type 'cabal-repl
    haskell-process-log t
    haskell-interactive-popup-errors nil
    haskell-process-auto-import-loaded-modules t
    haskell-process-suggest-remove-import-lines t
    haskell-process-suggest-hoogle-imports t
)

;;: formatting
(setq haskell-stylish-on-save t)

;;; haskell layout offsets
(setq
    haskell-indentation-ifte-offset 2
    haskell-indentation-layout-offset 2
    haskell-indentation-left-offset 2
    haskell-indentation-starter-offset 2
    haskell-indentation-where-post-offset 2
    haskell-indentation-where-pre-offset 2
)

(provide 'layer-haskell)

