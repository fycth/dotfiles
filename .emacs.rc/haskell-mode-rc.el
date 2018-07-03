
(require 'haskell-mode)
(require 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))
(intero-global-mode 1)
