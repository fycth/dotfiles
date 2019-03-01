
(require 'haskell-mode)
(require 'company-ghc)

(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)

(add-hook 'haskell-mode-hook (lambda ()
                          (company-mode)
                          (set (make-local-variable 'company-backends) '(company-ghc))))

(custom-set-variables
 '(haskell-tags-on-save t))

(global-set-key (kbd "C-c h") (defhydra hydra-haskell-intero (:color blue)
  "
  ^
  ^Haskell Intero^     ^Do^
  ^──────^─────────────^──^──────────
  _q_ quit             _i_ Show information of identifier at point
  ^^                   _t_ Show the type of thing at point, or the selection
  ^^                   _T_ Insert a type signature for the thing at point
  ^^                   _l_ Load this module in the REPL
  ^^                   _e_ Evaluate the selected region in the REPL
  ^^                   _a_ Apply suggestions from GHC
  ^^                   _c_ Clear REPL
  ^^                   _r_ Switch to and from the REPL
  ^^                   _j_ Jump to definition
  ^^                   ^^
  "
  ("q" nil)
  ("i" intero-info)
  ("t" intero-type-at)
  ("T" intero-uses-at)
  ("l" intero-repl-load)
  ("e" intero-repl-eval-region)
  ("a" intero-apply-suggestions)
  ("c" intero-repl-clear-buffer)
  ("r" intero-repl-switch-back)
  ("j" intero-goto-definition)))
