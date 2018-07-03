
(require 'flycheck)
(setq flycheck-check-syntax-automatically '(save new-line))
; As-you-type error highlighting
(add-hook 'after-init-hook #'global-flycheck-mode)


