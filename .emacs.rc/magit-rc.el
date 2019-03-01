
(require 'flycheck)

(global-set-key (kbd "C-c m") (defhydra hydra-magit (:color blue)
  "
  ^
  ^Magit^             ^Do^
  ^─────^─────────────^──^────────
  _q_ quit            _b_ branch and checkout
  ^^                  _c_ clone
  ^^                  _i_ init
  ^^                  _s_ status
  ^^                  ^^
  "
  ("q" nil)
  ("b" magit-branch-and-checkout)
  ("c" magit-clone)
  ("i" magit-init)
  ("s" magit-status)))
