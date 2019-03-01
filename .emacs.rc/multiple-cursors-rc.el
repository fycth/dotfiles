;; https://github.com/magnars/multiple-cursors.el

(require 'multiple-cursors)

(global-set-key (kbd "C-c c") (defhydra hydra-multiple-cursors (:color blue)
  "
  ^
  ^Cursors^           ^Do^
  ^─────^─────────────^──^────────
  _q_ quit            _._ Edit lines
  ^^                  _n_ Mark next lik ethis
  ^^                  _p_ Mark previous like this
  ^^                  _a_ Mark all like this
  ^^                  _c_ Clear cursors
  ^^                  ^^
  "
  ("q" nil)
  ("." mc/edit-lines)
  ("n" mc/mark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("a" mc/mark-all-like-this)
  ("c" mc/keyboard-quit)))
