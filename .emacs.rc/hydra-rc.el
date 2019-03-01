
(require 'hydra)

;; buffers menu
(global-set-key (kbd "C-c b") (defhydra hydra-buffer (:color blue)
  "
  ^
  ^Buffer^             ^Do^
  ^──────^─────────────^──^──────────
  _q_ quit             _k_ kill
  ^^                   _l_ list
  ^^                   _n_ next
  ^^                   _p_ previous
  ^^                   ^^
  "
  ("q" nil)
  ("k" kill-buffer)
  ("l" ibuffer)
  ("n" next-buffer)
  ("p" previous-buffer)))

