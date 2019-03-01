
(require 'yasnippet)
(yas-global-mode 1)

(global-set-key (kbd "C-c y") (defhydra hydra-yasnippet (:color blue)
  "
  ^
  ^YASnippet^          ^Do^
  ^─────────^──────────^──^────────
  _q_ quit             _i_ insert
  ^^                   _m_ mode
  ^^                   _n_ new
  ^^                   ^^
  "
  ("q" nil)
  ("i" ivy-yasnippet)
  ("m" yas-minor-mode)
  ("n" yas-new-snippet)))
