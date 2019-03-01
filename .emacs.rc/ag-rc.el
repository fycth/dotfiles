

(global-set-key (kbd "C-c C-s") (defhydra hydra-ag (:color blue)
  "
  ^
  ^Silver Search^     ^Do^
  ^─────^─────────────^──^────────
  _q_ quit            _s_ Search in specific dir
  ^^                  _f_ Search in files by type
  ^^                  _r_ Search by regexp
  ^^                  _p_ Search in project
  ^^                  _F_ Search in project files by type
  ^^                  _G_ Search in project by regexp
  ^^                  ^^
  "
  ("q" nil)
  ("s" ag)
  ("f" ag-files)
  ("r" ag-regexp)
  ("p" ag-project)
  ("F" ag-project-files)
  ("G" ag-project-regexp)
  ))
