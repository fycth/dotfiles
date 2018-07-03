
;; ---------------------------------------
;; load elscreen
;; ---------------------------------------
;(load "elscreen" "ElScreen" t)
(elscreen-start)

;; F9 creates a new elscreen, shift-F9 kills it
(global-set-key [f9] 'elscreen-create)
(global-set-key [S-f9] 'elscreen-kill)

;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "C-M-_") 'elscreen-previous)
(global-set-key (kbd "C-M-+") 'elscreen-next)
