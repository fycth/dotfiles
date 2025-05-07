;; Load custom variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Load config
(load "~/.emacs.d/config/core.el")
(load "~/.emacs.d/config/coding.el")

;; Language support
(load "~/.emacs.d/config/languages/lisp.el")
(load "~/.emacs.d/config/languages/ocaml.el")
(load "~/.emacs.d/config/languages/web.el")

(fido-mode t)
