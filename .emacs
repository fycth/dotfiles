
;;One Emacs instance per project, fiplr for going to files. F1 bound to magit-status, F5 bound to a wrapper around compile, and a dedicated window for compile.

(load "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/appearance-rc.el")
(load "~/.emacs.rc/web-mode-rc.el")
(load "~/.emacs.rc/elscreen-rc.el")
(load "~/.emacs.rc/dash-rc.el")
(load "~/.emacs.rc/flycheck-rc.el")
(load "~/.emacs.rc/haskell-mode-rc.el")
(load "~/.emacs.rc/golang-mode-rc.el")
(load "~/.emacs.rc/multiple-cursors-rc.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (intero))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
