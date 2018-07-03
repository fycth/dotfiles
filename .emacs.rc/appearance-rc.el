
(load-theme 'whiteboard t)

(recentf-mode 1)

(display-time-mode 1)

(setq default-directory "~/dev")

(setq backup-directory-alist `((".*" . ,"~/.emacs.d/.emacs-saves")))
(setq auto-save-file-name-transforms `((".*" ,"~/.emacs.d/.emacs-saves" t)))
(setq backup-by-copying t)
(setq delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; don't show the startup screen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; display line and column
(setq line-number-mode t)
(setq column-number-mode t)

(set-default 'indicate-empty-lines t)

;; switch off annoing beeps
(setq visible-bell t)

;; gotta see matching parents
(show-paren-mode t)

;; don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; indent with spaces, not with tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

;; work with utf-8
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8 'utf-8-unix)
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))
