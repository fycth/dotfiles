;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; utf-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;(global-eldoc-mode 1) ; eglot uses this too

(winner-mode 1) ; C-c <left>, C-c <right> window layout undo/redo

(electric-pair-mode 1) ; auto parens in pairs

(delete-selection-mode) ; allow highlight and backspace over text like a normal editor

;;(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p) ; auto chmod +x scripts

(add-hook 'before-save-hook 'whitespace-cleanup) ; auto strip whitespace

(recentf-mode 1) ; track recently opened files

(savehist-mode 1) ; save minibuffer history

(add-hook 'prog-mode-hook 'goto-address-mode) ; make comment urls clickable

;; better dired
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")))

(setq frame-resize-pixelwise t ; support better certain window managers like ratpoison

      ;; these settings still should be set on a per language basis, this is just a general default
      indent-tabs-mode nil ; spaces > tabs
      tab-width 2 ; tab is 8 spaces
      fill-column 79 ; python friendly

      ;; better security
      gnutls-verify-error t
      gnutls-min-prime-bits 2048

      ;; dont expire a passphrase
      password-cache-expiry nil

      mouse-yank-at-point t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

      ;; the most reliable tramp setup I have found (used at work every day...)
      tramp-default-method "ssh"
      tramp-copy-size-limit nil
      tramp-use-ssh-controlmaster-options nil

      ;; I recommend the following ~/.ssh/config settings be used with the tramp settings in this cfg:
      ;; Host *
      ;; ForwardAgent yes
      ;; AddKeysToAgent yes
      ;; ControlMaster auto
      ;; ControlPath ~/.ssh/master-%r@%h:%p
      ;; ControlPersist yes
      ;; ServerAliveInterval 10
      ;; ServerAliveCountMax 10

      ;; bash, please
      tramp-default-remote-shell "/bin/bash"
      shell-file-name "/bin/bash"
      explicit-shell-file-name "/bin/bash"


      vc-follow-symlinks t ; open symlinks, don't ask confusing questions

      ring-bell-function 'ignore ; be quiet

      browse-url-browser-function 'eww-browse-url ; use a text browser --great for clicking documentation links
      )

;;(defalias 'yes-or-no-p 'y-or-n-p) ; don't make us spell "yes" or "no"

;; set default startup screen
(setq inhibit-startup-screen t)

;; disable toolbars and menus
(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)

;; disable annoying beeping and blinking
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; put all auto-save files in a separate directory
(setq backup-directory-alist '(("." . "~/.emacs_saves")))

;; save desktop session on quit
(desktop-save-mode 1)

;; automatically update buffers when files change
(global-auto-revert-mode t)

;; set $PATH from user shell
(use-package exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-initialize)

(use-package helm)
(helm-mode 1)

;; whitespace mode
(use-package whitespace)
(setq whitespace-line-column 120)  ;; higlight long lines
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(set-face-attribute 'whitespace-space nil :background nil)
(set-face-attribute 'whitespace-indentation nil :background nil)
(set-face-attribute 'whitespace-newline nil
    :background nil
    :foreground "gray30"
)
(setq whitespace-display-mappings
    '(
        (space-mark 32 [32])
        (newline-mark 10 [182 10])
        (tab-mark 9 [10148])
    )
)

(global-linum-mode 1)
(setq linum-format "%4d \u2502 ")

;; higlight line
(use-package hl-line)
(global-hl-line-mode t)
;;(set-face-background 'hl-line "#20385E")

;; git diff highlight
(use-package diff-hl)
(global-diff-hl-mode t)

;; smart-mode-line
(use-package smart-mode-line)
(setq sml/no-confirm-load-theme t)
;;(sml/setup)
(setq
    sml/theme 'light
    sml/shorten-directory t
    sml/shorten-modes t
)

(use-package which-key
:ensure t
:init
(which-key-mode)
)

;; rainbow delimeters
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq your-own-path default-directory)
(if (file-exists-p
     (concat your-own-path ".emacs.desktop"))
    (if (y-or-n-p "Read .emacs.desktop and add hook?")
    (progn
      (desktop-read your-own-path)
      (add-hook 'kill-emacs-hook
            `(lambda ()
               (desktop-save ,your-own-path t))))))

(use-package key-chord)
(key-chord-mode 1)

(provide 'layer-better-defaults)

