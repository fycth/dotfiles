
(require 'cl)

;; declare package sources
(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize) ;; You might already have this line

;; define a list of packages to install via package manager
(defvar my-packages
    '(projectile
      helm
      helm-projectile
      recentf-ext
      hl-line+
      erlang
      magit
      fic-mode
      yasnippet
      web-mode
      haskell-mode
      wakatime-mode
      dash-at-point
      )
      "A list of packages to ensure are installed at launch.")

;; is a package installed
(defun my-packages-installed-p ()
    (loop for p in my-packages
	          when (not (package-installed-p p)) do (return nil)
		          finally (return t)))

;; go through the packages list and install all necessary
(unless (my-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p my-packages)
         (when (not (package-installed-p p))
           (message "installing package %s\n" p)
           (package-install p))))

(provide 'my-packages)

(setenv "ESHELL" (expand-file-name "~/dotfiles/eshell"))

(setq backup-directory-alist `((".*" . ,"~/.emacs.d/.emacs-saves")))
(setq auto-save-file-name-transforms `((".*" ,"~/.emacs.d/.emacs-saves" t)))
(setq backup-by-copying t)
(setq delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;;
;;(add-to-list 'load-path "~/.emacs.d/mytracker")
;;(load "mytracker.el")

;AutoInsert
(setq auto-insert-directory "~/dotfiles/auto-inserts")
(auto-insert-mode 't)
(setq auto-insert-alist '((erlang-mode . "header.erl")))

(setq user-full-name "Andrii Sergiienko"
      user-mail-address "andrey.sergienko@gmail.com")

;; turn ON web-mode for HTML and JS files
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))

;; turn on highlighting TODO, FIXME, BUG
(add-hook 'prog-mode-hook 'turn-on-fic-mode)

;; turn on yasnippet globally
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

(setq mac-option-modifier 'alt)

(global-hl-line-mode nil)
(toggle-hl-line-when-idle)

(projectile-global-mode)

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
    (require 'erlang-start))

(add-to-list 'auto-mode-alist '("[.]dtl" . html-mode))

;; recent opened files
(recentf-mode 1)

(display-time-mode 1)
;;(display-battery-mode 1)

;;small fringes
(set-fringe-mode '(1 . 1))

;; use command as the meta key
(setq ns-command-modifier (quote meta))

;; don't show the startup screen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

(setq transient-mark-mode t)

;; display line and column
(setq line-number-mode t)
(setq column-number-mode t)

(set-default 'indicate-empty-lines t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; switch off annoing beeps
(setq visible-bell t)

;; gotta see matching parents
(show-paren-mode t)

;; don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-o") 'other-window)

(global-unset-key (kbd "C-x 3")) ; was split-window-horiz
(global-unset-key (kbd "C-x 2")) ; was split-window-vert
(global-unset-key (kbd "C-x 1")) ; was delete other windows
(global-unset-key (kbd "C-x 0")) ; was delete-window
(global-unset-key (kbd "C-x o")) ; was other-window

;; mac os conventions
(global-set-key (kbd "M-a") 'mark-whole-buffer)

;; ibuffer > list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; magit main interface
(global-set-key (kbd "C-x g") 'magit-status)

;; easier buffer killing
(global-set-key (kbd "M-k") 'kill-this-buffer)

;; refresh like
(global-set-key [(f5)] 'revert-buffer)
(global-set-key [(control f5)] 'toggle-read-only)
(global-set-key [(shift f5)] 'ecb-redraw-layout)

;; indent with spaces, not with tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; set 'solarized dark' theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
;;(load-theme 'solarized-dark t)

;;
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; Erlang stuff
(add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-2.7/emacs")
(require 'erlang-start)
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))
(setq erlang-root-dir "/usr/local/lib/erlang")
(add-to-list 'exec-path "/usr/local/lib/erlang/bin")
(setq erlang-man-root-dir "/usr/local/lib/erlang/man")
(add-hook 'erlang-mode-hook
             (lambda ()
            ;; when starting an Erlang shell in Emacs, the node name
            ;; by default should be "emacs"
            (setq inferior-erlang-machine-options '("-sname" "emacsnode"))))

;; auto indent when paste
(dolist (command '(yank yank-pop))
       (eval `(defadvice ,command (after indent-region activate)
                (and (not current-prefix-arg)
                     (member major-mode '(emacs-lisp-mode lisp-mode erlang-mode
                                                          clojure-mode    scheme-mode
                                                          haskell-mode    ruby-mode
                                                          rspec-mode      python-mode
                                                          c-mode          c++-mode
                                                          objc-mode       latex-mode
                                                          plain-tex-mode))
                     (let ((mark-even-if-inactive transient-mark-mode))
                       (indent-region (region-beginning) (region-end) nil))))))

;; if we're inside of a projectile project, open file in projectile mode
;; otherwise, open file in helm mode
(defun sr-open-file()
  "Open file using projectile+helm or helm-for-files"
  (interactive)
  (if (projectile-project-p)
    (helm-projectile)
    (helm-for-files)))

(global-set-key (kbd "M-o") 'sr-open-file)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Open Dash docs at the cursor point
(global-set-key (kbd "C-x d") 'dash-at-point)

;; Wakatime mode
(global-set-key (kbd "C-x w") 'global-wakatime-mode)

;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key (kbd "M-<down>")
		(lambda () (interactive)
		  (condition-case nil (scroll-up)
				  (end-of-buffer (goto-char (point-max))))))

(global-set-key (kbd "M-<up>")
		(lambda () (interactive)
		  (condition-case nil (scroll-down)
				  (beginning-of-buffer (goto-char (point-min))))))

(global-set-key (kbd "M-<left>") 'beginning-of-line)                                 
(global-set-key (kbd "M-<right>") 'end-of-line)

(global-set-key (kbd "A-<down>") 'windmove-down)
(global-set-key (kbd "A-<up>") 'windmove-up)
(global-set-key (kbd "A-<left>") 'windmove-left)
(global-set-key (kbd "A-<right>") 'windmove-right)

(require 'un-define "un-define" t)

;; work with utf-8
(set-buffer-file-coding-system 'utf-8 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; for Mac
;; Hide the tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
;; Slow down the mouse wheel acceleration
(when (boundp 'mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(0.01)))

;; disable auto-save feature
(setq auto-save-default nil)

(defun sync-remote()
  "Does rsync over SSH with remote server"
  (interactive)
  (projectile-run-shell-command-in-root "sh sync.sh"))
(global-set-key [(f2)] 'sync-remote)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(wakatime-api-key "60f0cff9-9e10-4dd5-9f2f-5ee6ec4c9276")
 '(wakatime-cli-path "/usr/local/bin/wakatime"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; start emacs on a remote machine: emacsclient -c -a ""
;; close such emacs process: C-x c
;; Kill-server
(defun my-kill-emacs ()
(interactive)
(save-some-buffers)
(desktop-save-in-desktop-dir)
(kill-emacs))
(global-set-key (kbd "C-x c") 'my-kill-emacs)
