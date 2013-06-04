
(load "~/.emacs.d/tabs.el")
(load "~/.emacs.d/flymake.el")
(load "~/.emacs.d/utf-8.el")
(load "~/.emacs.d/recentf.el")
(load "~/.emacs.d/markdown-mode.el")
(load "~/.emacs.d/mac.el")
(load "~/.emacs.d/iswitchb.el")
(load "~/.emacs.d/ido.el")
(load "~/.emacs.d/hl-line.el")
(load "~/.emacs.d/fonts.el")
(load "~/.emacs.d/defuns.el")

;; for ocaml
(add-to-list 'load-path "~/.emacs.d/deps/tuareg-2.0.6")
(load "~/.emacs.d/deps/tuareg-2.0.6/tuareg.el")
(add-to-list 'auto-mode-alist '("[.]ml" . tuareg-mode))

(add-to-list 'load-path "~/.emacs.d/deps/tramp")
(require 'tramp)
(setq tramp-default-method "scp")

(add-to-list 'auto-mode-alist '("[.]dtl" . html-mode))

;; recent opened files
(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-xf" 'recentf-open-files)

;; Goto-line short-cut key
(global-set-key "\C-l" 'goto-line)

(display-time-mode 1)
;;(display-battery-mode 1)

;;small fringes
(set-fringe-mode '(1 . 1))

;;(when (fboundp 'toggle-scroll-bar)
;; (toggle-scroll-bar -1))

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

;;for emacs client
;;(server-start)

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

;; window navigation
(windmove-default-keybindings 'meta)

;; mac os conventions
(global-set-key (kbd "M-a") 'mark-whole-buffer)

;; find matched parens
(global-set-key (kbd "C-'") 'match-paren)

;; easy inserts
(global-set-key (kbd "C-.") 'insert-arrow)

;; ibuffer > list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; easier buffer killing
(global-set-key (kbd "M-k") 'kill-this-buffer)

;; indential and alignment
(global-set-key [(f8)] 'indent-region)
(global-set-key [(control f8)] 'align)
(global-set-key [(shift f8)] 'align-current)
(global-set-key [(meta f8)] 'align-regexp)

;; find stuff
(global-set-key [(f2)] 'ack)
(global-set-key [(control f2)] 'ack-same)
(global-set-key [(control meta f2)] 'ack-default-directory)
(global-set-key [(meta f2)] 'find-name-dired)
(global-set-key [(shift f2)] 'occur)

;; refresh like
(global-set-key [(f5)] 'revert-buffer)
(global-set-key [(control f5)] 'toggle-read-only)
(global-set-key [(shift f5)] 'ecb-redraw-layout)

(global-set-key (kbd "TAB") 'smart-tab)

;; set 'solarized dark' theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
(load-theme 'solarized-dark t)

;; maximize window
;(add-to-list 'load-path "~/.emacs.d/deps/maxframe")
;(require 'maxframe)
;(add-hook 'window-setup-hook 'maximize-frame t)

;;
(add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-2.6.7/emacs")
(require 'erlang-start)

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(setq erlang-root-dir "/usr/local/lib/erlang")
(add-to-list 'exec-path "/usr/local/lib/erlang/bin")
(setq erlang-man-root-dir "/usr/local/lib/erlang/man")

(add-to-list 'load-path "~/.emacs.d/deps/distel/elisp")
(require 'distel)
(distel-setup)

;; A number of the erlang-extended-mode key bindings are useful in the shell too

(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
                                        (lambda ()
                                                ;; add some Distel bindings to the Erlang shell
                                                (dolist (spec distel-shell-keys)
                                                        (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

(require 'flymake)

;; switch on flymake global
(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-xml-init ())

;; switch on flymake for erl only
;;(defun my-erlang-mode-hook ()
;;        (flymake-mode 1))
;;(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

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
