
;; disable package signature check (dirty hack to make it work)
(setq package-check-signature nil)

;; Configure MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Install use-package
;;
;; With this package all other package will be installed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
;; when installing package, it will be always downloaded automatically from
;; repository if is not available locally
(setq use-package-always-ensure t)

;; set default startup screen
(setq inhibit-startup-screen t)

;; https://github.com/WJCFerguson/emacs-faff-theme
;;(use-package faff-theme)

;; disable toolbars and menus
(menu-bar-mode 1)
(tool-bar-mode 1)
(scroll-bar-mode 1)
(column-number-mode 1)

;; disable annoying beeping and blinking
;;(setq visible-bell t)
;;(setq ring-bell-function 'ignore)

;; set font
;;(set-frame-font "Ubuntu Mono-14")

;; put all auto-save files in a separate directory
;;(setq backup-directory-alist '(("." . "~/.emacs_saves")))

;; save desktop session on quit
;;(desktop-save-mode 1)

;; automatically update buffers when files change
;;(global-auto-revert-mode t)

;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;

;; use-package: simple package configuration
(require 'use-package)

(use-package kaolin-themes)
(load-theme 'kaolin-blossom t t)
(enable-theme 'kaolin-blossom)

;;(use-package flycheck :ensure t :init (global-flycheck-mode))

;; set $PATH from user shell
;;(use-package exec-path-from-shell)

;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))
;;(exec-path-from-shell-initialize)

;; ido (file navigation)
;;(require 'ido)
;;(ido-mode t)

;; smex (like ido but for commands)
;;(require 'smex)
;;(global-set-key (kbd "M-x") 'smex)

(use-package ergoemacs-mode)
(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(ergoemacs-mode 1)

(use-package helm)
(helm-mode 1)

;;(use-package ivy)
;;(ivy-mode 1)
;;(setq ivy-use-virtual-buffers t)
;;(setq ivy-count-format "(%d/%d) ")
;;(define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
;;(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
;;(global-set-key "\C-s" 'swiper)
;;(define-key ivy-minibuffer-map (kbd "<DOWN>") 'ivy-next-line)
;;(define-key ivy-minibuffer-map (kbd "<up>") 'ivy-previous-line)

;; vim bindings
(use-package evil)
;;(evil-mode t)
(add-hook
    'evil-insert-state-exit-hook
    (lambda () (call-interactively #'save-buffer))
)
(global-set-key (kbd "<f10>") 'evil-local-mode)

;; when in normal mode, cursor will be eyllow
;;(setq evil-normal-state-cursor '(box "yellow"))
;;(setq evil-insert-state-cursor '(bar "white"))

(use-package key-chord)
(key-chord-mode 1)

;; clm/open-command-log-buffer opens small buffer that shows all the keystrokes
;; and functions used while operating Emacs
;;(use-package command-log-mode
;;  :config
;;  (global-command-log-mode)
;;)

;; answer questions with y/n (instead of yes/no)
;;(fset `yes-or-no-p `y-or-n-p)

(use-package rainbow-delimiters)

;; rainbow delimeters
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

;; markdown
(use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode
        ( ("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode)
        )
    :init (setq markdown-command "multimarkdown")
)

(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk-11.0.9.jdk/Contents/Home/")
(setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/jdk-11.0.9.jdk/Contents/Home/bin/java")

(use-package lsp-mode
:ensure t
:hook (
   (lsp-mode . lsp-enable-which-key-integration)
   (java-mode . #'lsp-deferred)
)
:init (setq
    lsp-keymap-prefix "C-c l"              ; this is for which-key integration documentation, need to use lsp-mode-map
    lsp-enable-file-watchers nil
    read-process-output-max (* 1024 1024)  ; 1 mb
    lsp-completion-provider :capf
    lsp-idle-delay 0.500
)
:config
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
    (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
	(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
)

(use-package lsp-java
:ensure t
:config (add-hook 'java-mode-hook 'lsp))

(use-package helm-lsp
:ensure t
:after (lsp-mode)
:commands (helm-lsp-workspace-symbol)
:init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package lsp-ui
:ensure t
:after (lsp-mode)
:bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
:init (setq lsp-ui-doc-delay 1.5
      lsp-ui-doc-position 'bottom
	  lsp-ui-doc-max-width 100
)
:custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
         ("M-9" . lsp-treemacs-errors-list)))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))

(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
    (dap-session-created . (lambda (&_rest) (dap-hydra)))
    (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

(use-package dap-java :ensure nil)

(use-package which-key
:ensure t
:init
(which-key-mode)
)

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last))
  (:map company-mode-map
	("<tab>". tab-indent-or-complete)
	("TAB". tab-indent-or-complete))
)

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;;(use-package avy
;;:ensure t
;;:chords
;;("jc" . avy-goto-char)
;;("jw" . avy-goto-word-1)
;;("jl" . avy-goto-line))


;;;;;;;;;;;;;
;; HASKELL ;;
;;;;;;;;;;;;;

;;(use-package lsp)
(use-package lsp-haskell)
;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;;(use-package haskell-mode)
;;(use-package haskell-interactive-mode)
;;(use-package haskell-process)
;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)  ;; enable interactive mode
;;(add-hook 'haskell-mode-hook 'flyspell-prog-mode)  ;; spell checking in Haskell files
;;(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)  ;; default template

;;; configure interactive mode
(setq
    haskell-process-type 'cabal-repl
    haskell-process-log t
    haskell-interactive-popup-errors nil
    haskell-process-auto-import-loaded-modules t
    haskell-process-suggest-remove-import-lines t
    haskell-process-suggest-hoogle-imports t
)

;;: formatting
(setq haskell-stylish-on-save t)

;;; haskell layout offsets
(setq
    haskell-indentation-ifte-offset 2
    haskell-indentation-layout-offset 2
    haskell-indentation-left-offset 2
    haskell-indentation-starter-offset 2
    haskell-indentation-where-post-offset 2
    haskell-indentation-where-pre-offset 2
)

;;; haskell hotkeys
;;(define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)
;;(define-key haskell-mode-map (kbd "M-g") 'haskell-mode-tag-find)

;;; haskell autocompletion
;;(require 'company-ghci)
;;(push 'company-ghci company-backends)
;;(add-hook 'haskell-mode-hook 'company-mode)
;;(add-hook
;;    'haskell-mode-hook
;;    (lambda ()
;;        (set (make-local-variable 'company-backends)
;;             (append '((company-capf company-dabbrev-code)) company-backends)
;;	)
;;    )
;;)

(setq haskell-tags-on-save t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; V generated by emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; /\ generated by emacs /\ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EDITOR AND COLOR SETTINGS

;; Set cursor color to white
;;(set-cursor-color "#ffffff")

;; higlight line
(use-package hl-line)
(global-hl-line-mode t)
;;(set-face-background 'hl-line "#20385E")

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

;;(defun custom/kill-this-buffer ()
;;  (interactive) (kill-buffer (current-buffer)))
;;(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

;;(global-set-key (kbd "C-z") 'undo)
;;(global-set-key (kbd "C-x C-x") 'execute-extended-command)

;; rust
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b9e406b52f60a61c969f203958f406fed50b5db5ac16c127b86bbddd9d8444f7" default)))
