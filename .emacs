;; Steps to install:
;;   0. Install Haskell tools:
;;       * ghc
;;       * cabal
;;       * stylish-haskell
;;       * fast-tags
;;       * hoogle
;;   1. Copy ~/.emacs up to ;; PACKAGES ;;
;;   2. Restart Emacs
;;   3. Install packages (M-x list-packages) and then (i to mark for installation and "x" to install all)
;;       * <color theme of choice>
;;       * company
;;       * company-ghci
;;       * evil
;;       * ivy
;;       * lsp-mode
;;       * lsp-java
;;       * diff-hl
;;       * exec-path-from-shell
;;       * haskell-mode
;;       * markdown-mode
;;       * rainbow-delimeters
;;       * smart-mode-line
;;       * smex
;;       * use-package
;;   4. Copy the rest of .emacs file
;;   5. Restart and enjoy!

;; disable package signature check (dirty hack to make it work)
(setq package-check-signature nil)

;; Configure MELPA
(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes '(whiteboard))
 '(custom-safe-themes
   '("947190b4f17f78c39b0ab1ea95b1e6097cc9202d55c73a702395fc817f899393" "47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "2eb1f5551310e99101f0f9426485ab73aa5386054da877aacd15d438382bb72e" default))
 '(fci-rule-color "#383838")
 '(frame-brackground-mode 'dark)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-archive-priorities '(("MELPA Stable" . 10) ("GNU ELPA" . 5) ("MELPA" . 0)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(tabbar-ruler rustic ergoemacs-mode lsp lsp-java flycheck-haskell haskell-emacs xwwp-follow-link-ivy treemacs-evil treemacs use-package markdown-mode dracula-theme smart-mode-line company company-ghci exec-path-from-shell diff-hl haskell-mode rainbow-delimiters evil smex))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
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

;; disable toolbars and menus
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)

;; disable annoying beeping and blinking
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; set font
;;(set-frame-font "Ubuntu Mono-14")

;; put all auto-save files in a separate directory
(setq backup-directory-alist '(("." . "~/.emacs_saves")))

;; save desktop session on quit
(desktop-save-mode 1)

;; automatically update buffers when files change
(global-auto-revert-mode t)

(use-package flycheck :ensure t :init (global-flycheck-mode))

;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;

;; use-package: simple package configuration
(require 'use-package)

;; set $PATH from user shell
(use-package exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-initialize)

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

(use-package ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
;;(global-set-key "\C-s" 'swiper)
(define-key ivy-minibuffer-map (kbd "<DOWN>") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "<up>") 'ivy-previous-line)

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
(fset `yes-or-no-p `y-or-n-p)


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
    sml/theme 'dark
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
