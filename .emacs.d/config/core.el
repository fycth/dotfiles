;; Keep backups/autosaves out of your org tree
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))
(setq create-lockfiles nil)

(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/lib"))
(setq treesit-font-lock-level 4)

;; Remap old modes to Tree-sitter-based ones
(setq major-mode-remap-alist
      '((js-mode          . js-ts-mode)
        (typescript-mode  . typescript-ts-mode)
        (tuareg-mode      . ocaml-ts-mode)
	(c-mode           . c-ts-mode)
	(lisp-mode        . common-lisp-ts-mode)
	))

;; Bootstrap package system and use-package
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Refresh once per session if nothing is cached
(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'package-pinned-packages '(transient . "gnu"))

;; Font
(set-face-attribute 'default nil :family "Fira Code" :height 160)

;; Ligatures (Emacs 29+ only)
(when (version<= "29.0" emacs-version)
  (defvar my/ligature-fonts
    '("Fira Code" "JetBrains Mono" "Iosevka" "Hasklig" "Cascadia Code" "Recursive"))

  (defun my/enable-all-ligatures ()
    (setq-local composition-function-table (make-char-table nil))
    (set-char-table-range composition-function-table t
                          #'(lambda (start end _match)
                              (compose-region start end))))

  (defun my/setup-ligatures-if-supported ()
    (let ((font (face-attribute 'default :family)))
      (when (and (member font my/ligature-fonts)
                 (derived-mode-p 'prog-mode))
        (my/enable-all-ligatures))))

  (add-hook 'prog-mode-hook #'my/setup-ligatures-if-supported))

;; UI/UX
(use-package which-key
  :config (which-key-mode))

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-soft t))

(fido-vertical-mode t)

(setq custom-safe-themes t)

(global-set-key [remap list-buffers] 'ibuffer)

(global-set-key (kbd "M-o") 'other-window)

(windmove-default-keybindings)

(global-set-key (kbd "M-i") 'imenu)

(global-font-lock-mode 1)

(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  :bind
  (("M-x"     . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-mini)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-r" . helm-recentf)
   ("M-y"     . helm-show-kill-ring)))

;; A modern, robust completion system to replace the buggy icomplete-mode
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Optional but recommended packages for a better experience
(use-package consult
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Make sure icomplete is disabled if you have it enabled elsewhere
(icomplete-mode -1)
