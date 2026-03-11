;; Keep backups/autosaves out of your working tree
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))
(setq create-lockfiles nil)

;; Package archives
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)

(package-refresh-contents)

(add-to-list 'package-pinned-packages '(transient . "gnu"))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Tree-sitter
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/lib"))
(setq treesit-font-lock-level 4)

(setq major-mode-remap-alist
      '((js-mode         . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (tuareg-mode     . ocaml-ts-mode)
        (c-mode          . c-ts-mode)))

(with-eval-after-load 'treesit
  (dolist (entry
           '((javascript "https://github.com/tree-sitter/tree-sitter-javascript")
             (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src")
             (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
             (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml/interface")))
    (add-to-list 'treesit-language-source-alist entry)))

;; Font
(set-face-attribute 'default nil :family "Fira Code" :height 160)

;; Ligatures (Emacs 29+)
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

(add-hook 'prog-mode-hook #'my/setup-ligatures-if-supported)

;; Theme
(setq custom-safe-themes t)
(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-soft t))

;; Completion: Vertico stack
(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-y"     . consult-yank-pop)
         ("M-s l"   . consult-line)
         ("M-s g"   . consult-grep)))

(use-package marginalia
  :init (marginalia-mode))

;; Persist minibuffer history
(savehist-mode 1)

;; Track recent files
(recentf-mode 1)

;; UI
(use-package which-key
  :config (which-key-mode))

(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(windmove-default-keybindings)
(global-set-key (kbd "M-i") 'imenu)
(global-font-lock-mode 1)

;; Word count
(use-package wc-mode
  :hook ((text-mode . wc-mode)
         (markdown-mode . wc-mode))
  :config
  (setq wc-modeline-format " WC:%tw"))
