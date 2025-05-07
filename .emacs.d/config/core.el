;; Bootstrap package system and use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

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

(setq custom-safe-themes t)

(global-set-key [remap list-buffers] 'ibuffer)

(global-set-key (kbd "M-o") 'other-window)

(windmove-default-keybindings)

