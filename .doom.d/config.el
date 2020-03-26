;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Andrii Serhiienko"
      user-mail-address "andrey.sergienko@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(require 'org-ref)

(require 'reformatter)

(setq org-link-search-must-match-exact-headline 'nil)

(customize-set-variable 'org-journal-dir "~/org/journal/")
(customize-set-variable 'org-journal-date-format "%A, %d %B %Y %H:%M")
(customize-set-variable 'org-journal-file-format "~/org/journal/%Y/%B-%d.org")
(customize-set-variable 'org-journal-file-type 'daily)
(require 'org-journal)
;;(add-to-list 'auto-mode-alist '("^/Users/as/org/journal/.*" . org-journal-mode))
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))
(define-key org-journal-mode-map (kbd "C-c C-c") 'org-journal-save-entry-and-exit)

(use-package ormolu
 :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
   ("C-c r" . ormolu-format-buffer)))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory "~/org/roam/")
  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-insert
        :desc "Org-Roam-Find"   "/" #'org-roam-find-file
        :desc "Org-Roam-Buffer" "r" #'org-roam)
  :config
  (org-roam-mode +1))

(use-package! deft
  :after org
;;  :bind
;;  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/roam/"))

;;(use-package popup) ;; dependency of haskell-tng-hsinspect

;;(use-package haskell-tng-mode
;;  :ensure nil
;;  :load-path "~/dev-tools/haskell-tng"
;;  :mode ((rx ".hs" eos) . haskell-tng-mode)

;;  :config
;;  (require 'haskell-tng-hsinspect)
;;  (require 'haskell-tng-extra)
;;  (require 'haskell-tng-extra-abbrev)
;;  (require 'haskell-tng-extra-company)
;;  (require 'haskell-tng-extra-projectile)
;;  (require 'haskell-tng-extra-smartparens)
;;  (require 'haskell-tng-extra-yasnippet)
;;  ;; (require 'haskell-tng-extra-stack)

;;  :bind
;;  (:map
;;   haskell-tng-mode-map
;;   ("RET" . haskell-tng-newline)
;;   ("C-c c" . haskell-tng-compile)
;;   ("C-c e" . next-error)))

