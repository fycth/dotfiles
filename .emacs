
;;One Emacs instance per project, fiplr for going to files. F1 bound to magit-status, F5 bound to a wrapper around compile, and a dedicated window for compile.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'exec-path "/usr/local/bin/")

(setq default-directory "~/dev")

(require 'cl)
(require 'package)

;; declare package sources
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))

;; define a list of packages to install via package manager
(defvar my-packages
  '(intero
    fiplr
    magit
    go-projectile
    flycheck
    gotest
    company
    company-go
    projectile
    recentf-ext
    erlang
    web-mode
    dash-at-point
    neotree
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

;; turn ON web-mode for HTML and JS files
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))

;(global-hl-line-mode nil)
;(toggle-hl-line-when-idle)

(projectile-global-mode)

(setq projectile-switch-project-action 'neotree-projectile-action)
(require 'neotree)
(setq neo-smart-open t)
(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (if (bound-and-true-p flymake-mode)
	(let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
    	(neotree-toggle)
        (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root.")))
    (neotree-toggle))
)
;(global-set-key [f8] 'neotree-toggle)
(global-set-key [f8] 'neotree-project-dir)

(recentf-mode 1)

(display-time-mode 1)

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

;; Open Dash docs at the cursor point
(global-set-key (kbd "C-x d") 'dash-at-point)

;; work with utf-8
(set-buffer-file-coding-system 'utf-8 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

; As-you-type error highlighting
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'haskell-mode)
(require 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)
(require 'flycheck)
(setq flycheck-check-syntax-automatically '(save new-line))
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))
(intero-global-mode 1)

(require 'fiplr)
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" ".idea" "tmp" "log" ".stack-work"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

(defun my-go-mode-hook ()
      (setq tab-width 2 indent-tabs-mode 1)
      ; eldoc shows the signature of the function at point in the status bar.
      (go-eldoc-setup)
      (local-set-key (kbd "M-.") #'godef-jump)
      (add-hook 'before-save-hook 'gofmt-before-save)

      ; extra keybindings from https://github.com/bbatsov/prelude/blob/master/modules/prelude-go.el
      (let ((map go-mode-map))
        (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
        (define-key map (kbd "C-c m") 'go-test-current-file)
        (define-key map (kbd "C-c .") 'go-test-current-test)
        (define-key map (kbd "C-c b") 'go-run)))
(add-hook 'go-mode-hook 'my-go-mode-hook)

; "projectile" recognizes git repos (etc) as "projects" and changes settings
; as you switch between them. 
(projectile-global-mode 1)
(require 'go-projectile)
(go-projectile-tools-add-path)
(setq gofmt-command (concat go-projectile-tools-path "/bin/goimports"))

; "company" is auto-completion
(require 'company)
(require 'go-mode)
(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
                          (company-mode)
                          (set (make-local-variable 'company-backends) '(company-go))))

; gotest defines a better set of error regexps for go tests, but it only
; enables them when using its own functions. Add them globally for use in
(require 'compile)
(require 'gotest)
(dolist (elt go-test-compilation-error-regexp-alist-alist)
  (add-to-list 'compilation-error-regexp-alist-alist elt))
(defun prepend-go-compilation-regexps ()
  (dolist (elt (reverse go-test-compilation-error-regexp-alist))
    (add-to-list 'compilation-error-regexp-alist elt t)))
(add-hook 'go-mode-hook 'prepend-go-compilation-regexps)

;(go-projectile-install-tools)
;(go-projectile-update-tools)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (intero))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
