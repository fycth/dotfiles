
(require 'cl)

(package-initialize) ;; You might already have this line

(add-to-list 'exec-path "/usr/local/bin/")

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
    elscreen
    multiple-cursors
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

(require 'company)
(require 'compile)

(projectile-global-mode)


