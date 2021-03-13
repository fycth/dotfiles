;; disable package signature check (dirty hack to make it work)
(setq package-check-signature nil)

(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
	("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
	("MELPA" . 5)))

(setq package-enable-at-startup nil)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
;; when installing package, it will be always downloaded automatically from
;; repository if is not available locally
(setq use-package-always-ensure t)

(setq emacs-layers '(layer-startup
	       layer-performance
	       layer-better-defaults
	       layer-binds-global
	       layer-ergoemacs
	       layer-evil-mode
	       layer-yasnippet
	       layer-company

	       layer-lsp
;	       layer-markdown
	       layer-java
	       layer-haskell
	       layer-rust
	       ))

(add-to-list 'load-path (concat user-emacs-directory "emacs-layers"))

(dolist (layer emacs-layers)
  (require layer))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load-file custom-file))

(setq custom-lisp-d (concat user-emacs-directory "custom.d"))

(defun custom-user-local-hook ()
  (when (file-directory-p custom-lisp-d)
  (dolist (file (directory-files custom-lisp-d nil "^.*\.el$"))
    (load-file (concat custom-lisp-d "/" file)))))

(add-hook 'emacs-startup-hook 'custom-user-local-hook)

(defun emacs-reconfigure ()
  (interactive)
  (load-file user-init-file)
  (run-hooks 'after-init-hook
	 'emacs-startup-hook))

(use-package kaolin-themes)
(load-theme 'kaolin-valley-light t t)
(enable-theme 'kaolin-valley-light)

;; clm/open-command-log-buffer opens small buffer that shows all the keystrokes
;; and functions used while operating Emacs
;;(use-package command-log-mode
;;  :config
;;  (global-command-log-mode)
;;)
