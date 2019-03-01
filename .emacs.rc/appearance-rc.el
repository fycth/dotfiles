
(load-theme 'whiteboard t)

(recentf-mode 1)

(display-time-mode 1)

(setq default-directory "~/dev")

(setq backup-directory-alist `((".*" . ,"~/.emacs.d/.emacs-saves")))
(setq auto-save-file-name-transforms `((".*" ,"~/.emacs.d/.emacs-saves" t)))
(setq backup-by-copying t)
(setq delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

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

;; work with utf-8
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8 'utf-8-unix)
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; use the clipboard buffer on X11
(setq x-select-enable-clipboard t)

;; overwrite an active region with a yank
(delete-selection-mode t)

;(electric-indent-mode t)
(global-set-key (kbd "RET") 'newline-and-indent)

;; useful to have a better overview where in the buffer the point is
;;(global-hl-line-mode t)

;; Open a file as root
(defun ck/find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (helm-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
(global-set-key (kbd "C-x F") 'ck/find-file-as-root)

(global-set-key (kbd "s-/") 'comment-line)

