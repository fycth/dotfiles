
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
