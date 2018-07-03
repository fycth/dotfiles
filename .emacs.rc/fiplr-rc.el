
(require 'fiplr)
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" ".idea" "tmp" "log" ".stack-work"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
