
(require 'git-gutter)

(global-git-gutter-mode t)

(custom-set-variables
 '(git-gutter:update-interval 60))

(custom-set-variables
 '(git-gutter:modified-sign "==") ;; two space
 '(git-gutter:added-sign "++")    ;; multiple character is OK
 '(git-gutter:deleted-sign "--"))

(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
