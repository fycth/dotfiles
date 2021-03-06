;; vim bindings
(use-package evil)
;;(evil-mode t)
(add-hook
    'evil-insert-state-exit-hook
    (lambda () (call-interactively #'save-buffer))
)
(global-set-key (kbd "<f10>") 'evil-local-mode)

(provide 'layer-evil-mode)
