(use-package eglot
  :ensure t)

;; Tree-sitter auto
(use-package treesit-auto
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; JavaScript / TypeScript tree-sitter modes
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))

;; OCaml tree-sitter
(add-to-list 'major-mode-remap-alist '(tuareg-mode . ocaml-ts-mode))

;; Tree-sitter grammar sources
(with-eval-after-load 'treesit
  (dolist (entry
           '((javascript "https://github.com/tree-sitter/tree-sitter-javascript")
             (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src")
             (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
             (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml/interface")))
    (add-to-list 'treesit-language-source-alist entry)))


