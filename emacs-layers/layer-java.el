(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk-11.0.9.jdk/Contents/Home/")
(setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/jdk-11.0.9.jdk/Contents/Home/bin/java")

(use-package lsp-java
:ensure t
:config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
    (dap-session-created . (lambda (&_rest) (dap-hydra)))
    (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

(use-package dap-java :ensure nil)

(provide 'layer-java)

