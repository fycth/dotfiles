;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(setq inhibit-startup-screen nil
      load-prefer-newer t)

(add-hook 'after-init-hook #'(lambda ()
			       (interactive)
			       (require 'server)
			       (or (server-running-p)
				   (server-start))))

(provide 'layer-startup)

