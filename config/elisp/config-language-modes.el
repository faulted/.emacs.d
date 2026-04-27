(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode rust-mode) .
                   ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))
  (setq rust-format-on-save t))

(provide 'config-language-modes)
