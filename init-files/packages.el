(setq custom-safe-themes t)
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package no-littering)

(use-package vterm
  :ensure t
  :preface
  (defun multi-vterm ()
    (interactive)
    (let ((n 1)
          name)
      (while (get-buffer (setq name (format "*vterm*<%d>" n)))
        (setq n (1+ n)))
      (vterm name)))

  (defun vterm-rename-buffer (name)
    (interactive "sName? ")
    (unless (eq major-mode 'vterm-mode)
      (user-error "Not a vterm buffer"))
    (rename-buffer (format "*vterm*<%s>" name)))

  :custom
  (vterm-max-scrollback 20000))

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode org-mode) . rainbow-delimiters-mode))

(use-package diredfl
  :defer t
  :hook (dired-mode . diredfl-mode))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package vundo
  :defer t)

(use-package magit
  :defer t)

;; Set the default behavior of the Magit buffer to reuse the current window
(setq display-buffer-alist
    '(("magit:.**"
       (display-buffer-reuse-window display-buffer-same-window))))

(use-package minions
  :config
  (minions-mode 1))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :config
  (setq consult-line-start-from-top t))

(use-package sudo-edit
  :defer t
  :after embark)

(use-package wgrep
  :ensure t)

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package multiple-cursors
  :demand t)

(use-package ace-window
  :ensure t)

(provide 'packages)
