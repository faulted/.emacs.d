;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1)    ; Disable the toolbar
(menu-bar-mode -1)    ; Disable the menu bar
(set-fringe-mode 10)  ; Give some room on the sides

(which-key-mode)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nord t))

(doom-themes-visual-bell-config)
(doom-themes-org-config)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  ((doom-modeline-height 30)
   (doom-modeline-total-line-number t)))

;; Automatically revert the buffer to keep the Git branch in the modeline up to date.
(setq auto-revert-check-vc-info t)

;; Put backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/backups/" t)

(global-display-line-numbers-mode t)

(use-package org-superstar
  :hook (org-mode))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sch" . "src scheme"))

(use-package org-roam
  :defer t)

(defun reload-init-file ()
  "Reload the user init file"
  (interactive)
  (load-file user-init-file))

(use-package rainbow-delimiters
  :defer t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'org-mode-hook 'rainbow-delimiters-mode)

(global-set-key (kbd "C-x O") 'other-frame)

(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)

(use-package counsel)

(use-package ivy
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setopt ivy-use-virtual-buffers t)
  (setopt ivy-count-format "(%d/%d) "))

(use-package swiper
  :bind
  (("M-C-s" . swiper)))

(use-package treemacs
  :defer t
  :config
  (progn
    (treemacs-follow-mode t))
  :bind
  (:map global-map
        ("C-x t t" . treemacs)))

(use-package treemacs-nerd-icons
  :after (treemacs)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package magit
  :defer t)

(use-package vundo
  :defer t
  :bind
  (:map global-map
        ("C-x u" . vundo)))

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-idle-delay 0.1)
(setq compandy-idle-delay  0.1)
(setq company-minimum-prefix-length 1)

(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(electric-indent-mode 1)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(add-hook 'before-save-hook 'untabify-buffer)
(add-hook 'prog-mode-hook (lambda ()
                            (setq indent-tabs-mode nil)))

(setq c-ts-mode-indent-style 'linux)
(setq c-ts-mode-indent-offset 4)

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (c-mode . c-ts-mode)))

(use-package pyvenv)

(use-package projectile
  :defer t)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package vterm
  :ensure t)
