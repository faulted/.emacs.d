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

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/backups/" t)

;; Put backup files (ie foo~) in ~/.emacs.d/.
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; Use copying to preserve symlinks
(setq backup-by-copying t)

;; Keep multiple versions
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun my/reload-init-file ()
  "Reload the user init file"
  (interactive)
  (load-file user-init-file))

(use-package rainbow-delimiters
  :defer t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'org-mode-hook 'rainbow-delimiters-mode)

(use-package diredfl
  :defer t
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Disable backup and auto-save for TRAMP files
(defun my-disable-tramp-backups ()
  "Disable backups and auto-saves for TRAMP files."
  (when (and buffer-file-name (file-remote-p buffer-file-name))
    (setq-local make-backup-files nil)
    (setq-local auto-save-default nil)
    (setq-local create-lockfiles nil)))

(add-hook 'find-file-hook #'my-disable-tramp-backups)

(defun my/gpg-verify-detached (sig data)
  "Verify GPG detached signature SIG for DATA."
  (interactive
   (list (read-file-name "Signature file (.sig): ")
         (read-file-name "Data file: ")))
  (let ((buf (get-buffer-create "*GPG Verify*")))
    (with-current-buffer buf
      (erase-buffer)
      (let ((code (call-process "gpg" nil buf t "--verify" sig data)))
        (if (= code 0)
            (message "Signature is valid.")
          (message "Signature verification failed.")))
      (display-buffer buf))))

;; (let ((password-store-dir "~/Documents/org-files/"))
;;   (unless (file-exists-p password-store-dir)
;;     (make-directory password-store-dir t)))

(use-package password-store
  :disabled)

(use-package password-store-otp
  :disabled)

(use-package pass
  :disabled)

;; Create ~/Documents/org-files/ directory if it doesn't exist
(let ((org-dir "~/Documents/org-files/"))
  (unless (file-exists-p org-dir)
    (make-directory org-dir t)))

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 1)
  (display-line-numbers-mode -1)
  (setq fill-column 120))

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

(use-package org-superstar
  :hook (org-mode))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(with-eval-after-load 'org
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))


;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(defface ivy-org
  '((t :inherit default))
  "Face used by Ivy for highlighting Org buffers in the alternatives.")

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sch" . "src scheme"))

;; Create ~/Documents/org-files/ directory if it doesn't exist
(let ((org-roam-dir "~/Documents/org-roam/"))
  (unless (file-exists-p org-roam-dir)
    (make-directory org-roam-dir t)))

;; Create ~/Documents/org-files/ directory if it doesn't exist
(let ((org-roam-daily-dir "~/Documents/org-roam/daily/"))
  (unless (file-exists-p org-roam-daily-dir)
    (make-directory org-roam-daily-dir t)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/org-roam"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-capture-mode-map
         ("C-c C-c" . nil)
         ("C-c C-'" . org-capture-finalize)
         :map org-mode-map
         ("C-M-i"   . completion-at-point))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(add-hook 'org-mode-hook
          (lambda ()
            (font-lock-ensure))) ;; Ensure font-locking on org-mode activation

(global-set-key (kbd "C-x O") 'other-frame)

(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)

(global-set-key (kbd "M-o") 'ace-window)

(use-package counsel
  :config
  (keymap-global-set "C-h v" #'counsel-describe-variable)
  (keymap-global-set "C-h f" #'counsel-describe-function)
  (keymap-global-set "C-h o" #'counsel-describe-symbol)
  (keymap-global-set "C-x C-f" #'counsel-find-file)
  (keymap-global-set "M-x" #'counsel-M-x)
  (keymap-global-set "C-c r" #'counsel-rg)
  (keymap-global-set "C-c l" #'counsel-locate)
  (keymap-global-set "C-x b" #'counsel-switch-buffer))

(use-package ivy
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-height 15)
  (setopt ivy-use-virtual-buffers t)
  (setopt ivy-count-format "(%d/%d) "))

;; Remove the leading regex "^" from the counsel search for M-x
(ivy-configure 'counsel-M-x
  :initial-input ""
  :display-transformer-fn #'counsel-M-x-transformer)

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package swiper
  :config
  (keymap-global-set "C-M-s" #'swiper)
  (keymap-global-set "C-s" #'swiper-isearch))

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

(use-package tablist)

(use-package pdf-tools)
(pdf-loader-install)

(defun my/pdf-mode-hook ()
  (display-line-numbers-mode -1))
(add-hook 'pdf-view-mode-hook 'my/pdf-mode-hook)

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

(defun my/untabify-buffer ()
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

(use-package projectile
  :defer t)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package vterm
  :ensure t)

(use-package avy
  :ensure t
  :bind
  (:map global-map
        ("C-:" . 'avy-goto-char)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company counsel diredfl doom-modeline doom-themes ivy-rich magit
             nerd-icons-dired org-auto-tangle org-roam org-superstar
             pass pdf-tools projectile pyvenv rainbow-delimiters
             treemacs-nerd-icons vterm vundo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
