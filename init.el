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

(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable the toolbar
(menu-bar-mode -1)     ; Disable the menu bar
(set-fringe-mode 10)   ; Give some room on the sides
(column-number-mode 1) ; Enable column numbers in mode-line

;; Create ~/Documents/org-files/ directory if it doesn't exist
(let ((org-dir "~/Documents/org-files/"))
  (unless (file-exists-p org-dir)
    (make-directory org-dir t)))

(defun my/org-mode-setup ()
  ; (org-indent-mode)
  (auto-fill-mode 1)
  (display-line-numbers-mode -1)
  (setq fill-column 100))

(use-package org
  :hook (org-mode . my/org-mode-setup))

;; Make sure org-indent face is available
(require 'org-indent)

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

(setq org-roam-directory (file-truename "~/Documents/org-roam"))

(defun org-roam-rg ()
  "Ripgrep search the org-roam directory"
  (interactive)
  (unless (boundp 'org-roam-directory)
    (error "org-roam-directory is not set"))
  (let ((default-directory org-roam-directory))
    (counsel-rg nil org-roam-directory "-i" "Search org-roam (case insensitive): ")))

(global-set-key (kbd "C-c n r") #'org-roam-rg)

(add-hook 'org-mode-hook
          (lambda ()
            (font-lock-ensure))) ;; Ensure font-locking on org-mode activation

(make-directory "~/Documents/org-agenda/" t)
(setq org-agenda-files '("~/Documents/org-agenda/agenda.org"))

(global-set-key (kbd "C-c n a") 'org-agenda)

(which-key-mode)

;; Unbind compose-mail
(unbind-key "C-x m")

;; Set up ibuffer keybind
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Set up undo and redo keybindings
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)

;; Set up other-window and ace-window
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'ace-window)
(global-set-key (kbd "C-M-o") 'ace-swap-window)

;; Set up split-line since I just overwrote it with ace-swap-window
(global-set-key (kbd "C-x o") 'split-line)

(defun my/visual-bell ()
  (invert-face 'mode-line-active)
  (invert-face 'mode-line-inactive)
  (run-with-timer 0.1 nil #'invert-face 'mode-line-active)
  (run-with-timer 0.1 nil #'invert-face 'mode-line-inactive))

(setq ring-bell-function #'my/visual-bell)

;; ;; Dependency package for standard-themes
;; (use-package modus-themes
;;   :ensure t)

;; (use-package standard-themes
;;   :ensure t
;;   :init
;;   (standard-themes-take-over-modus-themes-mode 1)
;;   :config
;;   (setq modus-themes-mixed-fonts t)
;;   (setq modus-themes-italic-constructs t)
;;   (modus-themes-load-theme 'standard-light-tinted))

(use-package modus-themes
  :ensure t
  :config
  (modus-themes-load-theme 'modus-vivendi))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "EMACS")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents   . 5)
            (projects  . 5)
            (agenda    . 5)
            (bookmarks . 5)
            (registers . 5)))
  (setq dashboard-projects-backend 'projectile)
  ;; (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  ;; (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (setq dashboard-item-names '(("Agenda for the coming week:" . "Agenda:"))))

(use-package projectile
  :defer t
  :config
  (projectile-load-known-projects))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package no-littering)
