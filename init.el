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
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-action-vc)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda))

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

(use-package smartparens
  :ensure smartparens
  :hook (prog-mode text-mode markdown-mode org-mode)
  :config
  (require 'smartparens-config))

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun my/copy-full-path-to-kill-ring ()
  "Copy the current buffer's full path to the kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

(use-package diredfl
  :defer t
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package markdown-mode
  :defer t)

(put 'dired-find-alternate-file 'disabled nil)
(eval-after-load "dired" '(progn
			    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
			    (define-key dired-mode-map (kbd "a") 'dired-find-file)))

(set-face-attribute 'default nil :family "Adwaita Mono" :height 105 :weight 'bold)

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
  ;; (variable-pitch-mode 1)
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
;; (with-eval-after-load 'org
;;   (dolist (face '((org-level-1 . 1.2)
;;                   (org-level-2 . 1.1)
;;                   (org-level-3 . 1.05)
;;                   (org-level-4 . 1.0)
;;                   (org-level-5 . 1.1)
;;                   (org-level-6 . 1.1)
;;                   (org-level-7 . 1.1)
;;                   (org-level-8 . 1.1)))
;;     (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))


;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

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

(global-set-key (kbd "C-x O") 'other-frame)

(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-O") 'ace-swap-window)

(global-set-key (kbd "C-c v n") 'multi-vterm)

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

(setq ivy-read-action-format-function #'ivy-read-action-format-columns)

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
        ("C-c t t" . treemacs)))

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
        (c-mode . c-ts-mode)
        (gdscript-mode . gdscript-ts-mode)))

(use-package reformatter
  :ensure t)

(reformatter-define gdformat-format
  :program "gdformat"
  :args '("-"))

(add-hook 'gdscript-ts-mode #'gdformat-format-on-save-mode)

(use-package projectile
  :defer t
  :config
  (projectile-load-known-projects))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package counsel-projectile
  :ensure t
  :config
  (setq counsel-projectile-preview-buffers t))

(counsel-projectile-mode +1)

;; Set the default switch project (C-c p p) action to open Magit in the project
(setq counsel-projectile-switch-project-action
      '(13
        ("o" counsel-projectile-switch-project-action
         "jump to a project buffer or file")
        ("f" counsel-projectile-switch-project-action-find-file
         "jump to a project file")
        ("d" counsel-projectile-switch-project-action-find-dir
         "jump to a project directory")
        ("D" counsel-projectile-switch-project-action-dired
         "open project in dired")
        ("b" counsel-projectile-switch-project-action-switch-to-buffer
         "jump to a project buffer")
        ("m" counsel-projectile-switch-project-action-find-file-manually
         "find file manually from project root")
        ("S" counsel-projectile-switch-project-action-save-all-buffers
         "save all project buffers")
        ("k" counsel-projectile-switch-project-action-kill-buffers
         "kill all project buffers")
        ("K" counsel-projectile-switch-project-action-remove-known-project
         "remove project from known projects")
        ("c" counsel-projectile-switch-project-action-compile
         "run project compilation command")
        ("C" counsel-projectile-switch-project-action-configure
         "run project configure command")
        ("E" counsel-projectile-switch-project-action-edit-dir-locals
         "edit project dir-locals")
        ("v" counsel-projectile-switch-project-action-vc
         "open project in vc-dir / magit / monky")
        ("sg" counsel-projectile-switch-project-action-grep
         "search project with grep")
        ("si" counsel-projectile-switch-project-action-git-grep
         "search project with git grep")
        ("ss" counsel-projectile-switch-project-action-ag
         "search project with ag")
        ("sr" counsel-projectile-switch-project-action-rg
         "search project with rg")
        ("xs" counsel-projectile-switch-project-action-run-shell
         "invoke shell from project root")
        ("xe" counsel-projectile-switch-project-action-run-eshell
         "invoke eshell from project root")
        ("xt" counsel-projectile-switch-project-action-run-term
         "invoke term from project root")
        ("xv" counsel-projectile-switch-project-action-run-vterm
         "invoke vterm from project root")
        ("Oc" counsel-projectile-switch-project-action-org-capture
         "capture into project")
        ("Oa" counsel-projectile-switch-project-action-org-agenda
         "open project agenda")))

;; Set the default behavior of the Magit buffer to reuse the current window
;; This prevents the projectile project switch from splitting the frame
(setq display-buffer-alist
    '(("magit:.**"
       (display-buffer-reuse-window display-buffer-same-window))))

(use-package vterm
  :ensure t)

(use-package ripgrep
  :ensure t)

(use-package multi-vterm
  :ensure t)

(use-package avy
  :ensure t
  :bind
  (:map global-map
        ("C-:" . 'avy-goto-char)))

(use-package gdscript-mode
  :defer t
  :hook (gdscript-ts-mode . eglot-ensure)
  :custom (gdscript-eglot-version 4))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(gdscript-ts-mode "localhost" 6005)))

(use-package crontab-mode
  :defer t)
