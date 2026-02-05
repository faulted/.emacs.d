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

(scroll-bar-mode -1)       ; Disable visible scrollbar
(tool-bar-mode -1)         ; Disable the toolbar
(menu-bar-mode -1)         ; Disable the menu bar
(set-fringe-mode 10)       ; Give some room on the sides
(column-number-mode 1)     ; Enable column numbers in mode-line
(delete-selection-mode 1)  ; Allow for overwriting regions when yanking from kill-ring

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

(which-key-mode)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 101)

(setq help-window-select t)

;; (auto-save-visited-mode 1)

(setq create-lockfiles nil)

(savehist-mode 1)

;; (setq recentf-auto-cleanup nil)
;; (setq recentf-max-saved-items 50)
;; (recentf-mode 1)
;; ;; Saves the recent file list every five minutes
;; (run-at-time nil (* 5 60) 'recentf-save-list)

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)

;; Disable backup and auto-save for TRAMP files
(defun disable-tramp-backups ()
  "Disable backups and auto-saves for TRAMP files."
  (when (and buffer-file-name (file-remote-p buffer-file-name))
    (setq-local make-backup-files nil)
    (setq-local auto-save-default nil)
    (setq-local create-lockfiles nil)))

(add-hook 'find-file-hook #'disable-tramp-backups)

;; Create ~/Documents/org-files/ directory if it doesn't exist
(let ((org-dir "~/Documents/org-files/"))
  (unless (file-exists-p org-dir)
    (make-directory org-dir t)))

(defun org-mode-setup ()
  ; (org-indent-mode)
  (auto-fill-mode 1)
  (display-line-numbers-mode -1)
  (setq fill-column 100))

(use-package org
  :hook (org-mode . org-mode-setup))

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

;; Keybinding used to call custom htop function
(global-set-key (kbd "C-c v h") 'htop)

;; Invert dired and list-directory bindings
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x d") 'list-directory)

(defun sudo-shell-command (command)
  (interactive "Command: ")
  (with-temp-buffer
    (cd "/sudo::/")
    (async-shell-command command)))

(defun visual-bell ()
  (invert-face 'mode-line-active)
  (invert-face 'mode-line-inactive)
  (run-with-timer 0.1 nil #'invert-face 'mode-line-active)
  (run-with-timer 0.1 nil #'invert-face 'mode-line-inactive))

(defun shutdown-computer ()
  "Prompt the user to shutdown the computer using `shutdown now`."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to shutdown the computer? ")
    (shell-command "shutdown now")))

(defun mandb ()
  "Prompt the user to update the mandb."
  (interactive)
  (when (yes-or-no-p "Update mandb? ")
    (sudo-shell-command "mandb")))

(defun updatedb ()
  "Promt the user to update the locate database."
  (interactive)
  (when (yes-or-no-p "Update locate database? ")
    (sudo-shell-command "updatedb")))

(defun htop ()
  "Open htop in a vterm buffer"
  (interactive)
  (require 'vterm)

  (let ((buf (vterm "*htop*")))
    (with-current-buffer buf
      
      ;; Clear any prompt and run htop
      (vterm-send-string "exec htop")
      (vterm-send-return))
    buf))

(setq ring-bell-function #'visual-bell)

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-nord t))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner 2)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents . 5)
			  (projects  . 5)
			  (agenda    . 5)
			  (bookmarks . 5)
			  (registers . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (setq dashboard-item-names '(("Agenda for the coming week:" . "Agenda:")))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-at-time 0.1 nil
			   (lambda ()
                             (when (eq major-mode 'dashboard-mode)
                               (revert-buffer :ignore-auto :noconfirm)))))))

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

  :bind
  (("C-c v n" . multi-vterm)
   ("C-c v r" . vterm-rename-buffer))

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
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("TAB" . dired-subtree-toggle)
	      ("<backtab>" . dired-subtree-remove)
	      ("S-TAB" . dired-subtree-remove))
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
  :defer t
  :bind
  (:map global-map
        ("C-x u" . vundo)))

(use-package magit
  :defer t
  :bind
  (:map global-map
	("C-x m m" . magit)
	("C-x m b" . magit-blame)))

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
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   :map minibuffer-local-map
   ("C-c C-c" . embark-collect)
   ("C-c C-e" . embark-export))
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
  :bind (("C-x c g" . consult-ripgrep)
	 ("C-x c G" . consult-git-grep)
	 ("C-x c l" . consult-locate)
	 ("C-x c f" . consult-find)
	 ("C-x c r" . consult-recent-file)
	 ("C-x c o" . consult-outline)
	 ;; Overwrites default Emacs behavior
	 ("C-x b" . consult-buffer)
	 ("C-x p b" . consult-project-buffer)
	 ("C-s" . consult-line)))

(use-package sudo-edit
  :defer t
  :after embark
  :bind
  (:map embark-file-map
	("s" . sudo-edit-find-file))
  (:map embark-become-file+buffer-map
	("s" . sudo-edit-find-file)))

(use-package wgrep
  :ensure t
  :bind
  (:map grep-mode-map
        ("e" . wgrep-change-to-wgrep-mode))
  :config
  ;; Finish edit ONLY with C-c C-c
  (define-key wgrep-mode-map (kbd "C-c C-c") #'wgrep-finish-edit)
  ;; Remove other default finish bindings
  (define-key wgrep-mode-map (kbd "C-x C-s") nil)
  (define-key wgrep-mode-map (kbd "C-c C-e") nil))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
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

(use-package multiple-cursors
  :demand t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-;" . mc/mark-all-like-this)
   ("C-\"" . mc/skip-to-next-like-this))
  :config
  (add-hook 'multiple-cursors-mode-hook
            (lambda ()
              (define-key mc/keymap (kbd "C-:")
                #'mc/skip-to-previous-like-this))))
