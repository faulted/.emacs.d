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

(use-package modus-themes
  :ensure t
  :config
  (modus-themes-load-theme 'modus-vivendi))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner 2)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents   . 5)
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
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)))               ;; orig. previous-matching-history-element

(use-package sudo-edit
  :defer t
  :after embark
  :bind
  (:map embark-file-map
	("s" . sudo-edit-find-file))
  (:map embark-become-file+buffer-map
	("s" . sudo-edit-find-file)))
