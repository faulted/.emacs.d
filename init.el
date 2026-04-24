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

(scroll-bar-mode -1)            ; Disable visible scrollbar
(tool-bar-mode -1)              ; Disable the toolbar
(menu-bar-mode -1)              ; Disable the menu bar
(column-number-mode 1)          ; Enable column numbers in mode-line
(delete-selection-mode 1)       ; Allow for overwriting regions when yanking from kill-ring
(setq inhibit-splash-screen t)  ; Disable splash screen

(setq-default indent-tabs-mode nil
	          tab-width 4)

;; create the backup dir if necessary, since emacs won't.
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

;; create the backup dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(setq auto-save-file-name-transforms
      '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
        (".*" "~/.emacs.d/autosaves/" t)))

(global-display-line-numbers-mode 1)
(add-hook 'display-line-numbers-mode-hook
          (lambda () (setq display-line-numbers 'relative)))

(which-key-mode)

(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 101)

(setq help-window-select t)

(setq create-lockfiles nil)

(savehist-mode 1)

(setq recentf-auto-cleanup nil)
(setq recentf-max-saved-items 50)
(recentf-mode 1)
;; Saves the recent file list every five minutes
(run-at-time nil (* 5 60) 'recentf-save-list)

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")

;; Disable backup and auto-save for TRAMP files
(defun disable-tramp-backups ()
  "Disable backups and auto-saves for TRAMP files."
  (when (and buffer-file-name (file-remote-p buffer-file-name))
    (setq-local make-backup-files nil)
    (setq-local auto-save-default nil)
    (setq-local create-lockfiles nil)))

(add-hook 'find-file-hook #'disable-tramp-backups)

(setq split-width-threshold 200)
(setq split-height-threshold nil)

(setq mode-line-position
      '(("(%l" (:eval (get-buffer-line-count)) ",%c) ")))

(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(setq ring-bell-function #'visual-bell)

(advice-add 'vc-mode-line :after
            (lambda (&rest _)
              (when (stringp vc-mode)
                (setq vc-mode (concat vc-mode " ")))))

(add-to-list 'load-path (expand-file-name "init-files/" user-emacs-directory))

(require 'keybindings)
(require 'org-mode)
(require 'packages)
(require 'language-modes)
(require 'functions)
