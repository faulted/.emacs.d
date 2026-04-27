(delete-selection-mode 1)

(setq-default indent-tabs-mode nil
	          tab-width 4)

(setq create-lockfiles nil)

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

(setq kill-buffer-delete-auto-save-files t)

(advice-add 'recover-this-file :around #'recover-this-file-maybe-delete-autosave)

(with-eval-after-load 'no-littering
  (savehist-mode 1))

(with-eval-after-load 'no-littering
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-saved-items 50)
  (recentf-mode 1)
  
  ;; Saves the recent file list every five minutes
  (run-at-time nil (* 5 60) 'recentf-save-list)

  ;; Suppress the recentf-save message in the minibuffer
  (advice-add 'recentf-save-list :around #'silent-recentf-save))

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")

;; Set wdired-change-to-wdired-mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c w") 'wdired-change-to-wdired-mode))

;; Disable backup and auto-save for TRAMP files
(defun disable-tramp-backups ()
  "Disable backups and auto-saves for TRAMP files."
  (when (and buffer-file-name (file-remote-p buffer-file-name))
    (setq-local make-backup-files nil)
    (setq-local auto-save-default nil)
    (setq-local create-lockfiles nil)))

(add-hook 'find-file-hook #'disable-tramp-backups)

(unbind-key "C-x m")

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-x o") 'split-line)

(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x d") 'list-directory)

(provide 'config-editor)
