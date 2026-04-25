;; Unbind compose-mail
(unbind-key "C-x m")

;; Set up ibuffer keybind
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Set up undo and redo keybindings
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)

;; Set up other-window
(global-set-key (kbd "M-o") 'other-window)

;; Set up split-line since I just overwrote it with ace-swap-window
(global-set-key (kbd "C-x o") 'split-line)

;; Keybinding used to call custom htop function
(global-set-key (kbd "C-c v h") 'htop)

;; Invert dired and list-directory bindings
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x d") 'list-directory)

;; Set wdired-change-to-wdired-mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c w") 'wdired-change-to-wdired-mode))

(with-eval-after-load 'vterm
  (global-set-key (kbd "C-c v n") 'multi-vterm)
  (global-set-key (kbd "C-c v r") 'vterm-rename-buffer))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "<backtab>") 'dired-subtree-remove)
  (define-key dired-mode-map (kbd "S-TAB") 'dired-subtree-remove))

(with-eval-after-load 'vundo
  (global-set-key (kbd "C-x u") 'vundo))

(with-eval-after-load 'magit
  (global-set-key (kbd "C-x m m") 'magit)
  (global-set-key (kbd "C-x m b") 'magit-blame))

(with-eval-after-load 'marginalia
  (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle))

(with-eval-after-load 'embark
  (global-set-key (kbd "C-.") 'embark-act)
  (define-key minibuffer-local-map (kbd "C-c C-c") 'embark-collect)
  (define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export))

(with-eval-after-load 'consult
  (global-set-key (kbd "C-x c g") 'consult-ripgrep)
  (global-set-key (kbd "C-x c G") 'consult-git-grep)
  (global-set-key (kbd "C-x c l") 'consult-locate)
  (global-set-key (kbd "C-x c f") 'consult-fd)
  (global-set-key (kbd "C-x c r") 'consult-recent-file)
  (global-set-key (kbd "C-x c o") 'consult-outline)
  (global-set-key (kbd "C-x c m") 'consult-man))

(with-eval-after-load 'consult
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key (kbd "C-x p b") 'consult-project-buffer)
  (global-set-key (kbd "C-S-s") 'consult-line))

(with-eval-after-load 'sudo-edit
  (define-key embark-file-map (kbd "s") 'sudo-edit-find-file)
  (define-key embark-become-file+buffer-map (kbd "s") 'sudo-edit-find-file))

(with-eval-after-load 'wgrep
  (define-key grep-mode-map (kbd "e") 'wgrep-change-to-wgrep-mode)
  ;; Finish edit ONLY with C-c C-c
  (define-key wgrep-mode-map (kbd "C-c C-c") #'wgrep-finish-edit)
  ;; Remove other default finish bindings
  (define-key wgrep-mode-map (kbd "C-x C-s") nil)
  (define-key wgrep-mode-map (kbd "C-c C-e") nil))

(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "<tab>") 'corfu-complete))

(with-eval-after-load 'dabbrev
  (global-set-key (kbd "M-/") 'dabbrev-completion)
  (global-set-key (kbd "C-M-/") 'dabbrev-expand))

(with-eval-after-load 'multiple-cursors
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-;") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-\"") 'mc/skip-to-next-like-this)
  (add-hook 'multiple-cursors-mode-hook
            (lambda ()
              (define-key mc/keymap (kbd "C-:")
                          #'mc/skip-to-previous-like-this))))

(with-eval-after-load 'ace-window
  (global-set-key (kbd "M-O") 'ace-window)
  (global-set-key (kbd "C-M-o") 'ace-swap-window))

(with-eval-after-load 'avy
  (with-eval-after-load 'org
    (global-set-key (kbd "C-'") 'avy-goto-char)
    (global-set-key (kbd "M-'") 'avy-goto-char-timer)))

(provide 'keybindings)
