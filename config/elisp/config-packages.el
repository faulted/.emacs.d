(setq custom-safe-themes t)
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package no-littering)

(use-package vterm
  :custom
  (vterm-max-scrollback 20000)
  :hook
  (vterm-mode . (lambda () (setq-local mouse-drag-copy-region t)))
  :bind
  (("C-c v n" . multi-vterm)
   ("C-c v r" . vterm-rename-buffer)))

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("TAB" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)
        ("S-TAB" . dired-subtree-remove)))

(use-package trashed
  :commands (trashed)
  :config
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package magit
  :bind
  (("C-x m m" . magit)
   ("C-x m b" . magit-blame)))

(setq display-buffer-alist
      '(("magit:.**"
         (display-buffer-reuse-window display-buffer-same-window))))

(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m) t))

(use-package minions
  :config
  (minions-mode 1))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :config
  (setq consult-line-start-from-top t)
  :bind
  (("C-x b"   . consult-buffer)
   ("C-x p b" . consult-project-buffer)
   ("C-S-s"   . consult-line)
   ("C-x c g" . consult-ripgrep)
   ("C-x c G" . consult-git-grep)
   ("C-x c l" . consult-locate)
   ("C-x c f" . consult-fd)
   ("C-x c r" . consult-recent-file)
   ("C-x c o" . consult-outline)
   ("C-x c m" . consult-man)))

(use-package embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  (("C-." . embark-act)
   :map minibuffer-local-map
   ("C-c C-c" . embark-collect)
   ("C-c C-e" . embark-export)))

(use-package embark-consult)

(use-package sudo-edit
  :after embark
  :bind
  (:map embark-file-map
        ("s" . sudo-edit-find-file)
        :map embark-become-file+buffer-map
        ("s" . sudo-edit-find-file)))

(use-package wgrep
  :bind
  (:map grep-mode-map
        ("e" . wgrep-change-to-wgrep-mode)
        :map wgrep-mode-map
        ("C-c C-c" . wgrep-finish-edit)
        ;; Remove default finish bindings so C-c C-c is the only way out
        ("C-x C-s" . nil)
        ("C-c C-e" . nil)))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :config
  (setq tab-always-indent 'complete
        corfu-preview-current nil
        corfu-min-width 20
        corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  :bind (:map corfu-map
              ("<tab>" . corfu-complete)))

(use-package dabbrev
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)
  :bind
  (("M-/"   . dabbrev-completion)
   ("C-M-/" . dabbrev-expand)))

;; (use-package rainbow-delimiters
;;   :defer t
;;   :hook ((prog-mode org-mode) . rainbow-delimiters-mode))

(use-package multiple-cursors
  :demand t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"          . mc/mark-next-like-this)
   ("C-<"          . mc/mark-previous-like-this)
   ("C-;"          . mc/mark-all-like-this)
   ("C-\""         . mc/skip-to-next-like-this))
  :hook
  (multiple-cursors-mode . (lambda ()
                             (define-key mc/keymap (kbd "C-:")
                                         #'mc/skip-to-previous-like-this))))

(use-package ace-window
  :bind
  (("M-O"   . ace-window)
   ("C-M-o" . ace-swap-window)))

(use-package avy
  :after org
  :bind
  (("C-'"  . avy-goto-char)
   ("M-'"  . avy-goto-char-timer)
   :map org-mode-map
   ("C-'"  . avy-goto-char)))

(when (string-equal (face-attribute 'default :family) "JetBrainsMono Nerd Font")
  (use-package ligature
    :config
    (let ((ligature-set '("--" "---" "==" "===" "!=" "!==" "=!="
                          "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                          "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                          "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                          "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                          "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                          "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                          "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                          "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                          "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                          "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                          ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                          "<:<" ";;;")))
      (mapc (lambda (major-mode)
              (ligature-set-ligatures major-mode ligature-set))
              '(prog-mode org-mode)))
    (global-ligature-mode t)))

(provide 'config-packages)
