(defun set-specific-faces-org ()
  ;; (set-face-attribute 'org-code nil
  ;;                     :inherit '(shadow fixed-pitch))
  ;; Without indentation the headlines need to be different to be visible
  (set-face-attribute 'org-level-1 nil
                      :height 1.25)
  (set-face-attribute 'org-level-2 nil
                      :height 1.15)
  (set-face-attribute 'org-level-3 nil
                      :height 1.1)
  (set-face-attribute 'org-level-4 nil
                      :height 1.05)
  (set-face-attribute 'org-level-5 nil
                      :height 1.0)
  (set-face-attribute 'org-date nil
                      :height 0.8)
  (set-face-attribute 'org-document-title nil
                      :height 1.5))

;; Create ~/Documents/org-files/ directory if it doesn't exist
(let ((org-dir "~/Documents/org-files/"))
  (unless (file-exists-p org-dir)
    (make-directory org-dir t)))

(defun org-mode-setup ()
  (org-indent-mode)
  (set-specific-faces-org))

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
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

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
         ("C-M-i"   . completion-at-point)
         ("C-<tab>" . org-indent-block))
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
    (consult-ripgrep org-roam-directory)))

(global-set-key (kbd "C-c n r") #'org-roam-rg)

(add-hook 'org-mode-hook
          (lambda ()
            (font-lock-ensure))) ;; Ensure font-locking on org-mode activation

(use-package org-superstar
  :ensure t
  :demand t)

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-indent-mode-turns-on-hiding-stars nil)

(make-directory "~/Documents/org-agenda/" t)
(setq org-agenda-files '("~/Documents/org-agenda/agenda.org"))

(global-set-key (kbd "C-c n a") 'org-agenda)

(setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))

(add-hook 'org-mode-hook 'display-line-numbers-mode)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'word-wrap-whitespace-mode)

(provide 'org-mode)
