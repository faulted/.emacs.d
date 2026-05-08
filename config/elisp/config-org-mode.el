(defun set-specific-faces-org ()
  ;; (set-face-attribute 'org-code nil
  ;;                     :inherit '(shadow fixed-pitch))
  ;; Without indentation the headlines need to be different to be visible
  (set-face-attribute 'org-level-1 nil :height 1.25)
  (set-face-attribute 'org-level-2 nil :height 1.15)
  (set-face-attribute 'org-level-3 nil :height 1.1)
  (set-face-attribute 'org-level-4 nil :height 1.05)
  (set-face-attribute 'org-level-5 nil :height 1.0)
  (set-face-attribute 'org-date nil :height 0.8)
  (set-face-attribute 'org-document-title nil :height 1.5))

(defun org-mode-setup ()
  (org-indent-mode)
  (set-specific-faces-org)
  (display-line-numbers-mode 1)
  (visual-line-mode 1)
  (word-wrap-whitespace-mode 1)
  (font-lock-ensure))

(make-directory "~/Documents/org-files/" t)
(make-directory "~/Documents/org-agenda/" t)
(make-directory "~/Documents/org-roam/" t)
(make-directory "~/Documents/org-roam/daily/" t)

(use-package org
  :hook (org-mode . org-mode-setup))

;; Make sure org-indent face is available
(require 'org-indent)

(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (require 'org-indent)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sch" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("sh"  . "src shell"))
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
  (add-to-list 'org-agenda-files '("~/Documents/org-roam/"))
  (when (file-exists-p "~/Documents/org-agenda/agenda.org")
    (add-to-list 'org-agenda-files '("~/Documents/org-agenda/agenda.org"))))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-superstar
  :demand t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-indent-mode-turns-on-hiding-stars nil))

(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "WAITING" "|" "DONE" "RESCHEDULED" "CANCELLED"))))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/org-roam"))
  (org-roam-completion-everywhere t)
  :config
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)

  (setq org-roam-dailies-capture-templates
        '(("d" "default" plain
           "* To-do items\n\n* Notes\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>")
           :unnarrowed t)))
  
  (require 'org-roam-protocol)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today)
   ("C-c n a" . org-agenda)
   ("C-c n r" . org-roam-rg)
   :map org-capture-mode-map
   ("C-c C-c" . nil)
   ("C-c C-'" . org-capture-finalize)
   :map org-mode-map
   ("C-M-i"   . completion-at-point)
   ("C-<tab>" . org-indent-block)))

(defun my/org-roam-daily-add-newline ()
  "Add a blank line after the title in new daily notes."
  (run-with-timer 0.01 nil
                  (lambda (buf)
                    (when (org-roam-dailies--daily-note-p)
                      (goto-char (point-min))
                      (when (re-search-forward "^#\\+title:.*\n" nil t)
                        (unless (looking-at "^$")
                          (insert "\n")))))
                  (current-buffer)))

(add-hook 'org-roam-capture-new-node-hook #'my/org-roam-daily-add-newline)

(defun my/org-roam-dailies-previous-file (today-file)
  "Return the most recent daily note file before TODAY-FILE."
  (let* ((daily-dir (file-name-directory today-file))
         (all-files (sort (directory-files daily-dir t "\\.org$") #'string<))
         (before-today (seq-filter
                        (lambda (f) (string< f today-file))
                        all-files)))
    (car (last before-today))))

(defun my/migrate-todos (source-file target-file)
  "Move incomplete TODO headings from SOURCE-FILE into the
'* To-do items' section of TARGET-FILE."
  (let (todos)
    (with-current-buffer (find-file-noselect source-file)
      (org-mode)
      (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAITING" "|" "DONE" "RESCHEDULED" "CANCELLED")))
            (org-todo-keyword-alist '(("TODO" . ?t) ("NEXT" . ?n) ("WAITING" . ?w)
                                      ("DONE" . ?d) ("RESCHEDULED" . ?r) ("CANCELLED" . ?c))))
        (org-map-entries
         (lambda ()
           (when (member (org-get-todo-state) '("TODO" "NEXT" "WAITING"))
             (push (org-get-heading t t t t) todos)
             (org-todo "RESCHEDULED")))
         nil 'file))
      (save-buffer))
    (when todos
      (with-current-buffer (find-file-noselect target-file)
        (org-mode)
        (goto-char (point-min))
        (if (re-search-forward "^\\* To-do items" nil t)
            (progn
              (end-of-line)
              (dolist (todo (nreverse todos))
                (insert (format "\n** TODO %s" todo))))
          (goto-char (point-min))
          (re-search-forward "^#\\+title:.*\n" nil t)
          (insert "\n* To-do items\n")
          (dolist (todo (nreverse todos))
            (insert (format "** TODO %s\n" todo))))
        (save-buffer)))))

(defun my/org-roam-dailies-carry-over-todos ()
  "Carry over incomplete TODOs from the previous daily note to today's."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (today-file (expand-file-name
                      (concat today ".org")
                      (expand-file-name org-roam-dailies-directory
                                        org-roam-directory)))
         (previous-file (my/org-roam-dailies-previous-file today-file)))
    (when (and previous-file (file-exists-p previous-file))
      (my/migrate-todos previous-file today-file))))

(defun my/org-roam-dailies-goto-today-with-carryover ()
  "Open today's daily note, initializing it with the template if new,
then carry over any incomplete TODOs from the previous note."
  (interactive)
  (org-roam-dailies-capture-today)
  (my/org-roam-dailies-carry-over-todos))

(global-set-key (kbd "C-c n d") #'my/org-roam-dailies-goto-today-with-carryover)

(defun org-roam-rg ()
  "Ripgrep search the org-roam directory via consult."
  (interactive)
  (unless (boundp 'org-roam-directory)
    (error "org-roam-directory is not set"))
  (consult-ripgrep org-roam-directory))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(provide 'config-org-mode)
