(scroll-bar-mode -1)           ; Disable scrollbar
(tool-bar-mode -1)             ; Disable toolbar
(menu-bar-mode -1)             ; Disable menu bar
(column-number-mode 1)         ; Show column in modeline
(setq inhibit-splash-screen t) ; Skip splash screen

(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

(when (member "JetBrainsMono Nerd Font" (font-family-list))
  (set-face-attribute 'default nil
                      :weight 'normal
                      :height 100
                      :family "JetBrainsMono Nerd Font"))

(setq ring-bell-function #'visual-bell)

(setq mode-line-position
      '(("(%l" (:eval (get-buffer-line-count)) ",%c) ")))

(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(advice-add 'vc-mode-line :after
            (lambda (&rest _)
              (when (stringp vc-mode)
                (setq vc-mode (concat vc-mode " ")))))

(defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string 
                   (format "^ %s\\W" (vc-backend buffer-file-name))
                   " " vc-mode)))
      (setq vc-mode noback))))

(which-key-mode)

(add-hook 'display-line-numbers-mode-hook
          (lambda () (setq display-line-numbers 'relative)))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(add-hook 'prog-mode-hook 'visual-line-mode)

(setq split-width-threshold 200)
(setq split-height-threshold nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 101)

(setq help-window-select t)

(provide 'config-ui)
