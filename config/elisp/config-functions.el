(defvar visual-bell--timer nil)

(defun visual-bell ()
  (when visual-bell--timer
    (cancel-timer visual-bell--timer)
    (setq visual-bell--timer nil)
    (copy-face 'mode-line-active--backup 'mode-line-active)
    (copy-face 'mode-line-inactive--backup 'mode-line-inactive))
  (copy-face 'mode-line-active 'mode-line-active--backup)
  (copy-face 'mode-line-inactive 'mode-line-inactive--backup)
  (invert-face 'mode-line-active)
  (invert-face 'mode-line-inactive)
  (setq visual-bell--timer
        (run-with-timer 0.1 nil
                        (lambda ()
                          (setq visual-bell--timer nil)
                          (copy-face 'mode-line-active--backup 'mode-line-active)
                          (copy-face 'mode-line-inactive--backup 'mode-line-inactive)))))

(defun get-buffer-line-count ()
  (format "/%d" (count-lines (point-min) (point-max))))

(defun recover-this-file-maybe-delete-autosave (orig-fun &rest args)
  "After declining recovery, offer to delete the autosave file."
  (condition-case err
      (apply orig-fun args)
    (error
     (if (string-match-p "canceled" (error-message-string err))
         (let ((auto-save-file (make-auto-save-file-name)))
           (when (and auto-save-file
                      (file-exists-p auto-save-file)
                      (yes-or-no-p
                       (format "Delete autosave file %s? " auto-save-file)))
             (delete-file auto-save-file)
             (message "Autosave file deleted.")))
       (signal (car err) (cdr err))))))

(defun silent-recentf-save (&rest args)
  (let ((inhibit-message t))
    (apply (car args) (cdr args))))

(defun sudo-shell-command (command)
  (interactive "Command: ")
  (with-temp-buffer
    (cd "/sudo::/")
    (async-shell-command command)))

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

(defun describe-active-modes-minibuffer ()
  "Select and describe an active mode using the minibuffer (works with Vertico)."
  (interactive)
  (let* ((major (symbol-name major-mode))
         (minors (mapcar #'symbol-name local-minor-modes))
         (global-minors (mapcar #'symbol-name
                                (seq-filter (lambda (m)
                                              (and (boundp m) (symbol-value m)))
                                            global-minor-modes)))
         ;; Annotate major mode so it's visually distinct in the list
         (candidates
          (append
           (list (propertize major 'display
                             (concat (propertize "[major] " 'face '(:foreground "cyan" :weight bold))
                                     major)))
           (mapcar (lambda (m)
                     (propertize m 'display
                                 (concat (propertize "[minor] " 'face '(:foreground "yellow"))
                                         m)))
                   (seq-uniq (append minors global-minors)))))
         (chosen (completing-read "Active modes: " candidates nil t)))
    (when chosen
      (let ((sym (intern chosen)))
        (describe-function sym)))))

(defun describe-active-modes--keymap-action (mode-name)
  "Describe the keymap for MODE-NAME if one exists."
  (let ((map-sym (intern (concat mode-name "-map"))))
    (if (and (boundp map-sym) (keymapp (symbol-value map-sym)))
        (describe-keymap map-sym)
      (message "No keymap found for %s" mode-name))))

;; Wire up an Embark action so you can hit your embark key on any candidate
(with-eval-after-load 'embark
  (define-key embark-general-map (kbd "K") #'describe-active-modes--keymap-action))

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

(global-set-key (kbd "C-c v h") 'htop)

(provide 'config-functions)
