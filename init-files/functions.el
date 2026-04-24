(defun visual-bell ()
  (invert-face 'mode-line-active)
  (invert-face 'mode-line-inactive)
  (run-with-timer 0.1 nil #'invert-face 'mode-line-active)
  (run-with-timer 0.1 nil #'invert-face 'mode-line-inactive))

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

(provide 'functions)
