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

(add-to-list 'load-path (expand-file-name "config/elisp/" user-emacs-directory))

(require 'config-functions)
(require 'config-ui)
(require 'config-editor)
(require 'config-packages)
(require 'config-org-mode)
(require 'config-language-modes)

(let ((work-file (expand-file-name "config/elisp/config-work.el" user-emacs-directory)))
  (when (file-exists-p work-file)
    (require 'config-work)))
