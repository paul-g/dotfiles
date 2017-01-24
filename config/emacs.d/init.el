; Disable startup message, tool bar and menu bar
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)

; Create separate file for customizations
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

; --- Package configuration
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package evil
  :ensure t
  :config (evil-mode))

(use-package powerline
  :ensure t
  :config (powerline-center-evil-theme))

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)

(use-package evil-nerd-commenter
  :ensure t)

(use-package auto-complete
  :ensure t
  :config (global-auto-complete-mode t))

; --- Helm
(use-package helm
  :ensure t)

(use-package helm-helm-commands
  :ensure t)

; --- Org mode configuration
(use-package org
  :ensure org-plus-contrib)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("org-llncs"
		 "\\documentclass{llncs}
		 [NO-DEFAULT-PACKAGES]
		 [PACKAGES]
		 [EXTRA]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; \hypersetup seems to break LLNCS and other styles
(with-eval-after-load 'ox-latex
  (setq org-latex-with-hyperref nil))
