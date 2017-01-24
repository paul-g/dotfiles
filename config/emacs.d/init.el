(setq inhibit-startup-message t)

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

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
