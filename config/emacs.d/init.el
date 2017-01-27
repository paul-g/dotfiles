;; Disable startup message, tool bar and menu bar
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; Enable ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
;; Vertical ido display
(setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]" "\n-> " ""))

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; decent backup settings
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; Save recent files
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 1000)

(defun open-init()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(defun ido-find-recentf ()
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Create separate file for customizations
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; --- Package configuration
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package powerline
  :ensure t
  :config (powerline-center-evil-theme))

(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :config (evil-mode))

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config (progn
            (evil-leader/set-leader "<SPC>")
            (evil-leader/set-key "xr" 'xref-find-references)
            (evil-leader/set-key "xd" 'xref-find-definitions)
            (evil-leader/set-key "xa" 'xref-find-apropos)
            (evil-leader/set-key "bb" 'ido-switch-buffer)
            (evil-leader/set-key "ff" 'ido-find-file)
            (evil-leader/set-key "fr" 'ido-find-recentf)
            (evil-leader/set-key "fp" 'projectile-find-file)
            (evil-leader/set-key "fd" 'open-init)
            (evil-leader/set-key "om" 'whitespace-mode)
            (evil-leader/set-key "oc" 'whitespace-cleanup)
            (evil-leader/set-key "ob" 'indent-buffer)
            (which-key-add-key-based-replacements "<SPC> o" "formatting")
            (which-key-add-key-based-replacements "<SPC> x" "xref")
            (which-key-add-key-based-replacements "<SPC> x a" "apropos")
            (which-key-add-key-based-replacements "<SPC> x r" "references")
            (which-key-add-key-based-replacements "<SPC> x d" "definitions")))

(use-package evil-magit
  :ensure t
  :config (progn
            (evil-leader/set-key "gs" 'magit-status)
            (which-key-add-key-based-replacements "<SPC> g" "git")))

(use-package evil-nerd-commenter
  :ensure t
  :config (progn
            (evil-leader/set-key "cl" 'evilnc-comment-or-uncomment-lines)
            (which-key-add-key-based-replacements "<SPC> c" "comment")
            (which-key-add-key-based-replacements "<SPC> c l" "line")))

(use-package auto-complete
  :ensure t
  :config (global-auto-complete-mode t))

(use-package ace-window
  :ensure t
  :config (evil-leader/set-key "w" 'ace-window))

;; --- Helm
(use-package helm
  :ensure t)

(use-package helm-helm-commands
  :ensure t)

(use-package projectile
  :ensure t)

;; --- Org mode configuration
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

(load-theme 'leuven t)
