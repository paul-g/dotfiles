;; Disable startup message, tool bar and menu bar
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(fset 'yes-or-no-p 'y-or-n-p)

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
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(use-package try)
(use-package elpy
  :pin elpy
  :config (elpy-enable)
  )

(use-package which-key
  :config (which-key-mode))

(use-package powerline
  :config (powerline-center-evil-theme))

(use-package magit)

(use-package helm-gtags)

(use-package evil-leader
  :init (global-evil-leader-mode)
  :config (progn
            (evil-leader/set-leader "<SPC>")
            (evil-leader/set-key
              "xr" 'xref-find-references
              "xd" 'xref-find-definitions
              "xa" 'xref-find-apropos
              "bb" 'ido-switch-buffer
              "ff" 'ido-find-file
              "fr" 'ido-find-recentf
              "fd" 'open-init
              "om" 'whitespace-mode
              "oc" 'whitespace-cleanup
              "ob" 'indent-buffer
              "pl" 'org-latex-export-to-latex
              "pf" 'projectile-find-file
              "pc" 'compile
              "pd" 'helm-gtags-dwim
              "pe" 'compile-goto-error
              "p>" 'next-error
              "p<" 'previous-error
              "sn" 'flyspell-goto-next-error
              "sa" 'flyspell-auto-correct-word)
            (which-key-add-key-based-replacements "<SPC> p" "project")
            (which-key-add-key-based-replacements "<SPC> o" "formatting")
            (which-key-add-key-based-replacements "<SPC> x" "xref")
            (which-key-add-key-based-replacements "<SPC> x a" "apropos")
            (which-key-add-key-based-replacements "<SPC> x r" "references")
            (which-key-add-key-based-replacements "<SPC> x d" "definitions")))

(use-package evil
  :config (evil-mode))

(use-package evil-magit
  :config (progn
            (evil-leader/set-key "gs" 'magit-status)
            (which-key-add-key-based-replacements "<SPC> g" "git")))

(use-package evil-nerd-commenter
  :config (progn
            (evil-leader/set-key "cl" 'evilnc-comment-or-uncomment-lines)
            (which-key-add-key-based-replacements "<SPC> c" "comment")
            (which-key-add-key-based-replacements "<SPC> c l" "line")))

(use-package ace-window
  :config (evil-leader/set-key "w" 'ace-window))

(use-package ace-jump-mode
  :config (evil-leader/set-key "<SPC>" 'ace-jump-mode))

;; --- Helm
(use-package helm)
(use-package helm-helm-commands)
(use-package projectile)

(defun check-load-file (f)
  (interactive)
  (message "Loading file")
  (if (file-exists-p f) (load-file f)))

(mapc 'check-load-file '("~/.emacs.d/init_local.el"
                         "~/.emacs.d/init_org.el"
                         "~/.emacs.d/init_autocomplete.el"
                         "~/.emacs.d/init_latex.el"
                         "~/.emacs.d/init_experiemental.el"))

(use-package srefactor
  :config (progn
            (global-semanticdb-minor-mode 1)
            (global-semantic-idle-scheduler-mode 1)
            (semantic-mode 1)
            (evil-leader/set-key "p r" 'srefactor-refactor-at-point)))

(load-theme 'leuven t)
(set-default-font "Ubuntu Mono 11")


(use-package cmake-mode)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
