;; -- Useful built-ins --
(require 'cl)

;; -- Package management --
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar packages
  '(ack-and-a-half helm helm-ls-git
    ;; coding stuff
    ascope auto-complete autopair yasnippet magit haskell-mode
    python elpy jedi inf-ruby multiple-cursors flycheck
    ;; writing
    ace-jump-mode ebib markdown-mode key-chord
    ;; ui
    expand-region paredit projectile helm-projectile popup
    powerline volatile-highlights yaml-mode yari
    ;;themes
    zenburn-theme ir-black-theme solarized-theme))

(defun packages-installed-p ()
  (loop for p in packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; -- imports --
(require 'ace-jump-mode)
(require 'ascope)
(require 'auto-complete)
(require 'autopair)
(require 'ebib)
(require 'expand-region)
(require 'flycheck)
(require 'key-chord)
(require 'kmacro)
(require 'multiple-cursors)
(require 'popup)
(require 'volatile-highlights)
(require 'yasnippet)
(require 'powerline)
(require 'elpy)
(require 'projectile)

;; -- UI, Editing --
(powerline-default-theme)
(autopair-global-mode)
(desktop-save-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(global-flycheck-mode 1)
(ido-mode 1)
(load-theme 'ir-black t)
(menu-bar-mode -1)
(setq column-number-mode t)
(setq indent-tabs-mode nil
      tab-width 2
      whitespace-style '(face tabs trailing lines-tail))
(show-paren-mode 1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(volatile-highlights-mode t)
(pending-delete-mode t)
(key-chord-mode 1)
(set-default 'cursor-type 'bar)
(blink-cursor-mode 0)
(savehist-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; -- Coding --
(define-key global-map (kbd "RET") 'newline-and-indent) ; Auto indent on enter
(setq next-line-add-newlines t)                         ; Add new lines at end of buffer
(setq gdb-many-windows t)
(setq yas/root-directory '("~/.yasnippet-snippets"))
(mapc 'yas/load-directory yas/root-directory)
(yas-global-mode 1)
(global-auto-complete-mode 1)
(setq compilation-scroll-output 'first-error)

;; --- Python IDE ---
(elpy-enable)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'projectile-on)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional

;; -- Backups --
(setq backup-directory-alist `(("." . "~/.emacs-bkp"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; -- Hooks --
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-buffer)

;; Hide Compilation buffer if everything went OK
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (delete-window (get-buffer-window buf))
                        )
                      buffer)))

(setq bury-compile-buffer 1)
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(defun toggle-bury-compile-buffer  ()
  "Toggle burying compilation buffer on/off."
  (interactive)
  (setq bury-compile-buffer (- 1 bury-compile-buffer))
  (if (= bury-compile-buffer 0)
      (remove-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)
    (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)))

(defun indent-buffer ()
  "Indent buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; -- Treat annotations as comments --
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(setq recording-macro 0)
(defun toggle-record-macro ()
  "Toggle record macro on/off"
  (interactive)
  (setq recording-macro (- 1 recording-macro))
  (if (= recording-macro 1)
      (kmacro-start-macro 0)
    (kmacro-end-macro nil)))

(setq compilation-scroll-output 'first-error)
(defun toggle-auto-scroll-compilation-buffer ()
  "Toggle scroll compilation buffer on/off"
  (interactive)
  (if (equal compilation-scroll-output nil)
      (setq compilation-scroll-output 'first-error)
    (setq compilation-scroll-output nil )))

(defun bashrc ()
  "Open a buffer with bashrc."
  (interactive)
  (find-file "~/.bashrc"))

(defun emacsel ()
  "Open a buffer with emacs.el"
  (interactive)
  (find-file "~/.emacs.el"))



;; -- Key bindings
(global-set-key [f1] 'projectile-regenerate-tags)
(global-set-key [f2] 'kmacro-call-macro)
(global-set-key [f3] 'toggle-record-macro)
(global-set-key [f5] 'compile)
(global-set-key [f7] 'ff-find-other-file)
(global-set-key [f8] 'ascope-init)
(global-set-key [f9] 'ascope-find-this-symbol)
(global-set-key [f10] 'ascope-find-global-definition)
(global-set-key [f11] 'ascope-find-functions-calling-this-functions)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-<tab>") 'yas-expand)
(global-set-key (kbd "C-c n") 'indent-buffer)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(key-chord-define-global "qq" 'query-replace)
(key-chord-define-global "$$" 'magit-status)
(key-chord-define-global ",," 'other-window)
(key-chord-define-global "bb" 'switch-to-buffer)
(key-chord-define-global "jj" 'find-file)
(key-chord-define-global "aa" 'helm-ls-git-ls)
(key-chord-define-global "zz" 'helm-etags-select)
(key-chord-define-global "ZZ" 'helm-projectile)

;; --- Various customisations ---
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray11" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(ac-candidate-face ((t (:background "gray30"))))
 '(ac-completion-face ((t (:foreground "darkgray"))))
 '(ac-selection-face ((t (:background "dark olive green"))))
 '(error ((t (:foreground "dark red" :weight bold))))
 '(flycheck-error ((t (:foreground "firebrick"))))
 '(flyspell-duplicate ((t (:foreground "Gold3" :weight bold))))
 '(flyspell-duplicate-face ((t (:foreground "Gold3" :weight bold))) t)
 '(flyspell-incorrect ((t (:foreground "firebrick4" :weight bold))))
 '(flyspell-incorrect-face ((t (:foreground "firebrick4" :underline t :weight bold))) t)
 '(fringe ((t (:background "gray11"))))
 '(helm-candidate-number ((t (:background "dark olive green" :foreground "black"))))
 '(helm-selection ((t (:background "dark olive green" :underline nil))))
 '(helm-source-header ((t (:background "gray13" :foreground "white" :weight bold :height 1.0))))
 '(hl-line ((t (:color nil :style nil :background "#151515" :underline nil))))
 '(lazy-highlight ((t (:background "dark goldenrod" :foreground "#2F2F00"))))
 '(linum ((t (:inherit (shadow default) :background "gray11
" :height 0.8))))
 '(magit-item-highlight ((t (:background "gray14"))))
 '(mode-line ((t (:background "#202020" :foreground "#CCCCCC"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#202020" :foreground "#000000" :weight light))))
 '(popup-summary-face ((t (:background "gray30" :foreground "dark goldenrod"))))
 '(popup-tip-face ((t (:background "gray30" :foreground "white"))))
 '(powerline-active1 ((t (:background "dark olive green"))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray11"))))
 '(tooltip ((t (:inherit variable-pitch :background "gray30" :foreground "black"))))
 '(warning ((t (:foreground "dark goldenrod" :weight bold)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output (quote first-error))
 '(compilation-window-height 10)
 '(fringe-mode 15 nil (fringe))
 '(global-linum-mode t)
 '(helm-full-frame nil)
 '(helm-match-plugin-mode t nil (helm-match-plugin))
 '(helm-split-window-in-side-p t)
 '(powerline-default-separator (quote arrow))
 '(powerline-text-scale-factor nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))
