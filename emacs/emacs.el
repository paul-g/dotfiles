;; -- Useful built-ins --
(require 'cl)

;; -- package management -- (Based on
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; -- Fetch required packages --
;; Based on a post by Boris Batsov
;;  http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/)
(defvar packages
  '(ack-and-a-half
    ;; coding stuff
    ascope auto-complete yasnippet clojure-mode coffee-mode magit haskell-mode python inf-ruby
    ;; writing
    ebib markdown-mode
    ;; ui
    expand-region paredit projectile popup
    volatile-highlights yaml-mode yari
    ;;themes
    zenburn-theme ir-black-theme solarized-theme)
  "A list of packages to ensure are installed at launch.")

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


;; -- imports --
(require 'ascope)
(require 'auto-complete)
(require 'ebib)
(require 'popup)
(require 'volatile-highlights)
(require 'yasnippet)


;; -- UI --
(global-hl-line-mode 0)
(global-linum-mode 1)
(load-theme 'ir-black t)
(menu-bar-mode -1)
(setq column-number-mode t)
(show-paren-mode 1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
;;(volatile-highlights-mode t)
(setq tab-width 2)
(setq indent-tabs-mode nil)

;; -- GDB configuration --
(setq gdb-many-windows t)

;; -- Enable ido switch buffer
(ido-mode 1)

;; -- Save open buffers on close --
(desktop-save-mode 1)

;; -- Coding --
(define-key global-map (kbd "RET") 'newline-and-indent) ; Auto indent on enter
(setq next-line-add-newlines t)                         ; Add new lines at end of buffer

;; Setup yasnippet
(setq yas/root-directory '("~/.yasnippet-snippets"))

;; Map `yas/load-directory' to every element
(mapc 'yas/load-directory yas/root-directory)
(yas-global-mode 1)
(global-auto-complete-mode 1)


;; -- Useful functions --
(add-hook 'after-save-hook 'compile-latex-hook)

(defun compile-latex-hook()
  (when (eq major-mode 'latex-mode)
    (recompile))
  )

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
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(defun indent-buffer ()
  "Indent buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


;; -- Key bindings
(global-set-key [f1] 'ascope-init)
(global-set-key [f2] 'ascope-find-this-symbol)
(global-set-key [f3] 'ascope-find-global-definition)
(global-set-key [f4] 'ascope-find-functions-calling-this-functions)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'ff-find-other-file)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-<tab>") 'yas-expand)
(global-set-key (kbd "C-c n") 'indent-buffer)
