;; -- Useful built-ins --
(require 'cl)

;; -- Package management --
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar packages
  '(ack-and-a-half
    ;; coding stuff
    ascope auto-complete yasnippet magit haskell-mode
    python inf-ruby multiple-cursors
    ;; writing
    ebib markdown-mode key-chord
    ;; ui
    expand-region paredit projectile popup
    volatile-highlights yaml-mode yari auto-dim-other-buffers
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
(require 'ascope)
(require 'auto-complete)
(require 'ebib)
(require 'expand-region)
(require 'key-chord)
(require 'kmacro)
(require 'multiple-cursors)
(require 'popup)
(require 'volatile-highlights)
(require 'yasnippet)


;; -- UI, Editing --
(desktop-save-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
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
(setq auto-dim-other-buffers-mode t)
(custom-set-faces
 '(auto-dim-other-buffers-face ((t (:background "gray2"))))
 '(hl-line ((t (:background "#151515" :underline nil)))))

;; -- Coding --
(define-key global-map (kbd "RET") 'newline-and-indent) ; Auto indent on enter
(setq next-line-add-newlines t)                         ; Add new lines at end of buffer
(setq gdb-many-windows t)
(setq yas/root-directory '("~/.yasnippet-snippets"))
(mapc 'yas/load-directory yas/root-directory)
(yas-global-mode 1)
(global-auto-complete-mode 1)
(setq compilation-scroll-output 'first-error)

;; -- Backups --
(setq backup-directory-alist `(("." . "~/.emacs-bkp"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

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

;; -- Key bindings
(global-set-key [f1] 'toggle-record-macro)
(global-set-key [f2] 'kmacro-call-macro)
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

(key-chord-define-global "qq" 'query-replace)

