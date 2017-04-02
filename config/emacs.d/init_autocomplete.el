;; Auto complete settings, use company
(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode))

;; --- C++ Mode Completion
;; Use Irony; seems able to pick up flags from a .clang_complete file
;; located at the root of the project directory. First time need to
;; run irony-install-server if not already installed; this requires
;; libclang, which should be available from most package managers
;; (use-package company-irony
;;   :ensure t)
;; (use-package irony
;;   :ensure t
;;   :config (progn
;;             (add-hook 'c++-mode-hook 'irony-mode)
;;             ;; Required to load files from a .clang_complete file as far as I can tell...
;;             (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))
;; (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))

;; One possible alternative for C++ completion is emacs-ycmd; the setup
;; below seems to work and also provides additional features like
;; goto-def, but it is very slow for some reason (?), which is why I m not using it;
;; it also requires an external executable to be built and compiled separately, which is
;; a bit of a pain; irony provides a simple emacs interface to compile the server
;; (use-package company-ycmd
;;   :ensure t
;;   :config (global-company-mode))

;; (use-package ycmd
;;   :ensure t
;;   :config (progn
;;             (company-ycmd-setup)
;;             (set-variable 'ycmd-server-command '("python" "/home/paul-g/workspaces/checkouts/ycmd/ycmd"))
;;             (set-variable 'ycmd-extra-conf-whitelist '("/home/paul-g/workspaces/spam/*"))
;;             (global-ycmd-mode)))
;; (eval-after-load 'company '(add-to-list 'company-backends 'company-ycmd))


;; It is also possible to use the builitin semantic package, but this
;; requires some global configuration using EDE to get semantic to
;; understand all the necessary include files; for lightweight
;; projects it seems easier to just include company irony
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)
;; (require 'ede)
;; (global-ede-mode)
;; (ede-cpp-root-project "project_root"
;;                       :file "/path/to/project/"
;;                       :include-path '("include" "src")
;;                       :system-include-path '("~/linux"))
