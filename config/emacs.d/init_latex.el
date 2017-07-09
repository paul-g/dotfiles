;; Configurations for latex mode
(defun build-latex ()
  (interactive)
  (TeX-command "LatexMk" 'TeX-master-file -1))

(use-package tex
  :defer t
  :ensure auctex
  :config (progn
            (setq
             TeX-PDF-mode t
             TeX-source-correlate-mode t
             TeX-source-correlate-method 'synctex
             TeX-command-default "LatexMK"
             TeX-view-program-selection '((output-pdf "Zathura")))
            (evil-leader/set-key-for-mode 'latex-mode
              "pc" 'build-latex
              "pv" 'TeX-view)))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-fold-mode 1)
            (add-hook 'find-file-hook 'TeX-fold-buffer t t)))

(use-package auctex-latexmk
  :config (progn (auctex-latexmk-setup)
                 (setq auctex-latexmk-inherit-TeX-PDF-mode t)))
