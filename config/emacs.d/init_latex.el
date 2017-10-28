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
              "pv" 'TeX-view
              "prt" 'reftex-toc
              "prc" 'reftex-citation
              "prp" 'reftex-parse-all
              "prv" 'reftex-view-crossref
              "prC" 'reftex-cleveref-Cref
              "pt" 'reftex-toc
              )))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-fold-mode 1)
            (add-hook 'find-file-hook 'TeX-fold-buffer t t)))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (flyspell-mode 1)
            (flyspell-buffer)))

(use-package auctex-latexmk
  :config (progn (auctex-latexmk-setup)
                 (setq auctex-latexmk-inherit-TeX-PDF-mode t)))
(use-package helm-bibtex
  :config (progn 
           (setq bibtex-completion-bibliography local-bibtex-completion-bibliography)))
