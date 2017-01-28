;; --- Org mode configuration
(use-package org
  :ensure org-plus-contrib)

;; Epic citation support 
(use-package org-ref
  :ensure t)

;; Display headings as fancy bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Custom format for LNCS documents which requires llncs.cls, to be in
;; the LaTeX compiler's path; for example in the same directory as the
;; .org file
;; http://www.springer.com/gb/computer-science/lncs/conference-proceedings-guidelines 
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

;; By default the report class assigns the top heading to \part, which
;; is a bit odd (?)
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-report"
                 "\\documentclass{report}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}"))))

;; \hypersetup seems to break LLNCS and other styles
(with-eval-after-load 'ox-latex
  (setq org-latex-with-hyperref nil))
