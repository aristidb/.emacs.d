; Org Mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps nil)

(setq org-directory (expand-file-name "~/org/"))
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-agenda-files (concat org-directory "agenda-files"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "notes.org") "Tasks")
         "* TODO %?\n  %i\m  %a")
        ("j" "Journal" entry (file+datetree (concat org-directory "journal.org"))
         "* %?\n  Entered on %U\n  %i\n  %a")))

(setq org-refile-targets '((nil . (:maxlevel . 3)) (org-agenda-files . (:maxlevel . 1))))
(setq org-refile-use-outline-path 'file)

(require 'org-habit)
(setq org-habit-show-habits-only-for-today nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (perl . t)
   (ditaa . t)
   (gnuplot . t)
   ))

(setq org-src-fontify-natively t) ; fontify source code

(setq org-export-latex-listings 'minted)
(setq org-export-latex-custom-lang-environments
      '(
        (emacs-lisp "common-lispcode")
        ))
(setq org-export-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")
        ("linenos" "")))
(setq org-latex-to-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-ditaa-jar-path (concat user-emacs-directory "ditaa0_9.jar"))

;; mobile org
(setq org-mobile-directory (expand-file-name "~/Dropbox/MobileOrg"))
(setq org-mobile-inbox-for-pull (concat org-directory "mobile.org"))
(run-with-timer 0 (* 30 60) 'org-mobile-pull)
