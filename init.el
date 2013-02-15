;;; Init.el
;;; Author: Aristid Breitkreuz <aristidb@googlemail.com>

(require 'cl)

; Personal info
(setq user-full-name "Aristid Breitkreuz")
(setq user-mail-address "aristidb@gmail.com")

; Other constants
(defconst console-p (eq (symbol-value 'window-system) nil))

; UTF-8
(prefer-coding-system 'utf-8)

; Global load paths
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "config/"))
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")
(add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp")

; Load utility functions
(load (concat user-emacs-directory "elisp-utils/elisp-utils.el"))

; Keys
;(when (eq system-type 'darwin)
;   (setq ns-command-modifier 'meta)
;   (setq ns-alternate-modifier 'none))

(global-set-key (kbd "C-c s") 'sort-lines)

; Add some paths for executables
(add-exec-paths '("~/.nix-profile/bin" "~/.cabal/bin" "/usr/texbin"))
(when (eq system-type 'darwin)
  (add-exec-paths '("/usr/local/bin")))

; ELPA
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar preload-packages '())
(setq preload-packages
      '(
        auctex
        ess
        magit
        ;mark-more-like-this ;in MELPA this is part of mark-multiple
        mark-multiple
        gh
        gist
        smex
        js2-mode
        unbound
        markdown-mode
        ace-jump-mode
        haskell-mode
        ))

(defun preload-packages-installed-p ()
  (loop for p in preload-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (preload-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (loop for p in preload-packages
        when (not (package-installed-p p)) do (package-install p)))

; Theme (not for Emacs 23)
(add-to-list 'load-path (concat user-emacs-directory "solarized-emacs/"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
(load-theme 'tango-dark t)

(find-and-set-font '("Monaco-10" "DejaVu Sans Mono-9"))

; Section for Custom. Emacs takes care of this. ;-)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("485737acc3bedc0318a567f1c0f5e7ed2dfde3fb" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-coinductive-constructor-face ((t (:foreground "#fcaf3e"))))
 '(agda2-highlight-datatype-face ((t (:foreground "#729fcf"))))
 '(agda2-highlight-error-face ((t (:foreground "#cc0000" :underline t))))
 '(agda2-highlight-function-face ((t (:foreground "#729fcf"))))
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "#8ae234"))))
 '(agda2-highlight-keyword-face ((t (:foreground "#edd400"))))
 '(agda2-highlight-module-face ((t (:foreground "#ad7fa8"))))
 '(agda2-highlight-number-face ((t (:foreground "#ad7fa8"))))
 '(agda2-highlight-postulate-face ((t (:foreground "#729fcf"))))
 '(agda2-highlight-primitive-face ((t (:foreground "#729fcf"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "#729fcf"))))
 '(agda2-highlight-record-face ((t (:foreground "#729fcf"))))
 '(agda2-highlight-string-face ((t (:foreground "#ef2929")))))

; Start the server for emacsclient.
(require 'server)
(unless (server-running-p)
  (server-start))

; Clipboard
(setq x-select-enable-clipboard t)

; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tabs-width 2)

; Buffers menu
(setq-default buffers-menu-max-size 30)

; Font lock
(global-font-lock-mode 1)

; Line / Column numbers
(line-number-mode 1)
(column-number-mode 1)
; Size indication
(size-indication-mode 1)

; Disable toolbar
(tool-bar-mode 0)

; ido
(require 'ido)
(ido-mode t)

; SMEX (= ido for M-x)
(require 'smex)
(add-hook 'after-init-hook 'smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(smex-initialize) ; only needed when you load this file with M-x eval-buffer

; TRAMP (some of it from http://tsdh.wordpress.com/2008/08/20/re-open-read-only-files-as-root-automagically/)
(require 'tramp)

(add-to-list 'tramp-remote-path "/run/current-system/sw/bin") ; for NixOS remotes

;; Sudo on remote systems
(add-to-list 'tramp-default-proxies-alist
                  '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
                  '("localhost" nil nil))

(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

; C++ Mode
(defun aristid-c-mode-common ()
  (interactive)
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                             1 font-lock-warning-face prepend)))
  (c-set-style "user")
  (setq c-basic-offset 2)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'label 0))

(add-hook 'c-mode-common-hook 'aristid-c-mode-common)

; Gnuplot Mode
;; these lines enable the use of gnuplot mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

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

; ESS
(require 'ess-site)

; Haskell
;(load (concat user-emacs-directory "haskell-mode-2.8.0/haskell-site-file"))
;(load "haskell-site-file")
(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(require 'haskell-align-imports)
(define-key haskell-mode-map (kbd "C-c .") 'haskell-align-imports)

; Magit
(require 'magit)
; Allow git config to demand magit extensions.
(add-hook 'magit-mode-hook 'magit-load-config-extensions)
(global-set-key (kbd "C-x g") 'magit-status)

; Gist
(require 'gist)

; Backups
(push (cons "." (concat user-emacs-directory "backups")) backup-directory-alist)

; Better buffer naming
(require 'uniquify)
(setq 
 uniquify-buffer-name-style 'reverse
 uniquify-separator ":")

; Enable "dangerous" functions.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

; Make writing scripts more comfortable.
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

; Calendar format.
(setq display-time-24hr-format t)
(setq calendar-week-start-day 1) ;; Week starts with Monday
(setq calendar-time-display-form '(24-hours ":" minutes
                                            (if time-zone " (")
                                            time-zone
                                            (if time-zone ")")))
; Agda Mode
(condition-case nil
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate")))
  (error (message "Loading agda-mode failed")))
    
(setq agda2-include-dirs (cons "." (mapcar 'expand-file-name '("~/agda/lib/src" "~/agda/lib-0.6/src"))))
(add-to-list 'ido-ignore-files "\\.agdai")

; Twelf
(when (eq system-type 'darwin)
  (setq twelf-root "/Applications/Twelf/")
  (load (concat twelf-root "emacs/twelf-init.el")))

; Mark Multiple / Mark More Like This
(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

; JS2 Mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; Unbound (find unbound shortcuts)
(require 'unbound)

; EPA/EPG (EasyPG Assistant for GnuGPG)
(require 'epa)
(require 'epa-file)
(epa-file-enable)
(epa-global-mail-mode 1) ; not using emacs mail now, but I AM PREPARED

; Ace Jump Mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

; ispell / flyspell
(require 'flyspell)
;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(global-set-key (kbd "C-c f") 'flyspell-correct-word-before-point)
(global-set-key (kbd "M-p") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "M-n") 'flyspell-goto-next-error)

; Nix mode
(require 'nix-mode)
