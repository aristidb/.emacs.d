;;; Init.el
;;; Author: Aristid Breitkreuz <aristidb@googlemail.com>

; Personal info
(setq user-full-name "Aristid Breitkreuz")
(setq user-mail-address "aristidb@googlemail.com")

; UTF-8
(prefer-coding-system 'utf-8)

; Global load paths
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "config/"))

; Load utility functions
(load (concat user-emacs-directory "elisp-utils/elisp-utils.el"))

; Keys
(when (eq system-type 'darwin)
   (setq ns-command-modifier 'meta)
   (setq ns-alternate-modifier 'none))

; Add some paths for executables
(add-exec-paths '("~/.cabal/bin"))
(when (eq system-type 'darwin)
  (add-exec-paths '("/usr/local/bin")))

; Theme (not for Emacs 23)
(when (> emacs-major-version 23)
  (add-to-list 'load-path (concat user-emacs-directory "solarized-emacs/"))
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
  (load-theme 'solarized-dark t))

; Section for Custom. Emacs takes care of this. ;-)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("485737acc3bedc0318a567f1c0f5e7ed2dfde3fb" default)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

; ido
(require 'ido)
(ido-mode t)

; SMEX (= ido for M-x)
(add-to-list 'load-path (concat user-emacs-directory "smex"))
(require 'smex)
(add-hook 'after-init-hook 'smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(smex-initialize) ; only needed when you load this file with M-x eval-buffer

; TRAMP (some of it from http://tsdh.wordpress.com/2008/08/20/re-open-read-only-files-as-root-automagically/)
(require 'tramp) ; total requirement :-)

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

(setq org-refile-targets '((nil . (:maxlevel . 3)) (org-agenda-files . (:maxlevel . 1))))
(setq org-refile-use-outline-path 'file)

(setq org-mobile-directory (expand-file-name "~/Dropbox/MobileOrg"))
(setq org-mobile-inbox-for-pull (concat org-directory "mobile.org"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (perl . t)
   (ditaa . t)
   (gnuplot . t)
   ))

; ESS
(add-to-list 'load-path (concat user-emacs-directory "ess-5.14/lisp"))
(require 'ess-site)

; Haskell
(load (concat user-emacs-directory "haskell-mode-2.8.0/haskell-site-file"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(require 'haskell-align-imports)
;(define-key 'haskell-mode-map (kbd "C-c .") 'haskell-align-imports)

; Magit
(add-to-list 'load-path (concat user-emacs-directory "magit"))
(require 'magit)
; Allow git config to demand magit extensions.
(add-hook 'magit-mode-hook 'magit-load-config-extensions)
(global-set-key (kbd "C-x g") 'magit-status)

; Gist
(add-to-list 'load-path (concat user-emacs-directory "gist.el/"))
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
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
