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


; EL-Get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-user-package-directory (concat user-emacs-directory "pkginit"))

(load (concat user-emacs-directory "el-get-sources.el"))

(setq my:el-get-packages
      '(el-get
        codepad
        ess
        magit
        smex
        gist
        org-mode
        js2-mode
        mark-multiple
        unbound
        markdown-mode
        ace-jump-mode
        ; temporary
        s
        cl-lib))

(when (el-get-executable-find "latex")
  (add-to-list 'my:el-get-packages 'auctex)
  (add-to-list 'my:el-get-packages 'reftex))

(when (el-get-executable-find "gnuplot")
  (add-to-list 'my:el-get-packages 'gnuplot-mode))

(when (el-get-executable-find "ghc")
  (add-to-list 'my:el-get-packages 'haskell-mode))

(el-get 'sync my:el-get-packages)

; ELPA
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar preload-packages '())
(setq preload-packages
      '(
        ;auctex
        ;ess
        ;magit
        ;mark-more-like-this ;in MELPA this is part of mark-multiple
        ;mark-multiple
        ;gh
        ;gist
        ;smex
        ;js2-mode
        ;unbound
        ;markdown-mode
        ;ace-jump-mode
        ;haskell-mode
        ;cl-lib
        ;s
        projectile
        sauron
        notify
        alert
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
 '(custom-safe-themes (quote ("485737acc3bedc0318a567f1c0f5e7ed2dfde3fb" default)))
 '(safe-local-variable-values (quote ((require-final-newline . t) (mangle-whitespace . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-coinductive-constructor-face ((t (:foreground "#fcaf3e"))) t)
 '(agda2-highlight-datatype-face ((t (:foreground "#729fcf"))) t)
 '(agda2-highlight-error-face ((t (:foreground "#cc0000" :underline t))) t)
 '(agda2-highlight-function-face ((t (:foreground "#729fcf"))) t)
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "#8ae234"))) t)
 '(agda2-highlight-keyword-face ((t (:foreground "#edd400"))) t)
 '(agda2-highlight-module-face ((t (:foreground "#ad7fa8"))) t)
 '(agda2-highlight-number-face ((t (:foreground "#ad7fa8"))) t)
 '(agda2-highlight-postulate-face ((t (:foreground "#729fcf"))) t)
 '(agda2-highlight-primitive-face ((t (:foreground "#729fcf"))) t)
 '(agda2-highlight-primitive-type-face ((t (:foreground "#729fcf"))) t)
 '(agda2-highlight-record-face ((t (:foreground "#729fcf"))) t)
 '(agda2-highlight-string-face ((t (:foreground "#ef2929"))) t))

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

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

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

; EPA/EPG (EasyPG Assistant for GnuGPG)
(require 'epa)
(require 'epa-file)
(epa-file-enable)
(epa-global-mail-mode 1) ; not using emacs mail now, but I AM PREPARED

; ispell / flyspell
(require 'flyspell)
;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(global-set-key (kbd "C-c f") 'flyspell-correct-word-before-point)
(global-set-key (kbd "M-p") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "M-n") 'flyspell-goto-next-error)

; Nix mode
(require 'nix-mode)

; Projectile
(projectile-global-mode)

; Weechat
(add-to-list 'load-path (concat user-emacs-directory "weechat.el"))
(require 'weechat)

(setq weechat-host-default "localhost")
(setq weechat-port-default 9000)

(setq weechat-auto-monitor-buffers
      '("euirc.#javacore"
        "euirc.#c++"
        "freenode.#haskell"
        "freenode.#haskell-lens"
        "freenode.#haskell-blah"
        "freenode.#startups"
        "freenode.#nixos"))

(setq weechat-notification-mode t)
(setq weechat-notification-handler 'weechat-sauron-handler)

(global-set-key (kbd "C-c C-b") 'weechat-switch-buffer)
;(set-face-background 'weechat-highlight-face "dark blue")

; Auth source
(require 'auth-source)
(add-to-list 'auth-sources (concat user-emacs-directory "authinfo.gpg"))

; Alert.el
(require 'alert)

; Sauron
(require 'sauron)

(setq sauron-separate-frame nil)
(global-set-key (kbd "C-c s") 'sauron-toggle-hide-show)
(add-hook 'sauron-mode-hook
          (lambda ()
            (local-set-key (kbd "q") 'sauron-toggle-hide-show)))

(add-hook 'sauron-event-added-functions
          (lambda
            (origin prio msg &optional props)
            (let
                ((sender (plist-get props :sender))
              (sauron-fx-notify (format "%s: %s" origin sender) msg 5000)))))
(add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)

(sauron-start-hidden)

(eval-after-load 'org
  `(progn
     (defun my-sauron-org-mobile-pull ()
       (let ((new-items (with-temp-buffer
                          (insert-file-contents org-mobile-inbox-for-pull)
                          (org-map-entries (lambda () (org-get-heading))))))
         (when (> (length new-items) 0)
           (sauron-add-event
            'org
            3
            (format "%i new item(s) in org-mobile inbox" (length new-items))
            (lambda ()
              (switch-to-buffer (or (find-buffer-visiting org-mobile-inbox-for-pull)
                                    (find-file-noselect org-mobile-inbox-for-pull))))))))
     (add-hook 'org-mobile-post-pull-hook 'my-sauron-org-mobile-pull)))
