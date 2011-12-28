;;; Init.el
;;; Author: Aristid Breitkreuz <aristidb@googlemail.com>

; Section for Custom. Emacs takes care of this. ;-)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

; Start the server for emacsclient.
(server-start)

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

; Org Mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

; ESS
(load (concat user-emacs-directory "ess-5.14/lisp/ess-site.el"))