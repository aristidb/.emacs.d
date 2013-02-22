; Haskell
;(load (concat user-emacs-directory "haskell-mode-2.8.0/haskell-site-file"))
;(load "haskell-site-file")
(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(require 'haskell-align-imports)
(define-key haskell-mode-map (kbd "C-c .") 'haskell-align-imports)

(setq haskell-program-name "/home/aristid/dotfiles/scripts/haskell-repl.pl")
