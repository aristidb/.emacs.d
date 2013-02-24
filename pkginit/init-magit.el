; Allow git config to demand magit extensions.
(add-hook 'magit-mode-hook 'magit-load-config-extensions)
(add-hook 'magit-log-edit-mode-hook 'auto-fill-mode)

(global-set-key (kbd "C-x g") 'magit-status)
