(require 'auth-source)

(setq weechat-host-default "hermit.breitkreuz.me")
(setq weechat-port-default 9000)
(setq weechat-mode-default "ssh -p 22023 -W localhost:%p %h") 

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

(global-set-key (kbd "C-,") 'weechat-switch-buffer)
(global-set-key (kbd "C-.") 'weechat-monitor-buffer)

(add-hook 'weechat-mode-hook 'visual-line-mode)
