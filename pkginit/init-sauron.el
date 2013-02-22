(require 'sauron)

(setq sauron-separate-frame nil)
(global-set-key (kbd "C-c s") 'sauron-toggle-hide-show)
(add-hook 'sauron-mode-hook
          (lambda ()
            (local-set-key (kbd "q") 'sauron-toggle-hide-show)))

(add-hook 'sauron-event-added-functions
          (lambda
            (origin prio msg &optional props)
            (let*
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
