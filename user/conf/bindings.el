;;; Global key bindigns
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-p") 'other-frame)
(global-set-key (kbd "s-s") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "<f9>") 'magit-status)
;; (electric-indent-mode t)

;; search file
(global-set-key (kbd "s-f") 'textmate-goto-file)


;; align-regexp
(global-set-key (kbd "s-a") 'align-regexp)


;; Tags
(global-set-key (kbd "M-,") 'pop-tag-mark) ; was tags-loop-continue
(global-set-key (kbd "M-.") 'ido-find-tag)
