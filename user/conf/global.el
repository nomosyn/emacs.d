;; clean text
(set-default 'fill-column 80)
(add-hook 'lisp-mode-hook 'turn-on-auto-fill)
(show-paren-mode t)
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)
(defvar whitespace-cleanup-on-save t)
(add-hook 'before-save-hook
      (lambda ()
        (if whitespace-cleanup-on-save (whitespace-cleanup))))
(setq transient-mark-mode t)
(pending-delete-mode t)
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))
(column-number-mode)



;; improve GC
(setq gc-cons-threshold 20000000)



;; frame size
(add-to-list 'default-frame-alist '(width  . 184))
(add-to-list 'default-frame-alist '(height . 52))



;; clean up frame
(tool-bar-mode 0)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(scroll-bar-mode -1)
(set-default 'indicate-empty-lines nil)
(set-fringe-mode '(1 . 1))



;; macbook keys
(setq ns-command-modifier 'meta)
(setq ns-option-modifier  'super)
(setq ns-right-option-modifier  'nil)



(setq visible-bell t)
(setq backup-directory-alist (list (cons "." user-backups-path)))
(setq delete-by-moving-to-trash t)
(server-start)
(global-auto-revert-mode)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-strip-common-suffix nil)
(require 'misc)
