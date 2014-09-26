;;; Tab management

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; If there is a tab, make it the size of 4 spaces
(setq-default tab-width 4)

;; Mode specific indent sizes
;; TODO: Consider putting these in their own mode specific inits
(setq c-basic-offset 8)
(setq css-indent-offset 2)
(setq sh-basic-offset 8)
(set-default 'javascript-indent-level 8)
