(require 'dired-details+)
(setq dired-listing-switches "-al")

;; \( -name "*js" -or -name "*html" -or -name "*styl" \) -not \( -path "*build*" -or -path "*meteor*" -or -path "*npm*" \)
; (add-hook 'dired-load-hook
;           (lambda ()
;             (load "dired-x")
;             ;; Set dired-x global variables here.  For example:
;             ;; (setq dired-guess-shell-gnutar "gtar")
;             ;; (setq dired-x-hands-off-my-keys nil)
;             ))
; (add-hook 'dired-mode-hook
;           (lambda ()
;             ;; Set dired-x buffer-local variables here.  For example:
;             (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^.*\\.meteor/.*$")
;             (dired-omit-mode 1)
;             ))
