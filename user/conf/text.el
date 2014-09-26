(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(defun custom-text-mode-hook ()
  (auto-fill-mode)
  (make-local-variable 'tab-width)
  (setq tab-width 4)
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (my-generate-tab-stops tab-width)))

(add-hook 'text-mode-hook 'custom-text-mode-hook)
