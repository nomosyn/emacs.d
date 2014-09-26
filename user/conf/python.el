;; PEP8 coding style
(setq python-indent-offset 4)
(add-hook 'python-mode-hook
          (lambda ()
            (setq fill-column 80)))
