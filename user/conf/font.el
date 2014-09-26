;;; Fonts

(when (eq system-type 'darwin)
  ;; default Latin font (e.g. Consolas)
  ;; default font size (point * 10)
  (set-face-attribute 'default nil :family "monaco" :height 110 :weight 'normal))
