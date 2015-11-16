;;; editorconfig-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "editorconfig" "editorconfig.el" (22076 52706
;;;;;;  0 0))
;;; Generated autoloads from editorconfig.el

(autoload 'editorconfig-apply "editorconfig" "\


\(fn)" nil nil)

(defvar editorconfig-mode nil "\
Non-nil if Editorconfig mode is enabled.
See the command `editorconfig-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `editorconfig-mode'.")

(custom-autoload 'editorconfig-mode "editorconfig" nil)

(autoload 'editorconfig-mode "editorconfig" "\
Toggle EditorConfig feature.

\(fn &optional ARG)" t nil)

(add-to-list 'auto-mode-alist '("/\\.editorconfig\\'" . conf-unix-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; editorconfig-autoloads.el ends here
