;;; quarter-plane-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-quarter-plane-mode quarter-plane-mode)
;;;;;;  "quarter-plane" "quarter-plane.el" (20185 4059))
;;; Generated autoloads from quarter-plane.el

(autoload 'quarter-plane-mode "quarter-plane" "\
Toggle Quarter-Plane mode on or off.
Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG turn mode on.
With zero or negative ARG turn mode off.

Use point movement commands that act as if the text extended
infinitely down and to the right, inserting spaces as necessary.
Excess whitespace is trimmed when saving or exiting Quarter-Plane mode.

Because it works by inserting spaces, Quarter-Plane mode won't work in
read-only buffers.

\\{quarter-plane-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-quarter-plane-mode nil "\
Non-nil if Global-Quarter-Plane mode is enabled.
See the command `global-quarter-plane-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-quarter-plane-mode'.")

(custom-autoload 'global-quarter-plane-mode "quarter-plane" nil)

(autoload 'global-quarter-plane-mode "quarter-plane" "\
Toggle Quarter-Plane mode in every possible buffer.
With prefix ARG, turn Global-Quarter-Plane mode on if and only if
ARG is positive.
Quarter-Plane mode is enabled in all buffers where
`quarter-plane-mode' would do it.
See `quarter-plane-mode' for more information on Quarter-Plane mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("quarter-plane-pkg.el") (20185 4059 71051))

;;;***

(provide 'quarter-plane-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; quarter-plane-autoloads.el ends here
