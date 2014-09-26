(require 'compile)

; TODO :
; (setq meteor '("Session" "Template" "..."))
; (setq jquery '("..."))
; (setq framework (append meteor jquery))
; (setq notio '("db2html" "..."))
; (setq project notio)
; (setq jslint-global-variables (append framework project))
(defcustom jslint-global-variables '("Session"
                                     "Template"
                                     "$"
                                     "Meteor"
                                     "window"
                                     "NOTIO")


  "JSLint expects every variables to be defined in the current js file.  But some variables may be defined outside of the current js file and global to every files.  This list gather these global variable names")

(defcustom jslint-errors-compile-filter '("^jslint:\\([_[:alnum:]-/]*.js\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):.*$"
          1 2 3) "The filter that parse errors from jslint.  Fed into compilation-error-regexp-alist")

(defun jslint-add-global-var (jslint-gvar)
  "Add predifined global variable names.  Format : var_1,var_2,..."
  (interactive "M")
  (setq jslint-global-variables (remove-duplicates
                                 (append jslint-global-variables
                                         (split-string jslint-gvar "," t))
                                 :test 'string=))
  (jslint-conf-compilation) ;; update dependencies
  (message (concat "jslint-global-variables : "
                   (mapconcat 'identity jslint-global-variables ", "))))

(defun jslint-rm-global-var (jslint-gvar)
  "Remove predifined global variable names.  Format : var_1,var_2,..."
  (interactive "M")
  (let (tmp-gvar)
    (setq tmp-gvar (split-string jslint-gvar "," t))
    (while tmp-gvar
      (let (cur-val)
        (setq cur-val (car tmp-gvar))
        (setq jslint-global-variables (remove cur-val jslint-global-variables)))
      (setq tmp-gvar (cdr tmp-gvar)))
    (jslint-conf-compilation) ;; update dependencies
    (message (concat "jslint-global-variables : " (mapconcat 'identity jslint-global-variables ", ")))))

;; (usr (emacs (M-x compile))) =>
;; (shell (jslint current_file)) =>
;; (emacs (display compile_buffer))
(defun jslint-conf-compilation ()
  (set (make-local-variable 'compilation-error-regexp-alist)
       jslint-errors-compile-filter)
  (make-local-variable 'compile-command)
  (let ((command "")
        (space " ")
        (com ",")
        (java "/usr/bin/java -jar")
        (jslint4java "~/javascript/jslint4java/jslint4java-2.0.4.jar --todo ")
        (predef-global "--predef")
        (jslint-glb-var-tmp (copy-tree jslint-global-variables)))
    (setq command (concat java space jslint4java))
    (when jslint-glb-var-tmp
        (setq predef-global (concat predef-global space "'" (car jslint-glb-var-tmp)))
         (setq jslint-glb-var-tmp (cdr jslint-glb-var-tmp))
         (while jslint-glb-var-tmp
           (let ((cur-global (car jslint-glb-var-tmp)))
             (setq predef-global (concat predef-global com cur-global))
             (setq jslint-glb-var-tmp (cdr jslint-glb-var-tmp))))
         (setq predef-global (concat predef-global "'"))
         (setq command (concat command space predef-global)))
    (setq compile-command (concat command space buffer-file-name))))

(add-hook 'js-mode-hook 'jslint-conf-compilation)
(add-hook 'js2-mode-hook 'jslint-conf-compilation)
