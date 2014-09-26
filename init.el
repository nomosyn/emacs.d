(setq debug-on-error nil)

(defun fold (f x list)
  "Ex:
     (fold 'concat \"X\" \'(\"a\" \"b\"))
       <- (concat (concat (concat \"X\") \"a\") \"b\")
       <- \"Xab\""
  (let ((li list) (x2 x))
    (while li (setq x2 (funcall f x2 (pop li))))
    x2))

(defun build-dir-path (path &rest fname-list)
  "build-dir-path: PATH NAME1 NAME2 ... -> PATH

  (build-dir-path some-path \"to\" \"this\" \"dir\") -> \"/some/path/to/this/dir\"
"
  (expand-file-name

   (fold (lambda (path fname)
           (concat path (file-name-as-directory fname)))

         ;; Check that path looks like /.../my/path/.../end/
         (if (string= path (file-name-as-directory path))
             path
           (error "first argument should be a directory path ex : /my/dir/"))

         fname-list)))

(defconst user-home (file-name-as-directory (expand-file-name "~")))
(defconst user-home-dir (file-name-as-directory user-home))
(defconst user-nnotes-directory (build-dir-path user-home-dir "nnotes")
  "Defines where nnotes project directory is")
(defconst user-nnotes-documents-directory (build-dir-path user-nnotes-directory "nnotes-documents")
  "Defines where nnotes project directory is")


(defconst user-emacs-directory (build-dir-path user-home-dir ".emacs.d")
  "Defines where is the user emacs directory located

If you want to boot from a emacs.d/ which is on a usb drive then set user-emacs-directory to
/Volumes/usb-drive/emacs.d and start emacs on the command line with
$ emacs -l /Volumes/usb-drive/init.el")


;; user-emacs-directory/*
(defconst user-emacs-init-path (concat user-emacs-directory "init.el"))
(defconst user-backups-path (build-dir-path user-emacs-directory "backups"))
(defconst user-elpa-path (concat user-emacs-directory (file-name-as-directory "elpa")))
(defconst user-path (concat user-emacs-directory (file-name-as-directory "user")))




;; user-emacs-directory/elpa/*
(defconst user-tools-ctags (concat user-elpa-path (file-name-as-directory "ctags-update-0.1.2")))
(defconst user-tools-undo-tree (concat user-elpa-path (file-name-as-directory "undo-tree-0.6.3")))




;; user-emacs-directory/user/*
(defconst user-addons-path (concat user-path (file-name-as-directory "addons")))
(defconst user-conf-path (concat user-path (file-name-as-directory "conf")))
(defconst user-org-path (concat user-home (file-name-as-directory "org")))



;; user-emacs-directory/user/org/*
(defconst user-todo-path (concat user-org-path "me.org")
  "Where all the todos of the user are saved")

(defconst user-nnotes-tasks-path (concat user-nnotes-documents-directory "todo.org")
  "Where all nnotes related tasks are stored")


(defconst user-time-path (concat user-org-path "time.org"))
(defconst user-org-mobile-inbox-for-pull-path (concat user-org-path "mobile-notes.org"))




;; user-emacs-directory/user/conf/*
(defconst user-tools-ac-dictionnaries (concat user-conf-path (file-name-as-directory "auto-complete-dictionnaries")))





;; user-emacs-directory/user/addons/*
(defconst user-snippets-dir-path (concat user-addons-path (file-name-as-directory "snippets")))
(defconst user-data-org-mode-path (concat user-addons-path (file-name-as-directory "org-8.2.7c/lisp")))





;; tools
(defconst user-tools-ctags-bin-path "/opt/local/bin/ctags")





;; data
(defconst user-data-org-mobile-path (expand-file-name "~/Dropbox/Applications/MobileOrg"))
(defconst user-data-ids-path (expand-file-name "~/Documents/Bibliothèque/identitees.org"))
(defconst user-data-org-mobile-files-list (list user-todo-path user-nnotes-tasks-path))
(defconst emacs-color-theme-solarized-path (concat user-addons-path (file-name-as-directory "emacs-color-theme-solarized")))




(defconst configuration-files '("theme"
                                "global"
                                "navigation"
                                "mac"
                                "defun"
                                "env"
                                "bindings"
                                "tab"
                                "disabled"
                                "font"
                                "utf-8"
                                "scratch"
                                "grep"
                                "indent-guide"
                                "org"
                                "elpa"
                                ;; "tags"
                                ;; "python"
                                "autocomplete"
                                "aspell"
                                "undo-tree"
                                "dired"
                                "fill-column-indicator"
                                "stylus"
                                "html"
                                "text"
                                "javascript"
                                "haskell"
                                "latex"
                                ;;"c-mode"
                                ))



(add-hook 'after-init-hook (lambda ()
                             (while configuration-files
                               (load (concat user-conf-path (car configuration-files)))
                               (setq configuration-files (cdr configuration-files)))
                             (load-file (concat user-addons-path "htmlize/htmlize.el"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(background-color "#fcf4dc")
 '(background-mode light)
 '(cursor-color "#52676f")
 '(custom-safe-themes
   (quote
    ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color "#52676f")
 '(org-agenda-files
   (quote
    ("/Users/PH/.emacs.d/user/org/me.org" "/Users/PH/.emacs.d/user/org/lig.org")))
 '(safe-local-variable-values (quote ((TeX-engine . xelatex)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-column ((t (:strike-through nil :underline nil :slant normal :weight normal))))
 '(org-column-title ((t (:underline t :weight bold)))))
