
;; Configure

(add-hook 'after-init-hook (lambda ()
                             (setq package-archives '(("\"marmalade\"" . "http://marmalade-repo.org/packages/")
                                                      ("\"tromey\"" . "http://tromey.com/elpa/")
                                                      ("gnu" . "http://elpa.gnu.org/packages/")
                                                      ("org-mode" . "http://orgmode.org/elpa/")
                                                      ("melpa" . "http://melpa.milkbox.net/packages/")))
                             (require 'un-define "un-define" t)
                             (set-buffer-file-coding-system 'utf-8 'utf-8-unix)
                             (set-default buffer-file-coding-system 'utf-8-unix)
                             (set-default-coding-systems 'utf-8-unix)
                             (prefer-coding-system 'utf-8-unix)
                             (set-default default-buffer-file-coding-system 'utf-8-unix)
                             (when (eq system-type 'darwin)
                               ;; default Latin font (e.g. Consolas)
                               ;; default font size (point * 10)
                               (set-face-attribute 'default nil :family "monaco" :height 110 :weight 'normal))
                             (load-theme 'solarized-dark t)
                             (setq ns-command-modifier 'meta)
                             (setq ns-option-modifier  'super)
                             (setq ns-right-option-modifier  'nil)
                             (when (fboundp 'tool-bar-mode)
                               (tool-bar-mode 0))
                             
                             ;; Slow down the mouse wheel acceleration
                             (when (boundp 'mouse-wheel-scroll-amount)
                               (setq mouse-wheel-scroll-amount '(0.01)))
                             (global-set-key (kbd "C-o") 'other-window)
                             (global-set-key (kbd "<f9>") 'magit-status)
                             (require 'expand-region)
                             (global-set-key (kbd "C-=") 'er/expand-region)
                             (defun iwb()
                               "Indent Whole Buffer"
                               (interactive)
                               (delete-trailing-whitespace)
                               (indent-region (point-min) (point-max) nil)
                               (untabify (point-min) (point-max)))
                             
                             (defun lorem ()
                               (interactive)
                               (insert "Lorem ipsum dolor sit amet, consectetuer adipiscing
                                   elit. Praesent libero orci, auctor sed, faucibus vestibulum,
                                   gravida vitae, arcu. Nunc posuere. Suspendisse
                                   potenti. Praesent in arcu ac nisl ultricies ultricies. Fusce
                                   eros. Sed pulvinar vehicula ante. Maecenas urna dolor, egestas
                                   vel, tristique et, porta eu, leo. Curabitur vitae sem eget arcu
                                   laoreet vulputate. Cras orci neque, faucibus et, rhoncus ac,
                                   venenatis ac, magna. Aenean eu lacus. Aliquam luctus facilisis
                                   augue. Nullam fringilla consectetuer sapien. Aenean neque
                                   augue, bibendum a, feugiat id, lobortis vel, nunc. Suspendisse
                                   in nibh quis erat condimentum pretium. Vestibulum tempor odio
                                   et leo. Sed sodales vestibulum justo. Cras convallis
                                   pellentesque augue. In eu magna. In pede turpis, feugiat
                                   pulvinar, sodales eget, bibendum consectetuer,
                                   magna. Pellentesque vitae augue."))
                             
                             (defun dedicate-window ()
                               (interactive)
                               (set-window-dedicated-p (selected-window) (not current-prefix-arg)))
                             
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
                             (defconst user-emacs-directory (build-dir-path user-home-dir ".emacs.d"))
                             (defconst user-nnotes-directory (build-dir-path user-home-dir "nnotes"))
                             (defconst user-backups-path (build-dir-path user-emacs-directory "backups"))
                             (defconst user-snippets-dir-path (build-dir-path user-emacs-directory "snippets"))
                             (defconst user-nnotes-documents-directory (build-dir-path user-nnotes-directory "nnotes-documents"))
                             (defconst user-elpa-path (concat user-emacs-directory (file-name-as-directory "elpa")))
                             (defconst user-org-path (concat user-home (file-name-as-directory "org")))
                             (defconst user-local-bin "/usr/local/bin")
                             (defconst user-nnotes-tasks-path (concat user-nnotes-documents-directory "todo.org"))
                             (defconst user-todo-path (concat user-org-path "me.org"))
                             ;; clean text
                             (setq initial-scratch-message "")
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
                             (setq gc-cons-threshold 20000000)
                             (add-to-list 'default-frame-alist '(width  . 184))
                             (add-to-list 'default-frame-alist '(height . 52))
                             (tool-bar-mode 0)
                             (setq inhibit-startup-message t)
                             (fset 'yes-or-no-p 'y-or-n-p)
                             (scroll-bar-mode -1)
                             (set-default 'indicate-empty-lines nil)
                             (set-fringe-mode '(1 . 1))
                             (setq visible-bell t)
                             (setq backup-directory-alist (list (cons "." user-backups-path)))
                             (setq delete-by-moving-to-trash t)
                             (server-start)
                             (global-auto-revert-mode)
                             (require 'uniquify)
                             (setq uniquify-buffer-name-style 'post-forward)
                             (setq uniquify-strip-common-suffix nil)
                             (require 'misc)
                             (setq exec-path (cons user-local-bin exec-path))
                             (setenv "PATH" (concat user-local-bin ":" (getenv "PATH")))
                             (setq-default indent-tabs-mode nil)
                             (setq-default tab-width 4)
                             (put 'upcase-region 'disabled nil)
                             (put 'downcase-region 'disabled nil)
                             (put 'set-goal-column 'disabled nil)
                             (put 'narrow-to-region 'disabled nil)
                             ;; auto-complete for words in buffers
                             (require 'auto-complete-config)
                             ;; Make sure we can find the dictionaries
                             ;; (add-to-list 'ac-dictionary-directories user-tools-ac-dictionnaries)
                             ;; Use dictionaries by default
                             ;; (setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
                             (global-auto-complete-mode nil)
                             ;; Start auto-completion after 2 characters of a word
                             (setq ac-auto-start 2)
                             ;; case sensitivity is important when finding matches
                             (setq ac-ignore-case nil)
                             
                             
                             
                             ;; auto-complete for finding files in a "project directory"
                             (projectile-global-mode)
                             
                             
                             
                             ;; auto-complete for finding recently visited files
                             (require 'recentf)
                             (recentf-mode 1)
                             (setq recentf-max-menu-items 100)
                             
                             
                             
                             ;; auto-complete for M-x stuff
                             (require 'smex)
                             (smex-initialize)
                             (global-set-key (kbd "M-x") 'smex)
                             (global-set-key (kbd "M-X") 'smex-major-mode-commands)
                             (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
                             
                             
                             
                             ;; auto-complete using fuzzy-matching
                             (require 'ido)
                             (ido-mode 1)
                             (ido-everywhere 1)
                             
                             (require 'ido-vertical-mode)
                             (ido-vertical-mode)
                             
                             ;; forget latest selected directory names
                             (setq ido-enable-last-directory-history nil)
                             
                             ;; disable ido faces to see flx highlights.
                             (setq ido-use-faces nil)
                             
                             (require 'flx-ido)
                             (flx-ido-mode 1)
                             
                             (require 'ido-ubiquitous)
                             (ido-ubiquitous)
                             
                             
                             ;; auto-complete using abbreviations
                             (require 'dropdown-list)
                             (require 'yasnippet)
                             (setq yas-snippet-dirs user-snippets-dir-path)
                             (setq yas-prompt-functions '(yas-ido-prompt
                                                          yas-dropdown-prompt
                                                          yas-completing-prompt))
                             (yas-global-mode 1)
                             
                             
                             
                             ;; auto-complete tags using fuzzy-matching
                             (defun ido-find-tag ()
                               "Find a tag using ido"
                               (interactive)
                               (tags-completion-table)
                               (let (tag-names)
                                 (mapatoms (lambda (x)
                                             (push (prin1-to-string x t) tag-names))
                                           tags-completion-table)
                                 (find-tag (ido-completing-read "Tag: " tag-names))))
                             
                             
                             
                             
                             ;; auto-complete stuff using TAB key
                             (setq hippie-expand-try-functions-list
                                   '(yas-hippie-try-expand
                                     try-expand-dabbrev
                                     try-expand-dabbrev-all-buffers
                                     try-expand-dabbrev-from-kill
                                     try-complete-file-name
                                     try-complete-lisp-symbol))
                             
                             (defvar smart-tab-using-hippie-expand t
                               "turn this on if you want to use hippie-expand completion.")
                             
                             (defun smart-indent ()
                               "Indents region if mark is active, or current line otherwise."
                               (interactive)
                               (if mark-active
                                   (indent-region (region-beginning)
                                                  (region-end))
                                 (indent-for-tab-command)))
                             
                             (defun smart-tab (prefix)
                               "Needs `transient-mark-mode' to be on. This smart tab is
                                       minibuffer compliant: it acts as usual in the minibuffer.
                             
                                       In all other buffers: if PREFIX is \\[universal-argument], calls
                                       `smart-indent'. Else if point is at the end of a symbol,
                                       expands it. Else calls `smart-indent'."
                               (interactive "P")
                               (labels ((smart-tab-must-expand (&optional prefix)
                                                               (unless (or (consp prefix)
                                                                           mark-active)
                                                                 (looking-at "\\_>"))))
                                 (cond ((minibufferp)
                                        (minibuffer-complete))
                                       ((smart-tab-must-expand prefix)
                                        (if smart-tab-using-hippie-expand
                                            (hippie-expand prefix)
                                          (dabbrev-expand prefix)))
                                       ((smart-indent)))))
                             
                             (global-set-key (kbd "TAB") 'smart-tab)
                             (require 'org)
                             (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
                             (global-set-key (kbd "C-c l") 'org-store-link)
                             (global-set-key (kbd "C-c a") 'org-agenda)
                             (global-set-key (kbd "C-c b") 'org-iswitchb)
                             (setq org-hide-leading-stars t)
                             
                             ;; Indentation
                             (setq org-list-indent-offset 2)
                             
                             
                             ;; Bindings
                             (defun org-shortcuts ()
                               (local-set-key (kbd "C-<up>") 'org-move-subtree-up)
                               (local-set-key (kbd "C-<down>") 'org-move-subtree-down)
                               (local-set-key (kbd "C-c i") 'org-clock-in)
                               (local-set-key (kbd "C-c o") 'org-clock-out)
                               (local-set-key (kbd "C-c t") 'org-todo)
                               (local-set-key (kbd "C-c r") 'org-clock-report)
                               (local-set-key (kbd "C-c .") 'org-time-stamp)
                               (auto-complete-mode)
                               (message "org-mode-hook func"))
                             (add-hook 'org-mode-hook 'org-shortcuts)
                             
                             
                             
                             (add-hook 'org-agenda-mode-hook
                                       (lambda ()
                                         (local-set-key (kbd "<tab>") 'org-agenda-goto)))
                             
                             
                             
                             ;; TODOs
                             (setq org-todo-keywords '("TODO(t!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)"))
                             
                             (setq org-todo-keyword-faces
                                   '(("TODO" :foreground "red" :weight bold)
                                     ("WAIT" :foreground "orange" :weight bold)
                                     ("DONE" :foreground "forest green" :weight bold)
                                     ("CANCELLED" :foreground "white" :weight bold)))
                             
                             (setq org-enforce-todo-dependencies t)
                             (setq org-log-into-drawer t)
                             (setq org-clock-into-drawer t)
                             
                             ;; TAGS
                             (setq org-tag-faces '(("ph" :foreground "cyan" :weight bold)
                                                   ("ad" :foreground "cyan" :weight bold)
                                                   ("bf" :foreground "cyan" :weight bold)
                                                   ("dev" :foreground "cyan" :weight bold)
                                                   ("doc" :foreground "cyan" :weight bold)
                                                   ("com" :foreground "cyan" :weight bold)))
                             
                             
                             
                             ;; Mobile
                             ;; (setq org-mobile-directory user-data-org-mobile-path)
                             ;; (setq org-mobile-inbox-for-pull user-org-mobile-inbox-for-pull-path)
                             
                             
                             
                             ;; Push todo.org when saved
                             ;; (add-hook 'after-save-hook
                             ;;           (lambda ()
                             ;;             (if (string= buffer-file-name user-todo-path)
                             ;;                 (org-mobile-push))))
                             
                             
                             ;; Agenda
                             (setq org-agenda-files (list
                                                     user-todo-path
                                                     user-nnotes-tasks-path))
                             
                             
                             (setq org-agenda-span 'month)
                             (setq org-deadline-warning-days 1)
                             (setq org-agenda-skip-scheduled-if-done t)
                             (setq org-log-done t)
                             
                             
                             
                             ;; Capture
                             ;; see global.el
                             (global-set-key (kbd "C-c c") 'org-capture)
                             
                             (defun user-before-finalize-capture-hooks ()
                               (org-id-get-create))
                             (add-hook 'org-capture-before-finalize-hook 'user-before-finalize-capture-hooks)
                             
                             (setq org-capture-templates
                                   '(("p"
                                      "personal"
                                      entry
                                      (file+headline user-todo-path "tasks")
                                      "* TODO \nDEADLINE: %t\n:PROPERTIES:\n:END:" :prepend t :clock-in t :clock-resume t)
                             
                                     ("n"
                                      "nnotes"
                                      entry
                                      (file+headline user-nnotes-tasks-path "tasks")
                                      "* TODO \nDEADLINE: %t\n:PROPERTIES:\n:END:" :prepend t :clock-in t :clock-resume t)))
                             
                             
                             
                             
                             
                             ;; code block
                             ;; allow ditaa block
                             (setq org-src-fontify-natively t)
                             
                             (org-babel-do-load-languages
                              'org-babel-load-languages
                              '((emacs-lisp . t)
                                (org . t)
                                (latex . t)
                                (ditaa . t)
                                (js . t)))
                             
                             (setq org-src-lang-modes '(("ocaml" . tuareg)
                                                        ("elisp" . emacs-lisp)
                                                        ("ditaa" . artist)
                                                        ("asymptote" . asy)
                                                        ("dot" . fundamental)
                                                        ("sqlite" . sql)
                                                        ("calc" . fundamental)
                                                        ("C" . c)
                                                        ("js" . js2)
                                                        ("cpp" . c++)
                                                        ("C++" . c++)
                                                        ("screen" . shell-script)))
                             
                             
                             (defun my-org-confirm-babel-evaluate (lang body)
                               (not (or
                                     (string= lang "org")
                                     (string= lang "ditaa")      ;; don't ask for ditaa
                                     (string= lang "emacs-lisp")))) ;; don't ask for elisp
                             (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
                             
                             
                             
                             ;; clocktable
                             (setq org-clock-clocktable-default-properties '(:maxlevel 3 :scope file))
                             (setq org-clock-persist 'history)
                             (org-clock-persistence-insinuate)
                             
                             
                             
                             ;; org-table
                             (setq org-enable-table-editor t)
                             (require 'sws-mode)
                             (require 'stylus-mode)
                             (require 'handlebars-sgml-mode)
                             (handlebars-use-mode 'global)
                             (setq sgml-basic-offset 4)
                             (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
                             (setq js2-allow-keywords-as-property-names nil)
                             (setq js2-mode-show-strict-warnings nil)
                             (setq js2-basic-offset 4)
                             (setq js2-bounce-indent-p nil)
                             (setq js2-dynamic-idle-timer-adjust 10000)
                             (setq js2-highlight-external-variables nil)
                             (setq js2-idle-timer-delay 1)
                             (setq js2-mode-show-parse-errors t)
                             (setq js2-pretty-multiline-declarations t)
                             (setq js2-highlight-level 3)
                             
                             (require 'js2-refactor)
                             (js2r-add-keybindings-with-prefix "C-c C-m")
                             
                             ;; jshint
                             ;; (require 'flycheck)
                             ;; (add-hook 'js2-mode-hook
                             ;;           (lambda () (flycheck-mode t)))
                             
                             (defun prettify-js-symbols ()
                               (push '("lambda" . ?λ) prettify-symbols-alist)
                               (push '("function" . ?ƒ) prettify-symbols-alist)
                               (push '("return" . ?⟼) prettify-symbols-alist)
                               (push '("<=" . ?≤) prettify-symbols-alist)
                               (push '(">=" . ?≥) prettify-symbols-alist)
                               (push '("!==" . ?≠) prettify-symbols-alist)
                               (prettify-symbols-mode)
                               (electric-pair-mode))
                             
                             (add-hook 'js2-mode-hook 'prettify-js-symbols)
                             (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
                             (require 'tex)
                             (add-hook 'TeX-mode-hook (lambda ()
                                                        (local-set-key (kbd "C-c h") 'TeX-fold-dwim)
                                                        (local-set-key (kbd "C-f") 'LaTeX-fill-region)
                                                        (LaTeX-math-mode)
                                                        ;; (setq TeX-engine 'xetex)
                                                        (turn-on-reftex)))
                             (setq TeX-auto-save t)
                             (setq TeX-parse-self t)
                             (setq-default TeX-master nil)
                             (setq reftex-plug-into-AUCTeX t)
                             (TeX-global-PDF-mode t)
                             (setq LaTeX-indent-level 4)
                             (setq LaTeX-item-indent 0)
                             
                             (add-hook 'after-save-hook
                                       (lambda ()
                                         (let ((cur-file-name ""))
                                           (setq cur-file-name (file-name-nondirectory (buffer-file-name)))
                                           (cond
                                            ((string= cur-file-name "french-tech-programme.tex") (shell-command "./build.sh programme"))
                                            ((string= cur-file-name "french-tech-demandeur.tex") (shell-command "./build.sh demandeur")))
                                           )
                                         )
                                       )))
