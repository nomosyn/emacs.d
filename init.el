(add-hook 'after-init-hook (lambda ()
                             (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                                                      ("org-mode" . "http://orgmode.org/elpa/")
                                                      ("melpa" . "http://melpa.org/packages/")))
                             (prefer-coding-system 'utf-8)
                             (set-default-coding-systems 'utf-8)
                             (set-terminal-coding-system 'utf-8)
                             (set-keyboard-coding-system 'utf-8)
                             ;; backwards compatibility as default-buffer-file-coding-system
                             ;; is deprecated in 23.2.
                             (if (boundp 'buffer-file-coding-system)
                                 (setq-default buffer-file-coding-system 'utf-8)
                               (setq default-buffer-file-coding-system 'utf-8))
                             
                             ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
                             (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
                             (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 110 :weight 'normal)
                             (load-theme 'solarized-dark t)
                             ;; Also works for `solarized-light', `solarized-dark',
                             ;; `sanityinc-solarized-light' and `sanityinc-solarized-dark'.
                             (custom-theme-set-faces
                              'solarized-dark
                              '(context-coloring-level-0-face  ((t :foreground "#839496")))
                              '(context-coloring-level-1-face  ((t :foreground "#268bd2")))
                              '(context-coloring-level-2-face  ((t :foreground "#2aa198")))
                              '(context-coloring-level-3-face  ((t :foreground "#859900")))
                              '(context-coloring-level-4-face  ((t :foreground "#b58900")))
                              '(context-coloring-level-5-face  ((t :foreground "#cb4b16")))
                              '(context-coloring-level-6-face  ((t :foreground "#dc322f")))
                              '(context-coloring-level-7-face  ((t :foreground "#d33682")))
                              '(context-coloring-level-8-face  ((t :foreground "#6c71c4")))
                              '(context-coloring-level-9-face  ((t :foreground "#69b7f0")))
                              '(context-coloring-level-10-face ((t :foreground "#69cabf")))
                              '(context-coloring-level-11-face ((t :foreground "#b4c342")))
                              '(context-coloring-level-12-face ((t :foreground "#deb542")))
                              '(context-coloring-level-13-face ((t :foreground "#f2804f")))
                              '(context-coloring-level-14-face ((t :foreground "#ff6e64")))
                              '(context-coloring-level-15-face ((t :foreground "#f771ac")))
                              '(context-coloring-level-16-face ((t :foreground "#9ea0e5"))))
                             (setq ns-command-modifier 'meta)
                             (setq ns-option-modifier  'super)
                             (setq ns-right-option-modifier  'nil)
                             (when (fboundp 'tool-bar-mode)
                               (tool-bar-mode 0))
                             (when (boundp 'mouse-wheel-scroll-amount)
                               (setq mouse-wheel-scroll-amount '(0.01)))
                             ;; select semantic groups of characters (word, sentence, quotes, block, ...)
                             (require 'expand-region)
                             (global-set-key (kbd "C-=") 'er/expand-region)
                             
                             
                             ;; Mark a place in a buffer.
                             (global-set-key (kbd "C--") 'push-mark-no-activate)
                             
                             ;; Go back to last mark in buffer.
                             (global-set-key (kbd "M--") 'jump-to-mark)
                             
                             ;; indent working buffer.
                             (global-set-key (kbd "<f8>") 'iwb)
                             
                             
                             
                             ;; log work into history.
                             (global-set-key (kbd "<f9>") 'magit-status)
                             
                             
                             
                             ;; switch windows.
                             (global-set-key (kbd "C-o") 'other-window)
                             
                             
                             
                             ;; switch frame.
                             (global-set-key (kbd "C-i") 'other-frame)
                             
                             
                             
                             ;; search string :
                             ;;;; in buffer
                             (global-set-key (kbd "C-s") 'isearch-forward)
                             (global-set-key (kbd "C-r") 'isearch-backward)
                             
                             ;;;; in this directory
                             (global-set-key (kbd "C-^") 'helm-ag)
                             
                             ;;;; in the current project
                             (global-set-key (kbd "C-)") 'helm-projectile-ag)
                             
                             
                             ;; search files/buffer among most probable candidates.
                             (global-set-key (kbd "C-:") 'helm-projectile-find-file)
                             (global-set-key (kbd "C-ù") 'helm-recentf)
                             
                             
                             
                             ;; narrow two regions of the same buffer in two windows.
                             (global-set-key (kbd "C-$") 'clone-indirect-buffer-other-window)
                             
                             
                             ;; move current line:
                             ;;;; up
                             (global-set-key [(meta shift up)]  'move-line-up)
                             
                             ;;;; down
                             (global-set-key [(meta shift down)]  'move-line-down)
                             
                             
                             ;; Capture
                             (global-set-key (kbd "C-c c") 'org-capture)
                             
                             
                             ;; Multi cursors
                             (global-set-key (kbd "C-c m c") 'mc/edit-lines)
                             
                             ;; Move forward one element
                             (global-set-key (kbd "M-n") 'org-forward-element)
                             
                             ;; Move forward one element
                             (global-set-key (kbd "M-p") 'org-backward-element)
                             
                             ;; backward one character
                             (global-set-key (kbd "C-b") 'backward-char)
                             
                             ;; js2-refactor prefix
                             (js2r-add-keybindings-with-prefix "C-c RET")
                             (defun flatten (list-of-lists?)
                             
                               ;; Verify argument type: list-of-lists? : List(List)
                               (let ((err-message "error: arg should be a list of lists"))
                                 (if (listp list-of-lists?)
                                     (dolist (list? list-of-lists?)
                                       (when (not (listp list?)) (error err-message)))
                                   (error err-message)))
                             
                               ;; List(List) -> List
                               (let ((rev-res-list)
                                     (res-list))
                                 (dolist (list list-of-lists? rev-res-list)
                                   (dolist (list-elem list)
                                     (setq rev-res-list (cons list-elem rev-res-list))))
                                 (dolist (elem rev-res-list res-list)
                                   (setq res-list (cons elem res-list)))))
                             
                             
                             
                             (defun iwb ()
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
                               "To prevent modifying a window buffer, make the selected window dedicated to its buffer."
                               (interactive)
                               (set-window-dedicated-p (selected-window) (not current-prefix-arg)))
                             
                             
                             (defun move-line-up ()
                               "Move up the current line."
                               (interactive)
                               (transpose-lines 1)
                               (forward-line -2)
                               (indent-according-to-mode))
                             
                             (defun move-line-down ()
                               "Move down the current line."
                               (interactive)
                               (forward-line 1)
                               (transpose-lines 1)
                               (forward-line -1)
                               (indent-according-to-mode))
                             
                             
                             (defun push-mark-no-activate ()
                               "Pushes `point' to `mark-ring' and does not activate the region
                                     Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
                               (interactive)
                               (push-mark (point) t nil)
                               (message "Pushed mark to ring"))
                             
                             (defun jump-to-mark ()
                               "Jumps to the local mark, respecting the `mark-ring' order.
                               This is the same as using \\[set-mark-command] with the prefix argument."
                               (interactive)
                               (set-mark-command 1))
                             
                             (defconst user/home-dir (file-name-as-directory (expand-file-name "~")))
                             (defconst user/documents-dir (concat user/home-dir (file-name-as-directory "Documents")))
                             (defconst user/emacs-dir (concat user/home-dir (file-name-as-directory ".emacs.d")))
                             (defconst user/emacs-conf-org (concat user/emacs-dir "README.org"))
                             (defconst user/nnotes-dir (concat user/documents-dir (file-name-as-directory "nnotes")))
                             (defconst user/backups-dir (concat user/emacs-dir (file-name-as-directory "backups")))
                             (defconst user/snippets-dir (concat user/emacs-dir (file-name-as-directory "snippets")))
                             (defconst user/nnotes-documents-dir (concat user/nnotes-dir (file-name-as-directory "nnotes-documents")))
                             (defconst user/elpa-dir (concat user/emacs-dir (file-name-as-directory "elpa")))
                             (defconst user/org-dir (concat user/documents-dir (file-name-as-directory "org")))
                             (defconst user/local-bin-dir "/usr/local/bin/")
                             (defconst user/tasks-file (concat user/documents-dir "tasks.org"))
                             (defconst user/libs (concat user/emacs-dir (file-name-as-directory "libs")))
                             (defconst user/org2asciidoc (concat user/libs (file-name-as-directory "org-asciidoc")))
                             
                             (add-to-list 'load-path user/libs)
                             (add-to-list 'load-path user/org2asciidoc)
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
                             (column-number-mode)
                             (setq gc-cons-threshold 20000000)
                             (tool-bar-mode 0)
                             (setq inhibit-startup-message t)
                             (fset 'yes-or-no-p 'y-or-n-p)
                             (scroll-bar-mode -1)
                             (set-default 'indicate-empty-lines nil)
                             (set-fringe-mode 15)
                             (setq visible-bell t)
                             (setq backup-directory-alist (list (cons "." user/backups-dir)))
                             (setq delete-by-moving-to-trash t)
                             (server-start)
                             (global-auto-revert-mode)
                             (require 'uniquify)
                             (setq uniquify-buffer-name-style 'post-forward)
                             (setq uniquify-strip-common-suffix nil)
                             (require 'misc)
                             (setq exec-path (cons user/local-bin-dir exec-path))
                             (setenv "PATH" (concat user/local-bin-dir ":" (getenv "PATH")))
                             (setq-default indent-tabs-mode nil)
                             (setq-default tab-width 4)
                             (put 'upcase-region 'disabled nil)
                             (put 'downcase-region 'disabled nil)
                             (put 'set-goal-column 'disabled nil)
                             (put 'narrow-to-region 'disabled nil)
                             
                             ;; (rainbow-mode)
                             ;; (rainbow-identifiers-mode)
                             ;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
                             (rainbow-delimiters-mode)
                             (rainbow-blocks-mode)
                             (electric-pair-mode)
                             (setq projectile-indexing-method 'alien)
                             (setq ag-highlight-search t)
                             (setq projectile-completion-system 'helm)
                             (projectile-global-mode)
                             (require 'helm-projectile)
                             (helm-projectile-on)
                             (setq-default ispell-program-name "aspell")
                             (setq ispell-list-command "list")
                             (setq ispell-extra-args '("--sug-mode=ultra"))
                             (require 'recentf)
                             (recentf-mode 1)
                             (setq recentf-max-menu-items 100)
                             
                             (global-company-mode)
                             
                             (require 'helm-config)
                             (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
                                   helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
                                   helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
                                   helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
                                   helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
                                   helm-ff-file-name-history-use-recentf t)
                             (helm-mode 1)
                             
                             (require 'yasnippet)
                             (setq yas-snippet-dirs user/snippets-dir)
                             
                             (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                                                      try-expand-dabbrev-all-buffers
                                                                      try-expand-dabbrev-from-kill
                                                                      try-complete-file-name-partially
                                                                      try-complete-file-name
                                                                      try-expand-all-abbrevs
                                                                      try-expand-list
                                                                      try-expand-line
                                                                      try-complete-lisp-symbol-partially
                                                                      try-complete-lisp-symbol))
                             (yas-global-mode 1)
                             
                             
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
                             (setq org-list-indent-offset 2)
                             (setq org-modules '(org-bbdb org-bibtex org-docview org-gnus org-habit org-id org-info org-irc org-mhe org-rmail org-w3m))
                             
                             
                             (defun org-shortcuts ()
                               (local-set-key (kbd "C-<up>") 'org-move-subtree-up)
                               (local-set-key (kbd "C-<down>") 'org-move-subtree-down)
                               (local-set-key (kbd "C-c i") 'org-clock-in)
                               (local-set-key (kbd "C-c o") 'org-clock-out)
                               (local-set-key (kbd "C-c t") 'org-todo)
                               (local-set-key (kbd "C-c r") 'org-clock-report)
                               (local-set-key (kbd "C-c .") 'org-time-stamp))
                             
                             
                             (add-hook 'org-mode-hook 'org-shortcuts)
                             (add-hook 'org-agenda-mode-hook
                                       (lambda ()
                                         (local-set-key (kbd "<tab>") 'org-agenda-goto)))
                             
                             
                             (setq org-todo-keywords '("TODO(t!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)"))
                             (setq org-todo-keyword-faces
                                   '(("TODO" :foreground "red" :weight bold)
                                     ("WAIT" :foreground "orange" :weight bold)
                                     ("DONE" :foreground "forest green" :weight bold)
                                     ("CANCELLED" :foreground "white" :weight bold)))
                             (setq org-enforce-todo-dependencies t)
                             
                             
                             (setq org-log-into-drawer t)
                             (setq org-clock-into-drawer t)
                             
                             
                             (setq org-tag-faces '(("ph" :foreground "cyan" :weight bold)
                                                   ("ad" :foreground "cyan" :weight bold)
                                                   ("bf" :foreground "cyan" :weight bold)
                                                   ("dev" :foreground "cyan" :weight bold)
                                                   ("doc" :foreground "cyan" :weight bold)
                                                   ("com" :foreground "cyan" :weight bold)))
                             
                             
                             
                             ;; Mobile
                             ;; (setq org-mobile-directory user/data-org-mobile-path)
                             ;; (setq org-mobile-inbox-for-pull user/org-mobile-inbox-for-pull-path)
                             
                             
                             
                             ;; Push todo.org when saved
                             ;; (add-hook 'after-save-hook
                             ;;           (lambda ()
                             ;;             (if (string= buffer-file-name user/todo-file)
                             ;;                 (org-mobile-push))))
                             
                             
                             
                             (setq org-agenda-files (list user/tasks-file))
                             
                             
                             (setq org-agenda-span 'month)
                             (setq org-deadline-warning-days 1)
                             (setq org-agenda-skip-scheduled-if-done t)
                             (setq org-log-done t)
                             
                             
                             (defun user/before-finalize-capture-hooks ()
                               (org-id-get-create))
                             (add-hook 'org-capture-before-finalize-hook 'user/before-finalize-capture-hooks)
                             
                             (setq org-capture-templates
                                   '(("p"
                                      "personal"
                                      entry
                                      (file+headline user/tasks-file "tasks")
                                      "* TODO \n:PROPERTIES:\n:END:\n %t\nSCHEDULED: %t\n DEADLINE: %t" :prepend t :clock-in t :clock-resume t)))
                             
                             
                             
                             
                             (setq org-src-fontify-natively t)
                             (org-babel-do-load-languages
                              'org-babel-load-languages
                              '((emacs-lisp . t)
                                (org . t)
                                (dot . t)
                                (latex . t)
                                (ditaa . t)
                                (js . t)))
                             (setq org-src-lang-modes '(("ocaml" . tuareg)
                                                        ("elisp" . emacs-lisp)
                                                        ("ditaa" . artist)
                                                        ("asymptote" . asy)
                                                        ("dot" . graphviz-dot)
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
                             
                             
                             (setq org-clock-clocktable-default-properties '(:maxlevel 3 :scope file))
                             (setq org-clock-persist 'history)
                             (org-clock-persistence-insinuate)
                             
                             
                             (setq org-enable-table-editor t)
                             (require 'sws-mode)
                             (require 'stylus-mode)
                             (require 'sws-mode)
                             (require 'stylus-mode)
                             (setq magit-diff-use-overlays nil)
                             (require 'handlebars-sgml-mode)
                             (handlebars-use-mode 'global)
                             (setq sgml-basic-offset 4)
                             (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
                             (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
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
                             (add-hook 'js2-mode-hook #'js2-refactor-mode)
                             
                             
                             (require 'flycheck)
                             (add-hook 'js2-mode-hook (lambda () (flycheck-mode t)))
                             
                             (setq flycheck-eslintrc ".eslintrc")
                             
                             (setq flycheck-checkers '(ada-gnat asciidoc c/c++-clang
                             c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee
                             coffee-coffeelint coq css-csslint d-dmd emacs-lisp
                             emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt
                             go-golint go-vet go-build go-test go-errcheck haml handlebars
                             haskell-ghc haskell-hlint html-tidy jade javascript-eslint
                             json-jsonlint less luacheck lua perl perl-perlcritic php
                             php-phpmd php-phpcs puppet-parser puppet-lint python-flake8
                             python-pylint python-pycompile r-lintr racket rpm-rpmlint rst
                             rst-sphinx ruby-rubocop ruby-rubylint ruby ruby-jruby rust sass
                             scala scala-scalastyle scss-lint scss sh-bash sh-posix-dash
                             sh-posix-bash sh-zsh sh-shellcheck slim sql-sqlint tex-chktex
                             tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint
                             yaml-jsyaml yaml-ruby))
                             
                             
                             (defun prettify-js-symbols ()
                               (push '("lambda" . ?λ) prettify-symbols-alist)
                               (push '("function" . ?ƒ) prettify-symbols-alist)
                               (push '("return" . ?⟼) prettify-symbols-alist)
                               (push '("<=" . ?≤) prettify-symbols-alist)
                               (push '(">=" . ?≥) prettify-symbols-alist)
                               (push '("!==" . ?≠) prettify-symbols-alist)
                               (prettify-symbols-mode))
                             
                             (add-hook 'js2-mode-hook 'prettify-js-symbols)
                             
                             (require 'context-coloring)
                             (add-hook 'js2-mode-hook 'context-coloring-mode)
                             
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
                             (add-hook 'emacs-lisp-mode-hook (lambda ()
                                                               (rainbow-mode)
                                                               (rainbow-delimiters-mode)
                                                               (smartparens-strict-mode)))
                             ;; (load-file "libs/ProofGeneral-4.3pre150202/ProofGeneral/generic/proof-site.el")
                             ;; (setq proof-prog-name "hoqtop")
                             (setq geiser-racket-binary "/Applications/Racketv6.1.1/bin/racket")
                             (require 'epa-file)
                             (epa-file-enable)
                             (setq epa-file-select-keys nil)
                             (defun should-update-metadata (abs-file-name)
                               (or (string-match-p ".*solutions/.*\.org$" absolute-cur-file-name)
                                   (string-match-p ".*problems/.*\.org$" absolute-cur-file-name)
                                   (string-match-p ".*Documents/notes/.*\.org$" absolute-cur-file-name)
                                   (string-match-p ".*problem\.org$" absolute-cur-file-name)
                                   (string-match-p ".*solution\.org$" absolute-cur-file-name)))
                             
                             
                             (add-hook 'after-save-hook
                                       (lambda ()
                                         (let ((cur-file-name "")
                                               (absolute-cur-file-name))
                                           (setq absolute-cur-file-name (buffer-file-name))
                                           (setq cur-file-name (file-name-nondirectory absolute-cur-file-name))
                                           (cond
                             
                                            ((should-update-metadata absolute-cur-file-name)
                                             (shell-command (concat "pbsol-meta-update.js " absolute-cur-file-name)))
                             
                                            ((string= buffer-file-name user/emacs-conf-org)
                                             (org-babel-tangle))
                             
                                            )
                                           )
                                         )
                                       )
                             ))
