(add-to-list 'load-path user-data-org-mode-path)
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
(setq org-mobile-directory user-data-org-mobile-path)
(setq org-mobile-inbox-for-pull user-org-mobile-inbox-for-pull-path)



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



;; Latex
(setq org-format-latex-options '(:foreground default :background default :scale 1.25 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
             ("begin" "$1" "$" "$$" "\\(" "\\[")))


(defun org-clock-time% (total &rest strings)
  "Compute a time fraction in percent.
TOTAL is a time string like '1d 10:21' specifying the total times.
STRINGS is a list of strings that should be checked for a time.
The first string that does have a time will be used.
This function is made for clock tables."

  (let (str-to-min
        (tot 0)
        s)

    ;; "([0-9]+d)? [0-9]+:[0-9]+" -> minutes
    (setq str-to-min
          (lambda (clock)
            (let ((re "\\(\\([0-9]+\\)d[[:space:]]\\)?\\([0-9]+\\):\\([0-9]+\\)"))
              (save-match-data
                (catch 'exit
                  (if (not (string-match re clock))
                      ;; ill formatted => 0
                      (throw 'exit 0.)

                    (+ (string-to-number (match-string 4 clock))
                       (* 60 (string-to-number (match-string 3 clock)))
                       (* 60 24 (if (match-string 2 clock)
                                    (string-to-number (match-string 2 clock))
                                  0)))))))))

    ;; compute time fraction in percent
    (catch 'exit
      (setq tot (funcall str-to-min total))
      (if (= tot 0.) (throw 'exit 0.))
      (while (setq s (pop strings))
        (throw 'exit
               (/ (* 100.0 (funcall str-to-min s))
                  tot)))
      0)))
