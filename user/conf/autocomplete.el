;; auto-complete for words in buffers
(require 'auto-complete-config)
;; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories user-tools-ac-dictionnaries)
;; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
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
