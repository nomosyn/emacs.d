;;; -*- lexical-binding: t -*-
;;; company-coq.el --- Company-mode backend for Proof General's coq-mode

;; Copyright (C) 2015  Clément Pit--Claudel
;; Author: Clément Pit--Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/cpitclaudel/company-coq

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package offers a company backend for Proof-General's Coq mode.
;;
;; Features
;; ========
;;
;; * Auto-completion of math symbols using company-math
;;
;; * Auto-completion of theorem names from the same buffer, with type
;;   annotations.
;;
;; * Easy access to Proof-General's templates (using yasnippet)
;;
;; * Auto-completion of (most of) Coq's tactics and commands, with snippets
;;   auto-extracted from the manual.
;;
;; * Auto-completion of module names in [Import] commands.
;;
;; * Auto-completion of identifiers in proof contexts.
;;
;; * Documentation for (most) tactics and commands, with excerpts from the
;;   manual shown directly in Emacs.
;;
;; Advanced features (requires a patched version of `coqtop`)
;; ==========================================================
;;
;; * Auto-completion of all known theorem and symbol names, with type
;;   annotations.
;;
;; See https://github.com/cpitclaudel/company-coq/ for further documentation
;;

;;; Code:
;;
;; Technical notes
;; ===============
;;
;; Sending  commands to  the prover  when it's  already busy  breaks everything.
;; 'proof-shell-handle-delayed-output-hook is  a good  place to  reload symbols,
;; except when an error occurs (in that case the hook runs before all output has
;; been processed.
;;
;; This problem is solved by refusing  to communicate with the prover, unless it
;; is reported as available. When it isn't, the interaction is either abandonned
;; (that's what happens  with documentation, and with completion  if the symbols
;; aren't available yet)) or delayed using  an idle timer (reload; in fact, this
;; one is always wrapped in an idle  timer). To prevent loops due to idle timers
;; firing in succession, reloads are only attempted once.
;;
;; The current implementation uses two hooks:
;;  * 'proof-shell-insert-hook
;;      In this one we check the input to see if it might introduce new symbols
;;      (e.g. [Require ...])
;;  * 'proof-shell-handle-delayed-output-hook
;;      In this one we parse the output to see if it suggests that new symbols
;;      have been introduced (e.g. [... defined])
;;
;; Since these  two hooks are  called into even for  commands issued by  our own
;; code, we only execute their body if  we are not currently already waiting for
;; an answer from the prover (company-coq-asking-question).

(require 'shr)          ;; HTML rendering
(require 'company)      ;; Autocompletion
(require 'company-math) ;; Math symbols
(require 'cl-lib)       ;; Compatibility
(require 'outline)      ;; Outlines
(require 'yasnippet)    ;; Templates
(require 'paren)        ;; Find matching block start
(require 'smie)         ;; Move around the source code

(require 'company-coq-abbrev)

(unless (require 'proof-site nil t)
  (message "company-coq: Unable to load proof-site. Is ProofGeneral installed, and did you add it to your load-path?"))

(defgroup company-coq nil
  "Options for the Coq company mode"
  :group 'company)

(defcustom company-coq-debug nil
  "Debug mode for company-coq."
  :group 'company-coq)

(defcustom company-coq-custom-snippets '("Section ${1:SectionName}.\n$0\nEnd $1."
                                         "Chapter ${1:ChapterName}.\n$0\nEnd $1." ;; Commented out in RefMan-ext.v
                                         "Module ${1:ModuleName}.\n$0\nEnd $1."
                                         "Module Type ${1:ModuleTypeName}.\n$0\nEnd $1.")
  "Custom YAS snippets")

(defcustom company-coq-autocomplete-symbols-dynamic nil
  "Autocomplete theorem names by periodically querying coq about defined identifiers. This is an experimental feature. It requires a patched version of Coq to work properly; it will be very slow otherwise."
  :group 'company-coq)

(defcustom company-coq-autocomplete-context t
  "Autocomplete hypotheses by parsing the latest Goals output. This is an experimental feature."
  :group 'company-coq)

(defcustom company-coq-autocomplete-modules t
  "Autocomplete module names by periodically querying coq about the current load path. This is an experimental feature."
  :group 'company-coq)

(defcustom company-coq-autocomplete-block-end t
  "Autocomplete the name of the last opened block. This is an experimental feature."
  :group 'company-coq)

(defcustom company-coq-autocomplete-symbols t
  "Autocomplete symbols by searching in the buffer for lemmas and theorems. If company-coq-autocomplete-symbols-dynamic is non-nil, query the proof assistant instead of searching."
  :group 'company-coq)

(defcustom company-coq-fast nil
  "Indicates whether we have access to a faster, patched REPL"
  :group 'company-coq)

(defcustom company-coq-explicit-placeholders t
  "Show holes using explicit placeholders"
  :group 'company-coq)

(defcustom company-coq-backends '(company-math-symbols-unicode company-coq-reserved-keywords company-coq-keywords)
  "List of backends to use for completion."
  :group 'company-coq)

(defcustom company-coq-disabled-patterns '("without loss")
  "List of patterns that are not imported from Proof General's list."
  :group 'company-coq)

(defvar company-coq-disabled-patterns-regexp (regexp-opt company-coq-disabled-patterns)
  "Regexp version of `company-coq-disabled-patterns'")

(defcustom company-coq-sorted-backends '(company-math-symbols-unicode
                                         company-coq-reserved-keywords
                                         company-coq-block-end
                                         company-coq-modules
                                         company-coq-context
                                         company-coq-keywords
                                         company-coq-symbols)
  "List of all backends, listed in the order in which you want
the results displayed. Note that the first backend that returns a
prefix superseeds all the others; they all must work with the
same prefix."
  :group 'company-coq)

(defvar company-coq-asking-question nil
  "Indicates whether a interaction has been initiated with the prover, to disable the input and output hooks.")

(defvar company-coq-symbols-reload-needed nil
  "Indicates whether a reload of all symbols might be needed. This variable is
  set from places where immediate reloading is impossible, for example in proof-shell-insert-hook")

(defvar company-coq-modules-reload-needed nil
  "Indicates whether a reload of all modules might be needed. This variable is
  set from places where immediate reloading is impossible, for example in proof-shell-insert-hook")

(defvar company-coq-dynamic-symbols nil
  "Keeps track of defined symbols.")

(defvar company-coq-buffer-defuns nil
  "Keeps track of buffer symbols, based on regexp searches")

(defvar company-coq-current-context nil
  "Keeps track of the context while proofs are ongoing.")

(defvar company-coq-known-path-specs nil
  "Keeps track of paths specs in load path.")

(defvar  company-coq-known-keywords nil
  "List of defined Coq syntax forms")

(defvar company-coq-last-goals-output nil
  "If proof-shell-last-goals-output matches this, it is ignored. This prevents old goals from being reparsed.")

(defconst company-coq-id-regexp-base "[a-zA-Z0-9_]")

(defconst company-coq-rich-id-regexp-base "[a-zA-Z0-9_.]")

(defconst company-coq-prefix-regexp-base "[a-zA-Z0-9_.!]") ;; '!' included so that patterns like [intros!] still work

(defconst company-coq-all-symbols-slow-regexp (concat "\\`\\(" company-coq-rich-id-regexp-base "+\\):.*\\'")
  "Regexp used to filter out lines without symbols in output of SearchPattern")

(defconst company-coq-goals-hyp-regexp (concat "\\`  \\(" company-coq-id-regexp-base "+\\) : \\(.*\\)\\'")
  "Regexp used to find hypotheses in goals output")

(defconst company-coq-goals-line-regexp (concat "\\`  ============================[= ]*\\'")
  "Regexp used to find hypotheses in goals output")

(defconst company-coq-path-part-regexp  "\\([^ ]+\\)")
(defconst company-coq-path-begin-regexp (concat "\\`"   company-coq-path-part-regexp " +\\'"))
(defconst company-coq-path-end-regexp   (concat "\\` +" company-coq-path-part-regexp   "\\'"))
(defconst company-coq-path-full-regexp  (concat "\\`"   company-coq-path-part-regexp " +" company-coq-path-part-regexp "\\'"))

(defun company-coq-make-headers-regexp (headers regexp-base)
  (concat "^[ \t]*\\b\\(" (regexp-opt headers) "\\)"
          "[[:space:]]+\\(" regexp-base "+\\)"))

(defconst company-coq-defuns-kwds '("Class" "CoFixpoint" "CoInductive"
                                    "Corollary" "Definition" "Example"
                                    "Fact" "Fixpoint" "Function"
                                    "Inductive" "Instance" "Lemma" "Let"
                                    "Ltac" "Program"  "Program Fixpoint"
                                    "Record" "Theorem" "with"))

(defconst company-coq-defuns-regexp (company-coq-make-headers-regexp company-coq-defuns-kwds
                                                                     company-coq-id-regexp-base)
  "Regexp used to locate symbol definitions in the current buffer.
This is mostly useful of company-coq-autocomplete-symbols-dynamic is nil.")

(defconst company-coq-block-end-regexp (company-coq-make-headers-regexp '("End")
                                                                          company-coq-id-regexp-base)
  "Regexp used to find section endings")

(defun company-coq-all-symbols-prelude ()
  "Command to run before listing all symbols, using a patched version of Coq"
  (when company-coq-fast "Set Search Minimal"))

(defconst company-coq-all-symbols-cmd "SearchPattern _"
  "Command used to list all symbols.")

(defvar company-coq-extra-symbols-cmd nil
  "Command used to list more symbols (SearchPattern _ doesn't search inside modules in 8.4).")

(defun company-coq-all-symbols-coda ()
  "Command to run after listing all symbols, using a patched version of Coq"
  (when company-coq-fast "Unset Search Minimal"))

(defun company-coq-all-symbols-filter-line ()
  "Lambda used to filter each output line"
  (if company-coq-fast
      (lambda (line) (> (length line) 0))
    (lambda (line) (string-match company-coq-all-symbols-slow-regexp line))))

(defun company-coq-all-symbols-extract-names ()
  "Lambda used to extract names from the list of output lines"
  (if company-coq-fast
      'identity
    (lambda (lines) (mapcar (lambda (line) (replace-regexp-in-string company-coq-all-symbols-slow-regexp "\\1" line)) lines))))

(defconst company-coq-doc-cmd "About %s"
  "Command used to retrieve the documentation of a symbol.")

(defconst company-coq-def-cmd "Print %s"
  "Command used to retrieve the definition of a symbol.")

(defconst company-coq-modules-cmd "Print LoadPath."
  "Command use to retrieve module path specs (for module name completion).")

(defconst company-coq-compiled-regexp "\\.vo\\'"
  "Regexp matching the extension of compiled Coq files.")

(defconst company-coq-prefix-regexp (concat company-coq-prefix-regexp-base "*")
  "Regexp used to find completion prefixes")

(defconst company-coq-symbol-regexp (concat company-coq-rich-id-regexp-base "*")
  "Regexp used to find symbol at point")

(defconst company-coq-undefined-regexp " not a defined object.$"
  "Regexp used to detect missing documentation (useful if database becomes outdated)")

(defconst company-coq-end-of-def-regexp "is \\(defined\\|assumed\\)"
  "Regexp used to detect signs that new definitions have been added to the context")

(defconst company-coq-error-regexp "\\`Error: "
  "Regexp used to detect errors (useful in particular to prevent reloading the modules list ater a failed import.")

(defconst company-coq-abort-proof-regexp "Current goals? aborted"
  "Regexp used to detect signs that new definitions have been added to the context")

(defconst company-coq-import-regexp "\\(Require\\)\\|\\(Import\\)"
  "Regexp used to detect signs that new definitions will be added to the context")

(defconst company-coq-load-regexp "\\(LoadPath\\)"
  "Regexp used to detect signs that new paths will be added to the load path")

(defconst company-coq-doc-tagline "Documentation for symbol %s"
  "Format string for the header of the documentation buffer")

(defconst company-coq-doc-def-sep "\n---\n\n"
  "Separation line between the output of company-coq-doc-cmd and company-coq-def-cmd in the doc buffer.")

(defconst company-coq-dabbrev-to-yas-regexp "#\\|@{\\([^}]+\\)}"
  "Used to match replace holes in dabbrevs")

(defconst company-coq-placeholder-regexp (concat company-coq-dabbrev-to-yas-regexp
                                                 "\\|\\${\\([^}]+\\)}\\|\\$[[:digit:]]")
  "Used to count placeholders in abbrevs")

(defconst company-coq-section-kwds '("Chapter" "Module" "Module Type" "Section")
  "Keywords used in outline mode and in company-coq-occur")


(defconst company-coq-outline-kwds `("Equations" "Goal" "Notation" "Remark" "Tactic Notation"
                                     ,@company-coq-section-kwds ,@company-coq-defuns-kwds)
  "Keywords used in outline mode and in company-coq-occur")

(defconst company-coq-section-regexp (company-coq-make-headers-regexp company-coq-section-kwds
                                                                      company-coq-id-regexp-base)
  "Regexp used to locate the closest section opening")

;; TODO: Would be nice to fold [Require Import]s together
(defconst company-coq-outline-regexp (company-coq-make-headers-regexp company-coq-outline-kwds
                                                                      company-coq-id-regexp-base)
  "Regexp used to locate headings")

(defun company-coq-outline-level ()
  "Function used to determine the current outline level"
  0)

(defconst company-coq-outline-heading-end-regexp "\\.[ \t\n]\\|\n"
  "Regexp used to locate the end of a heading")

(defconst script-full-path load-file-name
  "Full path of this script")

(defface company-coq-doc-header-face
  '((t :inherit highlight :height 1.2))
  "Face used to highlight the target line in the docs"
  :group 'defaut)

(defface company-coq-doc-tt-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used to highlight the keywords in the docs"
  :group 'defaut)

(defface company-coq-doc-i-face
  '((t :inherit font-lock-variable-name-face :weight bold :slant italic))
  "Face used to highlight the symbol names in the docs"
  :group 'defaut)

(when nil
  (defcustom company-coq-symbol-matching-scheme 'substring
    "The strategy used to look for keywords"
    :group company-coq)

  (defun company-coq-symbol-matching-scheme-is-plain ()
    (equal company-coq-symbol-matching-scheme 'plain)))

(defun company-coq-dbg (format &rest args)
  "Print a message if company-coq-debug is non-nil"
  (when company-coq-debug
    (apply 'message (concat "company-coq: " format) args)))

(defun company-coq-ask-prover (question)
  (when question
    (if (and (company-coq-prover-available) (fboundp 'proof-shell-invisible-cmd-get-result))
        (progn
          (setq company-coq-asking-question t)
          (unwind-protect
              (proof-shell-invisible-cmd-get-result question)
            (setq company-coq-asking-question nil)))
      (company-coq-dbg "Prover not available; question discarded"))))

(defun company-coq-split-lines (str)
  (if str (split-string str "\n")))

(defun company-coq-join-lines (lines sep &optional trans)
  (if lines (mapconcat (or trans 'identity) lines sep)))

(defun company-coq-take-while-non-empty (lines)
  (if lines
      (cl-loop for line in lines
               while (not (string-equal line ""))
               collect line)))

(defun company-coq-string-or-undefined (doc)
  "Returns DOC, unless DOC is a message saying that a symbol is undefined"
  (unless (string-match company-coq-undefined-regexp doc) doc))

(defun company-coq-boundp-string-match (regexp symbol)
  (and (boundp symbol)
       (symbol-value symbol)
       (string-match regexp (symbol-value symbol))))

(defun company-coq-boundp-equal (var symbol)
  (and (boundp var) (equal (symbol-value var) symbol)))

(defun company-coq-value-or-nil (symbol)
  (and (boundp symbol) (symbol-value symbol)))

(defun company-coq-extend-path (path components)
  "Contruct a path by appending each element in COMPONENTS to PATH"
  (mapc (lambda (component)
          (setq path (expand-file-name component path))) components)
  path)

(defun company-coq-chomp (l1 l2)
  "Remove the longest common prefix of l1 and l2. Returns a cons of what remains"
  ;; (message "> [%s] [%s]" l1 l2)
  (while (and l1 l2 (string-prefix-p (car l1) (car l2)))
    (setq l1 (cdr l1))
    (setq l2 (cdr l2)))
  ;; (message "< [%s] [%s]" l1 l2)
  (cons l1 l2))

(defun company-coq-split-logical-path (path)
  "Split a logical path, such as a module path, into individual components"
  (unless (string-equal path "<>")
    (split-string path "\\.")))

(defun company-coq-prover-available ()
  (let ((available (and (not company-coq-asking-question)
                        (fboundp 'proof-shell-available-p)
                        (proof-shell-available-p))))
    (when (not available)
      (company-coq-dbg "company-coq-prover-available: Prover not available"))
    available))

(defun company-coq-force-reload-with-prover (track-symbol store-symbol load-function)
  (company-coq-dbg "company-coq-force-reload-from-prover: %s %s %s"
                   (symbol-name track-symbol) (symbol-name store-symbol) (symbol-name load-function))
  (if (not (company-coq-prover-available))
      (company-coq-dbg "company-coq-force-reload-from-prover: Reloading aborted; proof process busy")
    (set track-symbol nil)
    (set store-symbol (funcall load-function))))

(defun company-coq-init-db (db initfun)
  (interactive)
  (company-coq-dbg "company-coq-init-db: Loading %s (if nil; currently has %s elems)" db (length (symbol-value db)))
  (or (symbol-value db)
      (progn
        (company-coq-dbg "company-coq-init-db: reloading")
        (funcall initfun))))

(defun company-coq-get-symbols ()
  "Load symbols by issuing command company-coq-all-symbols-cmd and parsing the results. Do not call if proof process is busy."
  (interactive)
  (with-temp-message "company-coq: Loading symbols..."
    (let* ((start-time     (current-time))
           (_              (company-coq-ask-prover (company-coq-all-symbols-prelude)))
           (output         (company-coq-ask-prover company-coq-all-symbols-cmd))
           (extras         (company-coq-ask-prover company-coq-extra-symbols-cmd))
           (_              (company-coq-ask-prover (company-coq-all-symbols-coda)))
           (half-time      (current-time))
           (lines          (nconc (company-coq-split-lines output) (company-coq-split-lines extras)))
           (line-filter    (company-coq-all-symbols-filter-line))
           (line-extractor (company-coq-all-symbols-extract-names))
           (filtered-lines (cl-remove-if-not (lambda (line) (funcall line-filter line)) lines))
           (names          (company-coq-union-sort #'string-equal #'string-lessp (funcall line-extractor filtered-lines))))
      (message "Loaded %d symbols (%d lines) in %.03f+%.03f seconds"
               (length names) (length lines) (float-time (time-subtract half-time start-time)) (float-time (time-since half-time)))
      names)))

(defun company-coq-force-reload-symbols ()
  (interactive)
  (when company-coq-autocomplete-symbols-dynamic
    (company-coq-force-reload-with-prover 'company-coq-symbols-reload-needed
                                          'company-coq-dynamic-symbols
                                          #'company-coq-get-symbols)))

(defun company-coq-init-symbols-or-defuns ()
  (interactive)
  (company-coq-dbg "company-coq-init-symbols-or-defuns: Loading symbols (if never loaded)")
  (if company-coq-autocomplete-symbols-dynamic
      (company-coq-init-db 'company-coq-dynamic-symbols 'company-coq-force-reload-symbols)
    (company-coq-reload-buffer-defuns)))

(defun company-coq-reload-buffer-defuns (&optional start end)
  (interactive) ;; FIXME should timeout after some time, and should accumulate search results
  (setq start (or start (point-min)))
  (setq end   (or end   (point-at-bol)))
  (let ((case-fold-search nil)
        (symbols          nil))
    (save-excursion
      (goto-char start)
      (while (search-forward-regexp company-coq-defuns-regexp end t)
        (push (match-string-no-properties 2) symbols)))
    (setq company-coq-buffer-defuns symbols)))

(defun company-coq-line-is-import-p ()
  (save-excursion
    (let* ((bol           (point-at-bol))
           (command-begin (or (search-backward ". " bol t) bol)))
      (goto-char command-begin)
      (looking-at " *\\(Require\\)\\|\\(Import\\)\\|\\(Export\\) *"))))

(defun company-coq-line-is-block-end-p ()
  (looking-back company-coq-block-end-regexp))

(defun company-coq-parse-path-specs (loadpath-lines)
  "Parse lines of output from company-coq-modules-cmd. Output is
a list of pairs of paths in the form (LOGICAL . PHYSICAL)"
  ;; (message "Path specs: discarding %s" current-spec)))
  (cl-loop for     line
           in      loadpath-lines
           with    current-spec = `(nil . nil)
           if      (string-match company-coq-path-begin-regexp line)
           do      (setcar current-spec (match-string 1 line))
           else if (string-match company-coq-path-end-regexp line)
           do      (setcdr current-spec (match-string 1 line))
           else if (string-match company-coq-path-full-regexp line)
           do      (setq current-spec `(,(match-string 1 line) . ,(match-string 2 line)))
           when    (and (car-safe current-spec) (cdr-safe current-spec))
           collect current-spec
           and do  (setq current-spec `(nil . nil))))

(defun company-coq-get-path-specs ()
  "Load modules by issuing command company-coq-modules-cmd and parsing the results. Do not call if proof process is busy."
  (interactive)
  (with-temp-message "company-coq: Loading module names..."
    (let* ((time       (current-time))
           (output     (company-coq-ask-prover company-coq-modules-cmd))
           (lines      (cdr-safe (company-coq-split-lines output)))
           (path-specs (company-coq-parse-path-specs lines)))
      (message "Loaded %d modules paths (%.03f seconds)" (length path-specs) (float-time (time-since time)))
      path-specs)))

(defun company-coq-force-reload-modules ()
  (interactive)
  (company-coq-force-reload-with-prover 'company-coq-modules-reload-needed
                                        'company-coq-known-path-specs
                                        #'company-coq-get-path-specs))

(defun company-coq-init-modules ()
  (interactive)
  (company-coq-dbg "company-coq-init-modules: Loading modules (if never loaded)")
  (company-coq-init-db 'company-coq-known-path-specs 'company-coq-force-reload-modules))

(defun company-coq-get-pg-keywords-db ()
  (apply #'append
         (mapcar #'company-coq-value-or-nil ;; Don't fail when PG is missing
                 '(coq-tactics-db coq-solve-tactics-db coq-solve-cheat-tactics-db coq-tacticals-db coq-commands-db))))

(defun company-coq-get-own-keywords-db ()
  (apply #'append company-coq-abbrevs-all))

(defun company-coq-normalize-abbrev (kwd)
  (downcase
   (replace-regexp-in-string
    "[ .]+\\'" ""
    (replace-regexp-in-string
     (concat " *\\(" company-coq-placeholder-regexp "\\) *") "#"
     kwd))))

(defun company-coq-parse-keywords-pg-entry (menuname _abbrev insert &optional _statech _kwreg insert-fun _hide)
  (when (or (and insert (not (string-match-p company-coq-disabled-patterns-regexp insert)))
            (and (not insert) insert-fun))
    (propertize (if insert-fun menuname insert)
                'source 'pg
                'insert insert
                'insert-fun insert-fun
                'stripped (company-coq-normalize-abbrev insert)))) ;; TODO: Remove inter-word spaces

(defun company-coq-parse-own-db-entry (abbrev-and-anchor)
  (let ((abbrev (car abbrev-and-anchor))
        (anchor (cdr abbrev-and-anchor)))
    (propertize abbrev
                'source 'own
                'anchor anchor
                'insert abbrev
                'stripped (company-coq-normalize-abbrev abbrev))))

(defun company-coq-parse-custom-db-entry (abbrev)
  (propertize abbrev
              'source 'custom
              'insert abbrev
              'stripped (company-coq-normalize-abbrev abbrev)))

(defun company-coq-abbrev-equal (a1 a2)
  (and (equal (company-coq-read-normalized-abbrev a1)
              (company-coq-read-normalized-abbrev a2))
       (not (equal (company-coq-read-abbrev-source a1)
                   (company-coq-read-abbrev-source a2)))))

(defun company-coq-read-normalized-abbrev (kwd)
  (get-text-property 0 'stripped kwd))

(defun company-coq-read-abbrev-source (kwd)
  (get-text-property 0 'source kwd))

(defun company-coq-union-nosort (test comp key &rest lists)
  (let ((merged  (cl-stable-sort (apply #'append lists) comp :key key))
        (prev    nil))
    (while merged
      (let ((top (pop merged)))
        (when (and prev (funcall test top prev))
          (put-text-property 0 (length top) 'dup t top))
        (setq prev top)))
    (cl-loop for abbrev in (apply #'append lists)
             if (not (get-text-property 0 'dup abbrev))
             collect abbrev)))

(defun company-coq-union-sort (test comp &rest lists)
  (let ((merged  (cl-sort (apply #'append lists) comp))
        (deduped nil)
        (prev    nil))
    (while merged
      (let ((top (pop merged)))
        (unless (and prev (funcall test top prev))
          (push top deduped))
        (setq prev top)))
    deduped))

(defun company-coq-number (ls)
  (let ((num 0))
    (mapc (lambda (x)
            (put-text-property 0 (length x) 'num num x)
            (setq num (+ 1 num)))
          ls)))

(defun company-coq-get-annotated-keywords ()
  (company-coq-dbg "company-coq-get-annotated-keywords: Called")
  (let ((pg-keywords     (remove nil (mapcar (lambda (db-entry)
                                               (apply 'company-coq-parse-keywords-pg-entry db-entry))
                                             (company-coq-get-pg-keywords-db))))
        (own-keywords    (company-coq-number
                          (mapcar #'company-coq-parse-own-db-entry (company-coq-get-own-keywords-db))))
        (custom-keywords (mapcar #'company-coq-parse-custom-db-entry company-coq-custom-snippets)))
    (company-coq-union-nosort
     #'company-coq-abbrev-equal #'string-lessp #'company-coq-read-normalized-abbrev
     custom-keywords own-keywords pg-keywords)))

(defun company-coq-force-reload-keywords ()
  (company-coq-dbg "company-coq-force-reload-keywords: Called")
  (setq company-coq-known-keywords (company-coq-get-annotated-keywords))
  (company-coq-dbg "company-coq-force-reload-keywords: Loaded %s symbols" (length company-coq-known-keywords)))

(defun company-coq-init-keywords ()
  (interactive)
  (company-coq-dbg "company-coq-init-keywords: Loading keywords (if never loaded)")
  (company-coq-init-db 'company-coq-known-keywords 'company-coq-force-reload-keywords))

(defun company-coq-is-lower (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[[:lower:]]" str)))

(defun company-coq-string-lessp-foldcase (str1 str2)
  (string-lessp (upcase str1) (upcase str2)))

(defmacro company-coq-lessp-fallback (a1 a2 fallback-t &optional fallback-nil)
  (declare (indent defun))
  `(let ((a1 ,a1)
         (a2 ,a2))
     (or (and      a1  (not a2))
         (and      a1       a2  ,fallback-t)
         (and (not a1) (not a2) ,(or fallback-nil fallback-t)))))

(defmacro company-coq-attr-lessp (symbol str1 str2 extraction cmp fallback-t &optional fallback-nil)
  (declare (indent defun))
  `(let ((a1 (,extraction ,cmp (get-text-property 0 ,symbol ,str1)))
         (a2 (,extraction ,cmp (get-text-property 0 ,symbol ,str2))))
     (company-coq-lessp-fallback a1 a2
       ,fallback-t ,fallback-nil)))

(defun company-coq-string-lessp-symbols (str1 str2)
  (company-coq-attr-lessp 'match-beginning str1 str2 eq 0
    (company-coq-string-lessp-foldcase str1 str2)))

(defun company-coq-string-lessp-keywords (str1 str2)
  (company-coq-attr-lessp 'source str1 str2 eq 'custom
    (company-coq-lessp-fallback (company-coq-is-lower str1) (company-coq-is-lower str2)
      (company-coq-attr-lessp 'num str1 str2 or nil
        (< a1 a2)
        (company-coq-string-lessp-foldcase str1 str2))
      (company-coq-string-lessp-foldcase str1 str2))))

(defun company-coq-make-proper-list (improper-list) ;;TODO used?
  (let ((last-cell (last improper-list)))
    (setcdr last-cell nil)
    improper-list))

(defun company-coq-propertize-match (match beginning end)
  (company-coq-dbg "company-coq-propertize-match: %s %s %s" match beginning end)
  (put-text-property 0 (length match) 'match-beginning beginning match)
  (put-text-property 0 (length match) 'match-end end match)
  match)

(defun company-coq-complete-prefix-substring (prefix completions &optional ignore-case)
  "List elements of COMPLETIONS containing with PREFIX"
  (let ((completion-ignore-case ignore-case)
        (case-fold-search       ignore-case)
        (prefix-re              (regexp-quote prefix)))
    (cl-loop for completion in completions
             if (string-match prefix-re completion)
             collect (company-coq-propertize-match completion (match-beginning 0) (match-end 0)))))

(defun company-coq-complete-prefix-fuzzy (prefix completions &optional ignore-case)
  "List elements of COMPLETIONS matching PREFIX"
  (let ((completion-ignore-case ignore-case))
    (company-coq-make-proper-list (completion-pcm-all-completions prefix completions nil (length prefix)))))

(defun company-coq-complete-prefix (prefix completions &optional ignore-case)
  "List elements of COMPLETIONS starting with PREFIX"
  (company-coq-dbg "company-coq-complete-prefix: Completing for prefix %s (%s symbols)" prefix (length completions))
  (let ((completion-ignore-case ignore-case)
        (prefix-len             (length prefix)))
    (mapcar
     (lambda (completion) (company-coq-propertize-match completion 0 prefix-len))
     (all-completions prefix completions))))

(defun company-coq-symbols-or-defuns ()
  (if (and company-coq-autocomplete-symbols-dynamic (company-coq-in-scripting-mode))
      company-coq-dynamic-symbols ;; Use actual symbols iff it's enabled and scripting mode is on
    company-coq-buffer-defuns))

(defun company-coq-complete-symbol-or-defun (prefix)
  "List elements of company-coq-dynamic-symbols or company-coq-buffer-defuns containing PREFIX"
  (interactive)
  (company-coq-complete-prefix-substring prefix (company-coq-symbols-or-defuns)))

(defun company-coq-complete-keyword (prefix)
  "List elements of company-coq-known-keywords starting with PREFIX"
  (interactive)
  (company-coq-complete-prefix prefix company-coq-known-keywords))

(defun company-coq-complete-context (prefix)
  "List elements of company-coq-current-context containing PREFIX"
  (interactive)
  (company-coq-complete-prefix-substring prefix company-coq-current-context))

(defun company-coq-no-empty-strings (ss)
  (cl-remove-if (lambda (s) (string-equal s "")) ss))

(defun company-coq-match-logical-paths (module-atoms path-atoms)
  "Produces a cons (QUALID-ATOMS SEARCH-ATOMS LEFT-PTH) from a
module path and a logical path. This function distinguishes three
cases:

1. The logical path is longer; in this case, the qualifier is the
   full logical path, and the search term is empty.

2. The module path is longer; in this case, the qualifier is the
   full logical path, plus what remains of the module path (minus
   the last item of the module path), and the search term is the
   last item of the module path.

3. The two paths don't match; in this case, there is no qualifier
   nor search term."
  (pcase (company-coq-chomp module-atoms path-atoms)
    (`(,mod .  nil) (let ((subdirectory-atoms (butlast mod)))
                      (unless (member "" subdirectory-atoms) ;; We don't support skipping over subdirectories
                       (cons (append path-atoms subdirectory-atoms) (cons mod nil)))))
    (`(nil  . ,pth) (cons path-atoms (cons nil pth)))
    (_              nil)))

(defun company-coq-complete-module-unqualified (search-path search-regexp)
  "Find module names matching SEARCH-REGEXP in SEARCH-PATH.
Results are file names only, and do not include the .vo
extension." ;; TODO format directories properly
  (when (file-exists-p search-path)
    (cl-loop for file in (directory-files search-path nil nil t)
             if      (or (not search-regexp) (string-match-p search-regexp file))
             if      (string-match-p company-coq-compiled-regexp file)
             collect (replace-regexp-in-string company-coq-compiled-regexp "" file)
             else if (and (not (string-match-p "\\." file)) (file-directory-p (expand-file-name file search-path)))
             collect (concat file "."))))

(defun company-coq-take-summed-lengths (ls count)
  (cl-loop for i = 0 then (+ 1 i)
           for l = ls then (cdr l)
           while (< i count)
           sum (length (car-safe l))))

(defun company-coq-qualify-module-names (mod-names qualid-atoms fully-matched-count part-matched-len physical-path)
  "Qualify each name in MOD-NAMES using QUALID-ATOMS."
  (when mod-names
    (let* ((qualid     (mapconcat 'identity qualid-atoms "."))
           (prefix     (if qualid-atoms (concat qualid ".") ""))
           (m-pref-len (company-coq-take-summed-lengths qualid-atoms fully-matched-count))
           (match-end  (+ m-pref-len ;; fully matched prefix
                          part-matched-len ;; partially matched element (end of search term)
                          fully-matched-count))) ;; dots
      (mapcar (lambda (mod-name)
                (propertize (concat prefix mod-name)
                            'meta      (concat physical-path " -> " mod-name)
                            'location  (expand-file-name (concat mod-name ".v") physical-path)
                            'match-end match-end))
              mod-names))))

(defun company-coq-complete-module-qualified (qualid-atoms search-atoms physical-path
                                              fully-matched-count part-matched-len)
  "Find qualified module names in PHYSICAL-PATH that match SEARCH-ATOMS."
  ;; (message "> [%s] [%s] [%s]" (prin1-to-string qualid-atoms) (prin1-to-string search-atoms) physical-path)
  (let* ((kwd           (car-safe (last search-atoms)))
         (nil-kwd       (or (not kwd) (equal kwd "")))
         (ext-path      (company-coq-extend-path physical-path search-atoms))
         (search-path   (if nil-kwd (file-name-as-directory ext-path)
                          (file-name-directory ext-path)))
         (search-regexp (if nil-kwd nil (concat "\\`" (regexp-quote kwd))))
         (mod-names     (company-coq-complete-module-unqualified
                         search-path search-regexp)))
    ;; (message "Searching in [%s] with regexp [%s]: [%s]" search-path search-regexp mod-names)
    (company-coq-qualify-module-names mod-names qualid-atoms fully-matched-count part-matched-len search-path)))

(defun company-coq-complete-module-from-atoms (module-atoms path-atoms physical-path)
  "Wrapper around company-coq-complete-module-qualified."
  (pcase (company-coq-match-logical-paths module-atoms path-atoms)
    (`(,qualid . (,search . ,pth))
     (let* (;; Fully matched count is the full qualid if the search term
            ;; (module-atoms) was strictly longer than the path, and otherwise
            ;; one less than the number of elements in common
            (fully-matched-count (if (eq search nil)
                                     (- (length path-atoms) (length pth) 1)
                                   (length qualid)))
            ;; Part matched len is always the length of the last search term
            (part-matched-len    (length (car-safe (last module-atoms)))))
       (company-coq-complete-module-qualified qualid search physical-path fully-matched-count part-matched-len)))))

(defun company-coq-complete-module-from-path-spec (module-atoms path-spec)
  "Find modules matching MODULE-ATOMS in PATH-SPEC.
This essentially attempts to match MODULE-ATOMS to the logical
path in PATH-SPEC, and for each matching position computes a
search term and a qualifier."
  (destructuring-bind (logical-path . physical-path) path-spec
    (let* ((path-atoms   (company-coq-split-logical-path logical-path))
           (completions  (list (company-coq-complete-module-from-atoms module-atoms nil physical-path))))
      (while path-atoms
        (push (company-coq-complete-module-from-atoms
               module-atoms path-atoms physical-path)
              completions)
        (setq path-atoms (cdr path-atoms)))
      (apply #'append completions))))

(defun company-coq-complete-modules (module)
  (when module
    (let ((module-atoms (company-coq-split-logical-path module))
          (completions nil))
      (mapc (lambda (path-spec)
              (push (company-coq-complete-module-from-path-spec
                     module-atoms path-spec)
                    completions))
            company-coq-known-path-specs)
      (apply #'company-coq-union-sort
             #'string-equal #'string-lessp completions))))

(defun company-coq-complete-block-end (prefix)
  "Find the closest section/chapter/... opening, if it matches the prefix at point"
  (when (and prefix (boundp 'show-paren-data-function) (functionp show-paren-data-function))
    (save-excursion
      ;; Find matching delimiter
      (when (re-search-backward company-coq-block-end-regexp)
        (goto-char (+ 1 (match-beginning 1)))
        (let ((delim-info (funcall show-paren-data-function)))
          (when delim-info
            (cl-destructuring-bind (_hb _he _tb there-end mismatch) delim-info
                (when (and (not mismatch) there-end)
                  (goto-char there-end)
                  (re-search-backward company-coq-section-regexp nil t)
                  (let ((nearest-section-name (match-string-no-properties 2)))
                    (when (and nearest-section-name
                               (equal prefix (substring nearest-section-name 0 (length prefix))))
                      (list nearest-section-name)))))))))))

(defun company-coq-complete-reserved (prefix)
  (when (and (boundp 'coq-reserved) (member prefix coq-reserved))
    (list prefix)))

(defun company-coq-shell-output-is-end-of-def ()
  "Checks whether the output of the last command matches company-coq-end-of-def-regexp"
  (company-coq-boundp-string-match company-coq-end-of-def-regexp 'proof-shell-last-output))

(defun company-coq-shell-output-proof-aborted ()
  "Checks whether the output of the last command matches company-coq-end-of-def-regexp"
  (company-coq-boundp-string-match company-coq-abort-proof-regexp 'proof-shell-last-output))

(defun company-coq-shell-output-is-end-of-proof ()
  "Checks whether proof-general signaled a finished proof"
  (company-coq-value-or-nil 'proof-shell-proof-completed))

(defun company-coq-shell-output-is-error ()
  "Checks whether proof-general signaled an error"
  (company-coq-boundp-string-match company-coq-error-regexp 'proof-shell-last-output))

(defun company-coq-maybe-reload-each ()
  (when company-coq-symbols-reload-needed (company-coq-force-reload-symbols))
  (when company-coq-modules-reload-needed (company-coq-force-reload-modules)))

(defmacro company-coq-remember-hyp (hyp-cons context)
  `(destructuring-bind (name . type-lines) ,hyp-cons
     (when (and name type-lines)
       ;; (message "New hyp: [%s . [%s]]" name type-lines)
       (let ((type (mapconcat #'company-coq-trim (nreverse type-lines) " ")))
         (push (propertize name 'meta type) ,context)))))

(defun company-coq-parse-goal-lines (goal-lines)
 (cl-loop for     line
          in      goal-lines
          with    context  = nil
          with    current-hyp = `(nil . nil)
          while   (not (string-match-p company-coq-goals-line-regexp line))
          if      (string-match company-coq-goals-hyp-regexp line)
          do      (company-coq-remember-hyp current-hyp context)
          and do  (setq current-hyp `(,(match-string 1 line) . ,(list (match-string 2 line))))
          else do (push line (cdr current-hyp))
          finally (company-coq-remember-hyp current-hyp context)
          finally return context))

(defun company-coq-maybe-reload-context (&optional end-of-proof)
  "Updates company-coq-current-context."
  (company-coq-dbg "company-coq-maybe-reload-context: Called")
  (let* ((output        (company-coq-value-or-nil 'proof-shell-last-goals-output))
         (is-new-output (not (string-equal output company-coq-last-goals-output))))
    (cond (end-of-proof  (company-coq-dbg "company-coq-maybe-reload-context: Clearing context")
                         (setq company-coq-current-context nil)
                         (setq output nil))
          (is-new-output (company-coq-dbg "company-coq-maybe-reload-context: Reloading context")
                         (setq company-coq-current-context (company-coq-parse-goal-lines
                                                            (company-coq-split-lines output)))))
    (setq company-coq-last-goals-output output)))

(defun company-coq-maybe-proof-output-reload-things ()
  "Updates company-coq-symbols-reload-needed if a proof just
completed or if output mentions new symbol, then calls
company-coq-maybe-reload-things. Also calls company-coq-maybe-reload-context."
  (interactive)
  (company-coq-dbg "company-coq-maybe-proof-output-reload-things: Called")
  (unless company-coq-asking-question
    (let ((is-error         (company-coq-shell-output-is-error))
          (is-aborted       (company-coq-shell-output-proof-aborted))
          (is-end-of-def    (company-coq-shell-output-is-end-of-def))
          (is-end-of-proof  (company-coq-shell-output-is-end-of-proof)))
      (when is-end-of-proof (company-coq-dbg "company-coq-maybe-proof-output-reload-things: At end of proof"))
      (when is-end-of-def   (company-coq-dbg "company-coq-maybe-proof-output-reload-things: At end of definition"))
      (when is-aborted      (company-coq-dbg "company-coq-maybe-proof-output-reload-things: Proof aborted"))
      ;; (message "[%s] [%s] [%s]" company-coq-symbols-reload-needed is-end-of-def is-end-of-proof)
      (setq company-coq-symbols-reload-needed (or company-coq-symbols-reload-needed is-end-of-def is-end-of-proof))
      (company-coq-maybe-reload-context (or is-end-of-def is-end-of-proof is-aborted))
      (if is-error (company-coq-dbg "Last output was an error; not reloading")
        (run-with-idle-timer 0 nil #'company-coq-maybe-reload-each)))))

(defun company-coq-maybe-proof-input-reload-things ()
  "Reload symbols if input mentions new symbols"
  (interactive)
  (company-coq-dbg "company-coq-maybe-proof-input-reload-things: Called")
  (unless company-coq-asking-question
    (let* ((is-advancing  (company-coq-boundp-equal 'action 'proof-done-advancing))
           (is-retracting (company-coq-boundp-equal 'action 'proof-done-retracting))
           (is-import     (and is-advancing (company-coq-boundp-string-match company-coq-import-regexp 'string)))
           (is-load       (and is-advancing (company-coq-boundp-string-match company-coq-load-regexp   'string))))
      (when is-retracting (company-coq-dbg "company-coq-maybe-proof-input-reload-things: Rewinding"))
      (when is-import     (company-coq-dbg "company-coq-maybe-proof-input-reload-things: New import"))
      (when is-load       (company-coq-dbg "company-coq-maybe-proof-input-reload-things: Touching load path"))
      (setq company-coq-symbols-reload-needed (or company-coq-symbols-reload-needed is-retracting is-import))
      (setq company-coq-modules-reload-needed (or company-coq-modules-reload-needed is-import is-load))
      (when is-retracting (company-coq-maybe-reload-context t)))))

(defun company-coq-in-coq-mode ()
  (or (derived-mode-p 'coq-mode)
      (ignore (company-coq-dbg "Not in Coq mode"))))

(defun company-coq-in-scripting-mode ()
  (or (company-coq-value-or-nil 'proof-script-buffer)
      (ignore (company-coq-dbg "Not in scripting mode"))))

(defun company-coq-grab-prefix ()
  ;; Only one grab function; otherwise the first backend in the list of backend shadows the others
  ;; FIXME: Should not return nil at the beginning of a hole
  (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
    (save-excursion ;; TODO could be optimized
      (when (looking-back company-coq-prefix-regexp (point-at-bol) t)
        (match-string-no-properties 0)))))

(defun company-coq-symbol-at-point () ;; FIXME could use (coq-id-or-notation-at-point)
  (let ((before (and (looking-back company-coq-symbol-regexp (point-at-bol) t)
                     (match-string-no-properties 0)))
        (after  (and (looking-at company-coq-symbol-regexp)
                     (match-string-no-properties 0))))
    (and before after (replace-regexp-in-string "\\`[\\.]*\\(.+?\\)[\\.]*\\'" "\\1" (concat before after)))))

(defun company-coq-prefix-simple ()
  (interactive)
  (company-coq-dbg "company-coq-prefix-simple: Called")
  (when (company-coq-in-coq-mode)
    (company-coq-grab-prefix)))

(defun company-coq-prefix-module ()
  (interactive)
  (company-coq-dbg "company-coq-prefix-module: Called")
  (when (and (company-coq-in-coq-mode)
             (company-coq-in-scripting-mode)
             (company-coq-line-is-import-p))
    (company-coq-grab-prefix)))

(defun company-coq-prefix-block-end ()
  (interactive)
  (company-coq-dbg "company-coq-prefix-block-end: Called")
  (when (company-coq-line-is-block-end-p)
    (company-coq-prefix-simple)))

(defun company-coq-documentation (name)
  (company-coq-dbg "company-coq-documentation: Called for name %s" name)
  (when (company-coq-prover-available)
    (company-coq-string-or-undefined (company-coq-ask-prover (format company-coq-doc-cmd name)))))

(defun company-coq-definition (name)
  (company-coq-dbg "company-coq-definition: Called for name %s" name)
  (when (company-coq-prover-available)
    (company-coq-string-or-undefined (company-coq-ask-prover (format company-coq-def-cmd name)))))

(defun company-coq-get-header (doc)
  (when doc
    (company-coq-take-while-non-empty
     (company-coq-split-lines doc))))

(defun company-coq-documentation-header (name)
  (company-coq-get-header
   (company-coq-documentation name)))

(defun company-coq-definition-header (name)
  (company-coq-get-header
   (company-coq-definition name)))

(defun company-coq-trim (str)
  (replace-regexp-in-string "\\` *" "" (replace-regexp-in-string " *\\'" "" str)))

(defun company-coq-truncate-to-minibuf (str)
  (when str
    (let* ((minibuf-w  (window-body-width (minibuffer-window))))
      (if (> (length str) minibuf-w)
          (concat (substring str 0 (- minibuf-w 3)) "...")
        str))))

(defun company-coq-meta-symbol (name)
  (company-coq-dbg "company-coq-meta-symbol: Called for name %s" name)
  (company-coq-truncate-to-minibuf
   (company-coq-join-lines
    (company-coq-documentation-header name) " " 'company-coq-trim)))

(defun company-coq-meta-keyword (name)
  (company-coq-dbg "company-coq-meta-keyword: Called for name %s" name)
  (and (company-coq-get-anchor name) ;; substitute-command-keys doesn't work here
       "C-h: Quick docs. C-w: Full docs (scrollable)."))

(defun company-coq-meta-simple (name)
  (company-coq-dbg "company-coq-meta-simple: Called for name %s" name)
  (company-coq-truncate-to-minibuf
   (get-text-property 0 'meta name)))

(defun company-coq-location-simple (name)
  (company-coq-dbg "company-coq-location-simple: Called for name %s" name)
  (let ((fname (get-text-property 0 'location name)))
    (when (and fname (file-exists-p fname))
      (company-coq-with-clean-doc-buffer
        (insert-file-contents fname)
        (coq-mode)
        (cons (current-buffer) 0)))))

(defun company-coq-get-pg-buffer ()
  (get-buffer "*goals*"))

(defun company-coq-get-pg-window ()
  (let ((pg-buffer (company-coq-get-pg-buffer)))
    (and pg-buffer (get-buffer-window pg-buffer))))

(defun company-coq-display-in-pg-window (buffer alist)
  ;; This always displays the buffer, unless no window is available.  This was
  ;; important, because if the window is not displayed upon calling
  ;; shr-insert-document, then shr would get the window width incorrectly, and
  ;; thus fail to wrap text properly. Setting the wrap limit to a large value
  ;; fixes this.
  (company-coq-dbg "Called company-coq-display-in-pg-buffer with %s %s" buffer alist)
  (let ((pg-window (company-coq-get-pg-window)))
    (when pg-window
      (set-window-dedicated-p pg-window nil)
      (set-window-buffer pg-window buffer))
    (or pg-window (display-buffer buffer))))

(defmacro company-coq-with-clean-doc-buffer (&rest body)
  (declare (indent defun))
  `(progn
     (company-coq-dbg "company-prepare-doc-buffer: Called")
     (let ((doc-buffer (get-buffer-create "*company-documentation*")))
       (company-coq-display-in-pg-window doc-buffer nil)
       (with-current-buffer doc-buffer
         (let ((inhibit-read-only t))
           (remove-overlays)
           (erase-buffer)
           ,@body)))))

(defun company-coq-make-title-line ()
  (let ((overlay (make-overlay (point-at-bol) (+ 1 (point-at-eol))))) ;; +1 to cover the full line
    (overlay-put overlay 'face 'company-coq-doc-header-face)))

(defun company-coq-get-anchor (kwd)
  (get-text-property 0 'anchor kwd))

(defun company-coq-count-holes (snippet)
  (let* ((count   0)
         (counter (lambda (_) (setq count (+ 1 count)) ""))
         (_       (replace-regexp-in-string company-coq-placeholder-regexp counter snippet)))
    count))

(defun company-coq-annotation-keywords (candidate)
  (let* ((snippet   (company-coq-get-snippet candidate))
         (num-holes (and snippet (company-coq-count-holes snippet)))
         (prefix    (if (company-coq-get-anchor candidate) "… " "")))
    (if (and (numberp num-holes) (> num-holes 0))
        (format "%s<kwd (%d)>" prefix num-holes)
      (format "%s<kwd>" prefix))))

(defun company-coq-annotation-context (_)
  "<h>")

(defun company-coq-doc-buffer-symbols (name)
  (company-coq-dbg "company-coq-doc-buffer-symbols: Called for name %s" name)
  (let ((doc (company-coq-documentation name))
        (def (company-coq-join-lines (company-coq-definition-header name) "\n")))
    (when (and doc def)
      (let* ((fontized-name (propertize name 'font-lock-face 'company-coq-doc-i-face))
             (doc-tagline (format company-coq-doc-tagline fontized-name))
             (doc-full (concat doc-tagline "\n\n" doc company-coq-doc-def-sep def)))
        (company-coq-with-clean-doc-buffer
          (insert doc-full)
          (when (fboundp 'coq-response-mode)
            (coq-response-mode))
          (goto-char (point-min))
          (company-coq-make-title-line)
          (current-buffer))))))

(defun company-coq-shr-tag-tt (cont)
  (shr-fontize-cont cont 'company-coq-doc-tt-face))

(defun company-coq-shr-tag-i (cont)
  (shr-fontize-cont cont 'company-coq-doc-i-face))

(defun company-coq-doc-keywords-prettify-title (target-point truncate)
  (goto-char (or target-point (point-min)))
  (when target-point
    ;; Remove the star ("*") added by shr
    (delete-char 1)
    (company-coq-make-title-line)
    (when truncate
      ;; Company-mode returns to the beginning of the buffer, so centering
      ;; vertically doesn't work.  Instead, just truncate everything, leaving
      ;; just a bit of room for comments preceeding the tactic if any.
      (forward-line -2)
      (delete-region (point-min) (point)))))

(defun company-coq-doc-keywords-put-html (html-full-path truncate)
  (let ((doc (with-temp-buffer
               (insert-file-contents html-full-path)
               (libxml-parse-html-region (point-min) (point-max))))
        (shr-width most-positive-fixnum)
        (after-change-functions nil)
        (shr-external-rendering-functions '((tt . company-coq-shr-tag-tt)
                                            (i  . company-coq-shr-tag-i))))
    (shr-insert-document doc) ;; This sets the 'shr-target-id property upon finding the shr-target-id anchor
    (turn-on-visual-line-mode)
    (company-coq-doc-keywords-prettify-title (next-single-property-change (point-min) 'shr-target-id) truncate)))

(defun company-coq-doc-buffer-keywords (name &optional truncate)
  (interactive)
  (company-coq-dbg "company-coq-doc-buffer-keywords: Called for %s" name)
  (when (fboundp 'libxml-parse-html-region)
    (let* ((anchor         (company-coq-get-anchor name))
           (shr-target-id  (and anchor (concat "hevea_quickhelp" (int-to-string (cdr anchor)))))
           (doc-short-path (and anchor (concat (car anchor) ".html.gz")))
           (doc-full-path  (and doc-short-path
                                (concat (file-name-directory script-full-path) "refman/" doc-short-path))))
      (when doc-full-path
        (company-coq-with-clean-doc-buffer
          (company-coq-doc-keywords-put-html doc-full-path truncate)
          (cons (current-buffer) (point)))))))

(defun company-coq-candidates-symbols ()
  (interactive)
  (company-coq-dbg "company-coq-candidates-symbols: Called")
  (when (company-coq-init-symbols-or-defuns)
    (company-coq-complete-symbol-or-defun (company-coq-prefix-simple))))

(defun company-coq-candidates-keywords ()
  (interactive)
  (company-coq-dbg "company-coq-candidates-keywords: Called")
  (when (company-coq-init-keywords)
    (company-coq-complete-keyword (company-coq-prefix-simple))))

(defun company-coq-candidates-context ()
  (interactive)
  (company-coq-dbg "company-coq-candidates-context: Called")
  (company-coq-complete-context (company-coq-prefix-simple)))

(defun company-coq-candidates-modules ()
  (interactive)
  (company-coq-dbg "company-coq-candidates-modules: Called with prefix %s" (company-coq-prefix-module))
  (when (company-coq-init-modules)
    (company-coq-complete-modules (company-coq-prefix-module))))

(defun company-coq-candidates-block-end ()
  (interactive)
  (company-coq-dbg "company-coq-candidates-block-end: Called")
  (company-coq-complete-block-end (company-coq-prefix-block-end)))

(defun company-coq-candidates-reserved ()
  (interactive)
  (company-coq-dbg "company-coq-candidates-reserved-keywords: Called")
  (company-coq-complete-reserved (company-coq-prefix-simple)))

(defun company-coq-match (completion)
  (company-coq-dbg "company-coq-match: matching %s" completion)
  (get-text-property 0 'match-end completion))

(defun company-coq-dabbrev-to-yas-format-marker (match)
  (let* ((start      (match-beginning 1))
         (end        (match-end       1))
         (identifier (or (and start end (substring match start end))
                         (and company-coq-explicit-placeholders "_") "")))
    (concat  "${" identifier "}")))

(defun company-coq-dabbrev-to-yas (abbrev)
  (let* ((snippet (replace-regexp-in-string
                   company-coq-dabbrev-to-yas-regexp
                   #'company-coq-dabbrev-to-yas-format-marker abbrev)))
    (company-coq-dbg "company-coq-dabbrev-to-yas: transformed to %s" snippet)
    snippet))

(defconst company-coq-abbrevs-transforms-alist '((own . company-coq-dabbrev-to-yas)
                                                 (pg  . company-coq-dabbrev-to-yas)))

(defun company-coq-abbrev-to-yas (abbrev source)
  (company-coq-dbg "company-coq-abbrev-to-yas: Transforming %s" abbrev)
  (let ((transform (cdr-safe (assq source company-coq-abbrevs-transforms-alist))))
    (if transform (funcall transform abbrev)
      abbrev)))

(defun company-coq-get-snippet (candidate)
  (let* ((abbrev  (get-text-property 0 'insert candidate))
         (source  (company-coq-read-abbrev-source candidate))
         (snippet (and abbrev (company-coq-abbrev-to-yas abbrev source))))
    snippet))

(defun company-coq-post-completion-keyword (kwd)
  (let* ((found   (search-backward kwd))
         (start   (match-beginning 0))
         (end     (match-end 0))
         (snippet (company-coq-get-snippet kwd)))
    (when (and found start end snippet)
      (let ((insert-fun (get-text-property 0 'insert-fun kwd)))
        (if insert-fun
            (progn
              (delete-region start end)
              (funcall insert-fun))
          (yas-expand-snippet snippet start end))))))

(defun company-coq-goto-occurence (&optional _event)
  (interactive)
  (let ((pos (occur-mode-find-occurrence)))
    (switch-to-buffer (marker-buffer pos))
    (goto-char pos)
    (kill-buffer "*Occur*")))

(defun company-coq-occur ()
  (interactive)
  (let ((same-window-regexps '("\*Occur\*")))
    (occur company-coq-outline-regexp)
    (with-current-buffer "*Occur*"
      (let ((local-map (copy-keymap (current-local-map))))
        (substitute-key-definition #'occur-mode-goto-occurrence #'company-coq-goto-occurence local-map)
        (substitute-key-definition #'occur-mode-mouse-goto #'company-coq-goto-occurence local-map)
        (use-local-map local-map)))))

(defun company-coq-grep-symbol (regexp)
  "Find REGEXP in the current directory and subdirectories."
  (interactive
   (list (cond
          ((use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))
          (t
           (let ((symbol (company-coq-symbol-at-point)))
             (if (> (length symbol) 1) (regexp-quote symbol)
               (read-from-minibuffer "Regexp to look for: ")))))))
  (company-coq-dbg "company-coq-grep-symbol: Looking for [%s]" regexp)
  (grep-compute-defaults)
  (rgrep regexp "*.v" default-directory)
  (with-current-buffer next-error-last-buffer
    (let ((inhibit-read-only t))
      (save-excursion ;; Prettify buffer title
        (goto-char (point-min))
        (when (re-search-forward "\\`[^\0]*?find.*" (point-max) t)
          (replace-match (replace-quote (format "Searching for [%s] in [%s]\n" regexp default-directory)))
          (goto-char (point-min))
          (company-coq-make-title-line))))))

;; TODO It would be nice to get syntax coloring in the grep buffer

;; TODO this could work better by using information from show-paren-data-function

(defun company-coq-cant-fold-unfold ()
  (save-excursion
    (condition-case nil
        (progn (outline-back-to-heading) nil)
      ('error t))))

(defun company-coq-fold ()
  "Hide the body of the current proof or definition. When outside
a proof, or when repeated, hide the body of all proofs and
definitions."
  (interactive)
  (when outline-minor-mode
    (if (or (eq last-command #'company-coq-fold) (company-coq-cant-fold-unfold))
        (hide-body)
      (hide-subtree))))

(unless (plist-member (symbol-plist 'company-coq-fold) 'disabled)
  (put #'company-coq-fold 'disabled t))

(defun company-coq-unfold ()
  (interactive)
  (when outline-minor-mode
    (if (or (eq last-command #'company-coq-unfold) (company-coq-cant-fold-unfold))
        (show-all)
      (show-subtree))))

;; TODO completion at end of full symbol

(defun company-coq-symbols (command &optional arg &rest ignored)
  "A company-mode backend for known Coq symbols."
  (interactive (list 'interactive))
  (company-coq-dbg "symbols backend: called with command %s" command)
  (pcase command
    (`interactive (company-begin-backend 'company-coq-symbols))
    (`prefix (company-coq-prefix-simple))
    (`candidates (company-coq-candidates-symbols))
    (`sorted t)
    (`duplicates nil)
    (`ignore-case nil)
    (`meta (company-coq-meta-symbol arg))
    (`no-cache t)
    (`match (company-coq-match arg))
    (`annotation "<symb>")
    (`doc-buffer (company-coq-doc-buffer-symbols arg))
    (`comparison-fun #'company-coq-string-lessp-symbols)
    (`require-match 'never)))

(defun company-coq-keywords (command &optional arg &rest ignored)
  "A company-mode backend for Coq keywords."
  (interactive (list 'interactive))
  (company-coq-dbg "keywords backend: called with command %s" command)
  (pcase command
    (`interactive (company-begin-backend 'company-coq-keywords))
    (`prefix (company-coq-prefix-simple))
    (`candidates (company-coq-candidates-keywords))
    (`sorted t)
    (`duplicates nil)
    (`ignore-case nil)
    (`meta (company-coq-meta-keyword arg))
    (`no-cache t)
    (`match (company-coq-match arg))
    (`annotation (company-coq-annotation-keywords arg))
    (`post-completion (company-coq-post-completion-keyword arg))
    (`doc-buffer (car (company-coq-doc-buffer-keywords arg t)))
    (`location (company-coq-doc-buffer-keywords arg nil)) ;; TODO
    (`comparison-fun #'company-coq-string-lessp-keywords)
    (`require-match 'never)))

(defun company-coq-context (command &optional arg &rest ignored)
  "A company-mode backend for identifiers grabbed from the current proof context."
  (interactive (list 'interactive))
  (company-coq-dbg "context backend: called with command %s" command)
  (pcase command
    (`interactive (company-begin-backend 'company-coq-context))
    (`prefix (company-coq-prefix-simple))
    (`candidates (company-coq-candidates-context))
    (`sorted t)
    (`duplicates nil)
    (`ignore-case nil)
    (`meta (company-coq-meta-simple arg))
    (`no-cache t)
    ;; (`match (company-coq-match arg))
    (`annotation (company-coq-annotation-context arg))
    ;; (`doc-buffer (car (company-coq-doc-buffer-keywords arg t)))
    ;; (`location (company-coq-doc-buffer-keywords arg nil)) ;; TODO
    (`comparison-fun #'company-coq-string-lessp-symbols)
    (`require-match 'never)))

(defun company-coq-modules (command &optional arg &rest ignored)
  "A company-mode backend for Coq modules."
  (interactive (list 'interactive))
  (company-coq-dbg "modules backend: called with command %s" command)
  (pcase command
    (`interactive (company-begin-backend 'company-coq-modules))
    (`prefix (company-coq-prefix-module)) ;; FIXME Completion at beginning of hole
    (`candidates (company-coq-candidates-modules))
    (`sorted t)
    (`duplicates nil)
    (`ignore-case nil)
    (`meta (company-coq-meta-simple arg))
    (`location (company-coq-location-simple arg))
    (`no-cache t)
    (`match (company-coq-match arg))
    (`require-match 'never)))

(defun company-coq-block-end (command &optional _arg &rest ignored)
  "A company-mode backend for the end of Sections and Chapters."
  (interactive (list 'interactive))
  (company-coq-dbg "section end backend: called with command %s" command)
  (pcase command
    (`interactive (company-begin-backend 'company-coq-block-end))
    (`prefix (company-coq-prefix-block-end))
    (`candidates (company-coq-candidates-block-end))
    (`sorted t)
    (`duplicates nil)
    (`ignore-case nil)
    (`require-match 'never)))


(defun company-coq-reserved-keywords (command &optional _arg &rest ignored)
  "A company-mode backend for language keywords, to prevent completion from kicking in instead of newline."
  (interactive (list 'interactive))
  (company-coq-dbg "reserved keywords backend: called with command %s" command)
  (pcase command
    (`interactive (company-begin-backend 'company-coq-reserved-keywords))
    (`prefix (company-coq-prefix-simple))
    (`candidates (company-coq-candidates-reserved))
    (`post-completion (newline 1 t))
    (`annotation "<reserved>")
    (`sorted t)
    (`duplicates nil)
    (`ignore-case nil)
    (`require-match 'never)))

(defun company-coq-make-backends-alist ()
  (mapcar (lambda (backend) (cons backend ()))
          (append '(nil) company-coq-sorted-backends)))

(defun company-coq-push-to-backend-alist (candidate backends-alist)
  (let* ((company-tag   (get-text-property 0 'company-backend candidate))
         (tag-container (or (assq company-tag backends-alist)
                            (assq nil         backends-alist))))
    (push candidate (cdr tag-container))))

(defun company-coq-sort-in-backends-order (candidates)
  (let ((backends-alist (company-coq-make-backends-alist)))
    (mapc (lambda (candidate) ;; Partition the candidates by backend
            (company-coq-push-to-backend-alist candidate backends-alist))
          candidates)
    (apply #'append
           (mapcar (lambda (pair) ;; Sort the results of each backends, and concat all
                     (cl-stable-sort (cdr pair) (or (and (car pair) (funcall (car pair) 'comparison-fun))
                                                    #'company-coq-string-lessp-foldcase)))
                   backends-alist))))

(defun company-coq-setup-hooks () ;; NOTE: This could be made callable at the beginning of every completion.
  ;; PG hooks
  ;; (add-hook 'proof-state-change-hook (lambda () (message "STATE CHANGE")))
  (add-hook 'proof-shell-insert-hook ;; (lambda () (message "INSERT")))
            'company-coq-maybe-proof-input-reload-things)
  (add-hook 'proof-shell-handle-delayed-output-hook ;; (lambda () (message "DELAYED OUTPUT")))
            'company-coq-maybe-proof-output-reload-things)
  (add-hook 'proof-shell-handle-error-or-interrupt-hook ;; (lambda () (message "ERROR OR INTERRUPT")))
            'company-coq-maybe-reload-context))

(defun company-coq-setup-optional-backends ()
  (when company-coq-autocomplete-context
    (add-to-list 'company-coq-backends #'company-coq-context t))

  (when company-coq-autocomplete-modules
    (add-to-list 'company-coq-backends #'company-coq-modules t))

  (when company-coq-autocomplete-symbols
    (add-to-list 'company-coq-backends #'company-coq-symbols t))

  (when company-coq-autocomplete-block-end
    (add-to-list 'company-coq-backends #'company-coq-block-end t))

  ;; Symbols backend
  (when (and company-coq-autocomplete-symbols-dynamic (not company-coq-fast))
    (message "Warning: Symbols autocompletion is an experimental
    feature. Performance won't be good unless you use a patched
    coqtop. If you do, set company-coq-fast to true.")))

(defvar company-coq-electric-exit-characters '(?\; ?.)
  "Characters that exist the current snippet.")

;; FIXME this should only happend in the last hole, and only if not in
;; nested parens, so as to prevent [assert true by (blah;] from
;; exiting.
(defun company-coq-maybe-exit-snippet ()
  (interactive)
  (let* ((after-exit-char (member (char-before (point)) company-coq-electric-exit-characters))
	 (snippet         (and after-exit-char (car-safe (yas--snippets-at-point)))))
    (self-insert-command 1)
    (when snippet
      (yas-exit-snippet snippet))))

(defun company-coq-proof-goto-point (&rest args)
  (interactive)
  (when (bound-and-true-p company-mode)
    (company-abort))
  (apply #'proof-goto-point args))

(defmacro company-coq-repeat-until-fixpoint-or-scan-error (body retform)
  `(save-excursion
     (condition-case nil
         (let ((prev-point nil))
           (while (not (equal prev-point (point)))
             (setq prev-point (point))
             ,body))
       (scan-error nil))
     ,retform))

(defun company-coq-beginning-of-proof ()
  (interactive)
  (company-coq-repeat-until-fixpoint-or-scan-error
   (smie-backward-sexp-command 1) (point-at-bol)))

(defun company-coq-end-of-proof ()
  (interactive)
  (company-coq-repeat-until-fixpoint-or-scan-error
   (smie-forward-sexp-command 1) (point-at-eol)))

(defun company-coq-narrow-to-defun ()
  (interactive)
  (narrow-to-region (company-coq-beginning-of-proof) (company-coq-end-of-proof)))

(defvar company-coq-map
  (let ((cc-map (make-sparse-keymap)))
    (define-key cc-map (kbd "C-c C-/")          #'company-coq-fold)
    (define-key cc-map (kbd "C-c C-\\")         #'company-coq-unfold)
    (define-key cc-map (kbd "C-c C-,")          #'company-coq-occur)
    (define-key cc-map (kbd "C-c C-&")          #'company-coq-grep-symbol)
    (define-key cc-map (kbd "C-<return>")       #'company-manual-begin)
    (define-key cc-map (kbd "SPC")              #'company-coq-maybe-exit-snippet)
    ;; (define-key cc-map (kbd "RET")           #'company-coq-maybe-exit-snippet) ;; FIXME too invasive
    (define-key cc-map [remap proof-goto-point] #'company-coq-proof-goto-point)
    (define-key cc-map [remap narrow-to-defun]  #'company-coq-narrow-to-defun) ;; FIXME handle sections properly
    cc-map)
  "Keymap for company-coq keybindings")

(define-minor-mode company-coq--keybindings-minor-mode
  "Minor mode to provide company-coq keybindings."
  :lighter nil
  :keymap company-coq-map)

(defun company-coq-setup-keybindings ()
  (company-coq--keybindings-minor-mode))

;;;###autoload
(defun company-coq-initialize () ;; TODO this could be a minor mode
  (interactive)
  (when (not (company-coq-in-coq-mode))
    (error "company-coq only works with coq-mode."))

  ;; Enable relevant minor modes
  (company-mode 1)
  (yas-minor-mode 1)
  (show-paren-mode 1)
  (outline-minor-mode 1)

  ;; Set a few company settings
  (set (make-local-variable 'company-idle-delay) 0)
  (set (make-local-variable 'company-tooltip-align-annotations) t)
  (set (make-local-variable 'company-abort-manual-when-too-short) t)

  ;; And a few outline settings
  (set (make-local-variable 'outline-level) #'company-coq-outline-level)
  (set (make-local-variable 'outline-regexp) company-coq-outline-regexp)
  (set (make-local-variable 'outline-heading-end-regexp) company-coq-outline-heading-end-regexp)

  ;; Load keywords
  (company-coq-init-keywords)

  ;; Setup hooks and extra backends
  (company-coq-setup-hooks)
  (company-coq-setup-optional-backends)

  ;; Let company know about our backends
  (add-to-list (make-local-variable 'company-backends) company-coq-backends)
  (add-to-list (make-local-variable 'company-transformers) #'company-coq-sort-in-backends-order)

  ;; Set up a few convenient key bindings
  (company-coq-setup-keybindings))

(defun company-coq-unload-function ()
  (when (featurep 'company-coq-abbrev)
    (unload-feature 'company-coq-abbrev t))

  (remove-hook 'proof-shell-insert-hook #'company-coq-maybe-proof-input-reload-things)
  (remove-hook 'proof-shell-handle-delayed-output-hook #'company-coq-maybe-proof-output-reload-things)
  (remove-hook 'proof-shell-handle-error-or-interrupt-hook #'company-coq-maybe-reload-context)

  (setq company-backends     (delete company-coq-backends company-backends))
  (setq company-transformers (delete #'company-coq-sort-in-backends-order company-transformers))

  (cl-loop for buffer in (buffer-list)
           do (with-current-buffer buffer
                (company-coq--keybindings-minor-mode -1)))

  nil)

(defun toggle-company-coq-debug ()
  "Toggles company-coq-debug. When on, prints debug messages during operation."
  (interactive)
  (setq company-coq-debug (not company-coq-debug))
  (message "company-coq-debug: %s" company-coq-debug))

;; TODO add a binding to look up the word at point

(provide 'company-coq)
;;; company-coq.el ends here
