;;; Grep is wicked

;; Grep/Find.  This needs some cleanup
(setq nomosyn-find-filter "-type f -not \\( -name '*~' -or -name '*#' -or -path '*.meteor/*' -or -name '#*' -or -name '*.log' -or -path '*CVS/*' -or -path '*.svn/*' -or -path '*.git/*' -or -path '*vendor/*' -or -path '*build/*' \\) ")

(setq grep-command "grep -IrinEe ")
(setq grep-find-command
      (format (concat "%s . " nomosyn-find-filter "-print0 | xargs -0 %s")
        find-program grep-command))
