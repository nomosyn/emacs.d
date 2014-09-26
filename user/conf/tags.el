(add-to-list 'load-path user-tools-ctags)

(setq path-to-ctags user-tools-ctags-bin-path)
 (defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name (directory-file-name dir-name))))
