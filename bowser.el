(defvar bowser-mode-hook nil)

(defvar bowser-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<return>") 'bowser-open)
    (define-key map (kbd "M-p") 'bowser-paste)
    (define-key map (kbd "M-y") 'bowser-copy)
    (define-key map (kbd "M-d") 'bowser-cut)
    (define-key map (kbd "M-n") 'bowser-new-directory)
    (define-key map (kbd "M-m") 'bowser-toggle-mark)
    (define-key map (kbd "M-x") 'bowser-delete)
    (define-key map (kbd "<backspace>") 'bowser-ascend)
    (define-key map (kbd "M-h") 'bowser-toggle-hidden)
    (define-key map (kbd "M-j") 'bowser-jump)
    (define-key map (kbd "M-o") 'bowser-open-with)
    (define-key map (kbd "M-r") 'bowser-rename)
    (define-key map (kbd "M-b") 'bowser-create-bookmark)
    (define-key map (kbd "M-B") 'bowser-open-bookmark)
    (define-key map (kbd "M-t") 'bowser-open-terminal)
    map)
  "Keymap for bowser")

(defun bowser-ascend ()
  "Move to the parent directory"

  (interactive)
  (setq bowser-directory (file-name-directory (directory-file-name bowser-directory)))
  (setq bowser-marked '())
  (bowser-refresh 1))

(defun bowser-copy ()
  "Add marked files to the copy list"

  (interactive)
  (setq bowser-copied-files bowser-marked)
  (setq bowser-marked '())
  (setq bowser-cut-files '()))

(defun bowser-create-bookmark ()
  "Add the current directory to the bookmarks list"

  (interactive)
  (add-to-list 'bowser-bookmarks bowser-directory))

(defun bowser-cut ()
  "Add marked files to the cut list"

  (interactive)
  (setq bowser-cut-files bowser-marked)
  (setq bowser-marked '())
  (setq bowser-copied-files '()))

(defun bowser-delete ()
  "Delete the marked file(s), with prompt"

  (interactive)
  (when (y-or-n-p "Are you sure you want to delete the marked files?")
    (dolist (file bowser-marked)
      (if (string= (substring file -1) "/")
	  (start-process "" nil "rm" "-r" "-v" file)
	(start-process "" nil "rm" "-v" file))))

  ;; TODO: Make process list auto revert / refresh
  (when (> (length (process-list)) 1)
    (list-processes))

  ;; TODO: Wait until the above processes are completed to refresh
  (bowser-refresh (line-number-at-pos)))

(defun bowser-jump ()
  "Fuzzy search all the directories under the home folder"

  (interactive)
  (let ((find-command (concat "find ~ -type d"))
	(find-output nil))
    (setq find-output (shell-command-to-string find-command))
    (setq find-output (split-string find-output "\n"))
    (setq find-output (remove "" find-output))
    (setq bowser-directory (completing-read "Choose a directory: " find-output))
    (setq bowser-directory (concat bowser-directory "/"))
    (bowser-refresh 1)))

(defun bowser-new-directory ()
  "Create a under directory under the current one"

  (interactive)
  (let ((new-directory nil))
    (setq new-directory (read-string "Enter an name for the new directory: "))
    (setq new-directory (concat bowser-directory new-directory))
    (start-process "" nil "mkdir" new-directory))
  (bowser-refresh (line-number-at-pos)))

(defun bowser-open ()
  "Open the selected file or directory"

  (interactive)

  (let ((selected-file (thing-at-point 'line t))
	(last-character nil)
	(extension nil))

    ;; get the full path of the selected file
    (setq selected-file (replace-regexp-in-string "\n" "" selected-file))
    (setq selected-file (concat bowser-directory selected-file))

    ;; get the extension and last character from file
    (setq last-character (substring selected-file -1))
    (setq extension (file-name-extension selected-file))

    (when (string= last-character "/")
      (setq bowser-directory selected-file)
      (setq bowser-marked '())
      (bowser-refresh 1))

    (when (not (string= last-character "/"))
      (start-process "" nil "mimeopen" selected-file))))

(defun bowser-open-bookmark ()
  "Open a bookmarked directory"

  (interactive)
  (setq bowser-directory (completing-read "Choose a bookmark: " bowser-bookmarks))
  (bowser-refresh 1))

(defun bowser-open-terminal ()
  "Open a terminal in the present directory"

  (interactive)
  (start-process "" nil "urxvt" "-cd" bowser-directory))

(defun bowser-open-with ()
  "Open the selected file with a specified application"

  (interactive)
  (let ((selected-file (thing-at-point 'line t))
	(application nil))
    (setq selected-file (replace-regexp-in-string "\n" "" selected-file))
    (setq selected-file (concat bowser-directory selected-file))
    (setq application (read-string "Enter an application: "))
    (start-process "" nil application selected-file)))

(defun bowser-paste ()
  "Paste copied or cut files to current directory"

  (interactive)

  (dolist (file bowser-copied-files)
    (if (string= (substring file -1) "/")
	(start-process "" nil "cp" "-v" "-r" "-n" file bowser-directory)
      (start-process "" nil "cp" "-n" "-v" file bowser-directory)))

  (dolist (file bowser-cut-files)
    (if (string= (substring file -1) "/")
	(start-process "" nil "mv" "-v" "-n" file bowser-directory)
      (start-process "" nil "mv" "-n" file bowser-directory)))
  (setq bowser-cut-files '())

  ;; TODO: make the process list auto refresh/revert
  (when (> (length (process-list)) 1)
    (list-processes))

  ;; TODO: Wait until the above processes are completed to refresh
  (bowser-refresh (line-number-at-pos)))

(defun bowser-refresh (line-to-go-to)
  "Refresh the directory"

  (interactive)
  (erase-buffer)
  (call-process "ls" nil t nil bowser-hidden bowser-sort "-p" "--group-directories-first" bowser-directory)
  (setq header-line-format bowser-directory)

  ;; color directories---shouldn't have to unhighlight and rehighlight like this
  (unhighlight-regexp ".*\/")
  (highlight-regexp ".*\/" "font-lock-function-name-face")

  (goto-line line-to-go-to))

(defun bowser-rename ()
  "Rename the selected file"

  (interactive)
  (let ((selected-file (thing-at-point 'line t))
	(new-name nil))

    (interactive)
    (setq selected-file (replace-regexp-in-string "\n" "" selected-file))
    (setq new-name (read-string "New name: " selected-file))
    (setq selected-file (concat bowser-directory selected-file))
    (setq new-name (concat bowser-directory new-name))
    (start-process "" nil "mv" "-n" selected-file new-name)
    (bowser-refresh (line-number-at-pos))))

(defun bowser-sort-change ()
  "Change the order of the listing"

  (interactive)
  (setq bowser-sort (read-string "Sort by name (default), size (-S), modified (-t) , accessed (-u), or extension (-X):"))
  (when (not (member bowser-sort '("-S" "-t" "-u" "-X")))
    (setq bowser-sort "-1"))
  (bowser-refresh 1))

(defun bowser-toggle-hidden ()
  "Toggle hidden files"

  (interactive)
  (if (string= bowser-hidden "-A")
      (setq bowser-hidden "-1")
    (setq bowser-hidden "-A"))
  (bowser-refresh 1))

(defun bowser-toggle-mark ()
  "Mark files for copying, moving, or deleting"

  (interactive)
  (let ((selected-file-name (thing-at-point 'line t)))

    ;; get full path of highlighted file
    (setq selected-file-name (replace-regexp-in-string "\n" "" selected-file-name))
    (setq selected-file-path (concat bowser-directory selected-file-name))

    ;; mark or unmark the file
    (if (string= (substring selected-file-name 0 2) "  ")
	(progn (setq bowser-marked (remove selected-file-path bowser-marked))
	       (end-of-line)
	       (if (re-search-backward "^  " nil t)
		   (replace-match "")))
      (progn (add-to-list 'bowser-marked selected-file-path)
	     (if (re-search-backward "^" nil t)
		 (replace-match "  "))))
    (next-line)))

(defun bowser-mode ()
  "A simple file browser"

  (switch-to-buffer "bowser")
  (interactive)
  (kill-all-local-variables)
  (use-local-map bowser-mode-map)
  (setq bowser-home (concat "/home/" (user-login-name) "/"))
  (setq bowser-directory bowser-home)
  (setq bowser-hidden "-1")
  (setq bowser-sort "-1")
  (setq bowser-marked '())
  (setq bowser-bookmarks '())
  (setq header-line-format bowser-directory)
  (bowser-refresh 1)
  (setq major-mode 'bowser-mode)
  (setq mode-name "bowser")
  (run-hooks 'bowser-mode-hook)
  (linum-mode)
  (hl-line-mode))

(provide 'bowser-mode)
