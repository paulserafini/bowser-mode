(defvar bowser-mode-hook nil)

(defvar bowser-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<return>") 'bowser-open)
    (define-key map (kbd "M-p") 'bowser-paste)
    (define-key map (kbd "M-c") 'bowser-copy)
    (define-key map (kbd "M-m") 'bowser-toggle-mark)
    (define-key map (kbd "M-x") 'bowser-delete)
    (define-key map (kbd "<backspace>") 'bowser-ascend)
    (define-key map (kbd "M-h") 'bowser-toggle-hidden)
    (define-key map (kbd "M-j") 'bowser-jump)
    (define-key map (kbd "M-o") 'bowser-open-with)
    (define-key map (kbd "M-r") 'bowser-rename)
    (define-key map (kbd "M-b") 'bowser-create-bookmark)
    (define-key map (kbd "M-B") 'bowser-open-bookmark)
    map)
  "Keymap for bowser")

(defun bowser-open-with ()
  "Open the selected file with a specified application"

  (interactive)
  (let ((selected-file (thing-at-point 'line t))
	(application nil))
  (setq selected-file (replace-regexp-in-string "\n" "" selected-file))
  (setq selected-file (concat bowser-directory selected-file))
  (setq application (read-string "Enter an application: "))
  (start-process "" nil application selected-file)))

(defun bowser-open ()
  "Open the selected file or directory"

  (interactive)

  (let ((selected-file (thing-at-point 'line t))
	(last-character nil)
	(extension nil)
	(images '("jpeg" "jpg" "png"))
	(text '("csv" "el" "epub" "org" "R" "sh" "tex" "txt"))
	(videos '("avi" "mkv" "mp4")))

  ;; get the full path of the selected file
  (setq selected-file (replace-regexp-in-string "\n" "" selected-file))
  (setq selected-file (concat bowser-directory selected-file))

  ;; get the extension and last character from file
  (setq last-character (substring selected-file -1))
  (setq extension (file-name-extension selected-file))

  (when (string= last-character "/")
    (setq bowser-directory selected-file)
    (bowser-refresh))

  (when (string= extension "pdf")
    (start-process "" nil "zathura" selected-file))

  (when (member extension text)
    (find-file-other-window selected-file))

  (when (member extension images)
    (start-process "" nil "sxiv" selected-file))

  (when (member extension videos)
    (start-process "" nil "mpv" selected-file))))

(defun bowser-delete ()
  "Delete the marked file(s), with prompt"

  (interactive)
  (if (y-or-n-p "Are you sure you want to delete the marked files?")
      (dolist (file bowser-marked)
	(if (string= (substring file -1) "/")
	    (start-process "" nil "rm" "-r" file)
	    (start-process "" nil "rm" file))))
  (bowser-refresh))

(defun bowser-refresh ()
  "Refresh the directory"

  (interactive)
  (erase-buffer)
  (call-process "ls" nil t nil bowser-hidden-switch "-p" "--group-directories-first" bowser-directory)

  ;; color directories---shouldn't have to unhighlight and rehighlight like this
  (unhighlight-regexp ".*\/")
  (highlight-regexp ".*\/" "font-lock-function-name-face")

  (goto-char (point-min)))

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
	     (if (re-search-backward "^  " nil t)
		 (replace-match "")))
    (progn (add-to-list 'bowser-marked selected-file-path)
	   (if (re-search-backward "^" nil t)
	       (replace-match "  "))))))

(defun bowser-copy ()
  "Add marked files to the copy list"

  (interactive)
  (setq bowser-copied-files bowser-marked)
  (setq bowser-marked '())
  (setq bowser-cut-files '()))

(defun bowser-cut ()
  "Add marked files to the cut list"

  (interactive)
  (setq bowser-cut-files bowser-marked)
  (setq bowser-marked '())
  (setq bowser-copied-files '()))

(defun bowser-paste ()
  "Paste copied or cut files to current directory"

  (interactive)
  (dolist (file bowser-copied-files)
  (start-process "" nil "cp" file bowser-directory))
  (dolist (file bowser-cut-files)
  (start-process "" nil "mv" file bowser-directory))
  (setq bowser-cut-files '())
  (bowser-refresh))

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
  (start-process "" nil "mv" selected-file new-name)
  (bowser-refresh)))

(defun bowser-ascend ()
  "Move to the parent directory"

  (interactive)
  (setq bowser-directory (file-name-directory (directory-file-name bowser-directory)))
  (bowser-refresh))

(defun bowser-toggle-hidden ()
  "Toggle hidden files"

  (interactive)
  (if (string= bowser-hidden-switch "-A")
      (setq bowser-hidden-switch "-1")
      (setq bowser-hidden-switch "-A"))
  (bowser-refresh))

(defun bowser-jump ()
  "Fuzzy search all the directories under the home folder"

  (interactive)
  (let ((find-commant (concat "find " bowser-home "  -type d"))
	(find-output nil))
  (setq find-command (concat "find " bowser-home "  -type d"))
  (setq find-output (shell-command-to-string find-command))
  (setq find-output (split-string find-output "\n"))
  (setq find-output (remove "" find-output))
  (setq bowser-directory (completing-read "File: " find-output))
  (setq bowser-directory (concat bowser-directory "/"))
  (bowser-refresh)))

(defun bowser-open-bookmark ()
  "Open a bookmarked directory"

  (interactive)
  (setq bowser-directory (completing-read "File: " bowser-bookmarks))
  (setq bowser-directory (concat bowser-directory "/"))
  (bowser-refresh))

(defun bowser-create-bookmark ()
  "Add the current directory to the bookmarks list"

  (interactive)
  (add-to-list 'bowser-bookmarks bowser-directory))


(defun bowser-mode ()
  "A simple file browser"

  (switch-to-buffer "bowser")
  (interactive)
  (kill-all-local-variables)
  (use-local-map bowser-mode-map)
  (setq bowser-home (concat "/home/" (user-login-name) "/"))
  (setq bowser-directory bowser-home)
  (setq bowser-hidden-switch "-1")
  (setq bowser-marked '())
  (setq bowser-bookmarks '())
  (bowser-refresh)
  (setq major-mode 'bowser-mode)
  (setq mode-name "bowser")
  (run-hooks 'bowser-mode-hook)
  (linum-mode)
  (hl-line-mode))

(provide 'bowser-mode)
