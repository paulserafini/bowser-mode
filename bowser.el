(defvar bowser-mode-hook nil)

(defvar bowser-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<return>") 'bowser-open)
    (define-key map (kbd "M-p") 'bowser-paste)
    (define-key map (kbd "M-c") 'bowser-copy)
    (define-key map (kbd "M-m") 'bowser-mark)
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
  (setq selection (thing-at-point 'line t))
  (setq selection (replace-regexp-in-string "\n" "" selection))
  (setq selection (concat current-directory selection))
  (setq application (read-string "Application:"))
  (start-process "" nil application selection))

(defun bowser-open ()
  "Open the selected file or directory"

  (interactive)

  ;; get the full path of the selected file
  (setq selection (thing-at-point 'line t))
  (setq selection (replace-regexp-in-string "\n" "" selection))
  (setq selection (concat current-directory selection))

  ;; get the extension and last character from file
  (setq last-character (substring selection -1))
  (setq extension (file-name-extension selection))

  ;; define file associations
  (setq images '("jpeg" "jpg" "png"))
  (setq videos '("avi" "mkv" "mp4"))
  (setq text '("csv" "el" "epub" "org" "R" "sh" "tex" "txt"))

  (when (string= last-character "/")
    (setq current-directory selection)
    (bowser-refresh))

  (when (string= extension "pdf")
    (start-process "" nil "zathura" selection))

  (when (member extension text)
    (find-file-other-window selection))

  (when (member extension images)
    (start-process "" nil "sxiv" selection))

  (when (member extension videos)
    (start-process "" nil "mpv" selection)))

(defun bowser-delete ()
  "Delete the marked file(s), with prompt"

  (interactive)
  (if (y-or-n-p "Are you sure you want to delete the marked files?")
      (dolist (file marked)
	(if (string= (substring file -1) "/")
	    (start-process "" nil "rm" "-r" file)
	    (start-process "" nil "rm" file))))
  (bowser-refresh))

(defun bowser-refresh ()
  "Refresh the directory"

  (interactive)
  (erase-buffer)
  (call-process "ls" nil t nil hidden-variable "-p" "--group-directories-first" current-directory)

  ;; color directories---shouldn't have to unhighlight and rehighlight like this
  (unhighlight-regexp ".*\/")
  (highlight-regexp ".*\/" "font-lock-function-name-face")

  (goto-char (point-min)))

(defun bowser-mark ()
  "Mark files for copying, moving, or deleting"

  (interactive)
  (setq marked-name (thing-at-point 'line t))
  (setq marked-name (replace-regexp-in-string "\n" "" marked-name))
  (setq marked-path (concat current-directory marked-name))
  (if (string= (substring marked-name 0 2) "  ")
      (progn (setq marked (remove marked-path marked))
	     (if (re-search-backward "^  " nil t)
		 (replace-match "")))
    (progn (add-to-list 'marked marked-path)
	   (if (re-search-backward "^" nil t)
	       (replace-match "  ")))))

(defun bowser-copy ()
  "Add marked files to the copy list"

  (interactive)
  (setq bowser-cp-list marked)
  (setq marked '())
  (setq bowser-mv-list '()))

(defun bowser-cut ()
  "Add marked files to the cut list"

  (interactive)
  (setq bowser-mv-list marked)
  (setq marked '())
  (setq bowser-cp-list '()))

(defun bowser-paste ()
  "Paste copied or cut files to current directory"

  (interactive)
  (dolist (file bowser-cp-list)
  (start-process "" nil "cp" file current-directory))
  (dolist (file bowser-mv-list)
  (start-process "" nil "mv" file current-directory))
  (setq bowser-mv-list '())
  (bowser-refresh))

(defun bowser-rename ()
  "Rename the selected file"

  (interactive)
  (setq selection (thing-at-point 'line t))
  (setq selection (replace-regexp-in-string "\n" "" selection))
  (setq new-name (read-string "New name: " selection))
  (setq selection (concat current-directory selection))
  (setq new-name (concat current-directory new-name))
  (start-process "" nil "mv" selection new-name)
  (bowser-refresh))

(defun bowser-ascend ()
  "Move to the parent directory"

  (interactive)
  (setq current-directory (file-name-directory (directory-file-name current-directory)))
  (bowser-refresh))

(defun bowser-toggle-hidden ()
  "Toggle hidden files"

  (interactive)
  (if (string= hidden-variable "-A")
      (setq hidden-variable "-1")
      (setq hidden-variable "-A"))
  (bowser-refresh))

(defun bowser-jump ()
  "Fuzzy search all the directories under the home folder"

  (interactive)
  (setq find-command (concat "find " home-directory "  -type d"))
  (setq output (shell-command-to-string find-command))
  (setq output (split-string output "\n"))
  (setq output (remove "" output))
  (setq current-directory (completing-read "File: " output))
  (setq current-directory (concat current-directory "/"))
  (bowser-refresh))

(defun bowser-open-bookmark ()
  "Open a bookmarked directory"

  (interactive)
  (setq current-directory (completing-read "File: " bookmarks))
  (setq current-directory (concat current-directory "/"))
  (bowser-refresh))

(defun bowser-create-bookmark ()
  "Add the current directory to the bookmarks list"

  (interactive)
  (add-to-list 'bookmarks current-directory))


(defun bowser-mode ()
  "A simple file browser"

  (switch-to-buffer "bowser")
  (interactive)
  (kill-all-local-variables)
  (use-local-map bowser-mode-map)
  (setq home-directory (concat "/home/" (user-login-name) "/"))
  (setq current-directory home-directory)
  (setq hidden-variable "-1")
  (setq marked '())
  (setq bookmarks '())
  (bowser-refresh)
  (setq major-mode 'bowser-mode)
  (setq mode-name "bowser")
  (run-hooks 'bowser-mode-hook)
  (linum-mode)
  (hl-line-mode))

(provide 'bowser-mode)
