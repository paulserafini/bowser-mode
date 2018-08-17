(defvar bowser-mode-hook nil)

(defvar bowser-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<return>") 'open-selection)
    (define-key map (kbd "M-p") 'bowser-mode-cp)
    (define-key map (kbd "M-m") 'bowser-mode-mark)
    (define-key map (kbd "M-x") 'bowser-mode-delete)
    (define-key map (kbd "M-h") 'bowser-mode-ascend)
    (define-key map (kbd "M-e") 'bowser-mode-show-hidden)
    (define-key map (kbd "M-j") 'bowser-mode-jump)
    (define-key map (kbd "M-j") 'bowser-mode-jump)
    (define-key map (kbd "M-o") 'bowser-open-with)
    map)
  "Keymap for bowser")

(defun bowser-open-with ()
  (interactive)
  (setq selection (thing-at-point 'line t))
  (setq selection (replace-regexp-in-string "\n" "" selection))
  (setq selection (concat current-directory selection))
  (setq application (read-string "Application:"))
  (start-process "" nil application selection))


(defun open-selection ()
  "Open the selected file or directory"

  (interactive)

  ;; make variables other than current-directory local

  (setq selection (thing-at-point 'line t))
  (setq selection (replace-regexp-in-string "\n" "" selection))
  (setq selection (concat current-directory selection))

  ;; get possible extensions from the selected file/directory
  (setq last-character (substring selection -1))
  (setq extension (file-name-extension selection))

  ;; setup file associations
  (setq images '("jpeg" "jpg" "png"))
  (setq videos '("avi" "mkv" "mp4"))
  (setq text '("csv" "el" "org" "R" "sh" "tex" "txt"))

  (when (string= last-character "/")
    (setq current-directory selection)
    (bowser-mode-refresh))

  (when (string= extension "pdf")
    (start-process "" nil "zathura" selection))

  (when (member extension text)
    (find-file-other-window selection))

  (when (member extension images)
    (start-process "" nil "sxiv" selection))

  (when (member extension videos)
    (start-process "" nil "mpv" selection)))

(defun bowser-mode-delete ()
  (interactive)
  (setq selection (thing-at-point 'line t))
  (setq selection (replace-regexp-in-string "\n" "" selection))
  (setq selection (concat current-directory selection))
  (start-process "" nil "rm" selection)
  (bowser-mode-refresh))

(defun bowser-mode-refresh ()
  (interactive)
  (erase-buffer)
  (call-process "ls" nil t nil hidden-variable "-p" "--group-directories-first" current-directory)
  (unhighlight-regexp ".*\/")
  (highlight-regexp ".*\/" "font-lock-function-name-face")
  (goto-char (point-min)))

(defun bowser-mode-mark ()
  (interactive)
  (setq marked-name (thing-at-point 'line t))
  (setq marked-name (replace-regexp-in-string "\n" "" marked-name))
  (setq marked-path (concat current-directory marked-name)))

(defun bowser-mode-cp ()
  (interactive)
  (setq target-path (concat current-directory marked-name))
  (start-process "" nil "cp" marked-path target-path)
  (bowser-mode-refresh))

(defun bowser-mode-mv ()
  (interactive)
  (setq target-path (concat current-directory marked-name))
  (start-process "" nil "mv" marked-path target-path)
  (bowser-mode-refresh))

(defun bowser-mode-ascend ()
  (interactive)
  (setq current-directory (file-name-directory (directory-file-name current-directory)))
  (bowser-mode-refresh))

(defun bowser-mode-show-hidden ()
  (interactive)
  (if (string= hidden-variable "-A")
      (setq hidden-variable "-1")
      (setq hidden-variable "-A"))
  (bowser-mode-refresh))

(defun bowser-mode-jump ()
  (interactive)
  (setq find-command (concat "find " home-directory "  -type d"))
  (setq output (shell-command-to-string find-command))
  (setq output (split-string output "\n"))
  (setq output (remove "" output))
  (setq current-directory (completing-read "File: " output))
  (setq current-directory (concat current-directory "/"))
  (bowser-mode-refresh))

(defun bowser-mode ()
  "A simple file browser"
  (switch-to-buffer "bowser")
  (interactive)
  (kill-all-local-variables)
  (use-local-map bowser-mode-map)
  (setq home-directory (concat "/home/" (user-login-name) "/"))
  (setq current-directory home-directory)
  (setq hidden-variable "-1")
  (bowser-mode-refresh)
  (setq major-mode 'bowser-mode)
  (setq mode-name "bowser")
  (run-hooks 'bowser-mode-hook)
  (linum-mode)
  (hl-line-mode))

(provide 'bowser-mode)
