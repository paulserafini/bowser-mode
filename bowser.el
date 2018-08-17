(defvar bowser-mode-hook nil)

(defvar bowser-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<return>") 'open-selection)
    (define-key map (kbd "M-p") 'bowser-mode-cp)
    (define-key map (kbd "M-m") 'bowser-mode-mark)
    (define-key map (kbd "M-x") 'bowser-mode-delete)
    map)
  "Keymap for bowser")

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
    (erase-buffer)
    (call-process "ls" nil t nil "-a" "-p" "--group-directories-first" current-directory))

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
  (erase-buffer)
  (call-process "ls" nil t nil "-a" "-p" "--group-directories-first" current-directory))

(defun bowser-mode-mark ()
  (interactive)
  (setq marked-name (thing-at-point 'line t))
  (setq marked-name (replace-regexp-in-string "\n" "" marked-name))
  (setq marked-path (concat current-directory marked-name)))

(defun bowser-mode-cp ()
  (interactive)
  (setq target-path (concat current-directory marked-name))
  (start-process "" nil "cp" marked-path target-path)
  (erase-buffer)
  (call-process "ls" nil t nil "-a" "-p" "--group-directories-first" current-directory))

(defun bowser-mode-mv ()
  (interactive)
  (setq target-path (concat current-directory marked-name))
  (start-process "" nil "mv" marked-path target-path)
  (erase-buffer)
  (call-process "ls" nil t nil "-a" "-p" "--group-directories-first" current-directory))

(defun bowser-mode ()
  "A simple file browser"
  (switch-to-buffer "bowser")
  (interactive)
  (kill-all-local-variables)
  (use-local-map bowser-mode-map)
  (erase-buffer)
  (setq current-directory (concat "/home/" (user-login-name) "/"))
  (call-process "ls" nil t nil "-a" "-p" "--group-directories-first" current-directory)
  (setq major-mode 'bowser-mode)
  (setq mode-name "bowser")
  (run-hooks 'bowser-mode-hook))

(provide 'bowser-mode)
