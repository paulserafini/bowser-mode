(defvar bowser-mode-hook nil)

(defvar bowser-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<return>") 'open-selection)
    map)
  "Keymap for bowser")

;;;###autoload
;;;(add-to-list 'auto-mode-alist '("\\.wpd\\'" . wpdl-mode))


(defun open-selection ()
  "Open the selected file or directory"

  (interactive)

  ;; make variables other than current-directory local

  ;; (interactive)

  (if (not (boundp 'current-directory))
      (setq current-directory "/home/paul/"))

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

  (when (member extension images)
    (start-process "" nil "sxiv" selection))

  (when (member extension videos)
    (start-process "" nil "mpv" selection)))

(defun bowser-mode ()
  "A simple file browser"
  (interactive)
  (kill-all-local-variables)
  (use-local-map bowser-mode-map)
  (erase-buffer)
  (call-process "ls" nil t nil "-a" "-p" "--group-directories-first" "/home/paul/")
  (setq major-mode 'bowser-mode)
  (setq mode-name "bowser")
  (run-hooks 'bowser-mode-hook))

(provide 'bowser-mode)

