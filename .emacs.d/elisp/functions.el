;; Functions

(defun bash ()
  "ansi-term with bash shell"
  (interactive)
  (ansi-term "/bin/bash"))

(defun open-lines (arg)
  "Moves cursor to the end of current line and then opens line.

See `open-line'"
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

; TODO: permanent colors when opening new frames e.g. C-x 5 2
(defun my-dark-frame()
  "Dark frame colors."
  (interactive)
  (set-foreground-color "gray90")
  (set-background-color "#1B2630")
  (set-cursor-color "gray80")
  (set-mouse-color "white")
  (set-face-background 'region "blue")
  (add-to-list 'default-frame-alist '(foreground-color . "gray90"))
  (add-to-list 'default-frame-alist '(background-color . "#1b2630"))
  (add-to-list 'default-frame-alist '(cursor-color . "gray80"))
  (add-to-list 'default-frame-alist '(mouse-color . "white")))

(defun my-light-frame()
  "Light frame colors."
  (interactive)
  (set-foreground-color "gray5")
  (set-background-color "#f0ebde")
  (set-cursor-color "gray5")
  (set-mouse-color "gray5")
  (set-face-background 'region "navajo white")
  (set-face-attribute 'fringe nil :background "#eeeeec")
  (add-to-list 'default-frame-alist '(foreground-color . "gray5"))
  (add-to-list 'default-frame-alist '(background-color . "#f0ebde"))
  (add-to-list 'default-frame-alist '(cursor-color . "gray5"))
  (add-to-list 'default-frame-alist '(mouse-color . "gray5")))

(defun delete-line ()
  "Delete line under the cursor."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (kill-line 1)))

(defun terminal-init-screen ()
  "Terminal initialization function for screen.

Ref: http://www.emacswiki.org/emacs/GnuScreen"
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

(defun set-hs-keys ()
  "Set simpler hide show keys."
  (local-set-key (kbd "C-C C-f") 'hs-toggle-hiding)
  (local-set-key (kbd "<C-M-mouse-1>") 'hs-toggle-hiding)
  (local-set-key (kbd "<kp-add>") 'hs-show-block)
  (local-set-key (kbd "C-<kp-add>") 'hs-show-all)
  (local-set-key (kbd "<kp-subtract>") 'hs-hide-block)
  (local-set-key (kbd "C-<kp-subtract>") 'hs-hide-all))

(defun insert-date-string()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-date-time-string()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun beautify-json ()
  "Beautify JSON object in region if active, otherwise in whole buffer.

Ref: http://stackoverflow.com/questions/435847/"
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
	(e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region
     b e
     "python -mjson.tool" (current-buffer) t)))

(defun set-newline-and-indent ()
  "Map C-m to newline and indent."
  (local-set-key (kbd "C-m") 'newline-and-indent))

(defun insert-python-doc-string ()
  "Insert empty python doc string."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (insert "\"\"\"")
  (end-of-line)
  (newline-and-indent)
  (newline-and-indent)
  (insert "\"\"\"")
  (previous-line))

(defun elpy-k2-django-runner (top file module test)
  "Test the project using the Django discover runner.

Use devmanage.py as main script for running the tests, rather
than global django-admin.py script.
This requires Django 1.6 or the django-discover-runner package."
  (interactive (elpy-test-at-point))
  (if module
      (elpy-test-run top
                     "python" "devmanage.py" "test" "--noinput" "--keepdb"
                     (if test
                         (format "%s.%s" module test)
                       module))
    (elpy-test-run top
                   "python" "devmanage.py" "test" "--noinput" "--keepdb"
                   )))

(defun buff-rename ()
  "Rename buffer so that new buffer name is in form: dirname/filename."
  (interactive)
  (let ((sname (split-string buffer-file-name "/")))
    ;; simple debug
    ;; (message "Length: %d" (length sname))
    ;; (message "Last: %s" (nth (- (length sname) 1) sname))
    (rename-buffer
     (concat (nth (- (length sname) 2) sname) "/" (nth (- (length sname) 1) sname)))
    (message "New name: %s" (buffer-name))
    ))

(defun filesize (value)
  "Return string representation of size given in VALUE as bytes"
  (interactive "p")
  (if (< value 512000)
      (message "%.2f Kb" (/ value 1024.0)))
  (if (< value 4194304000)
      (message "%.2f Mb" (/ value 1048576.0))
    (message "%.2f Gb" (/ value 1073741824.0))))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
