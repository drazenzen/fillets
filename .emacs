;;
;; ___.dpz .emacs file
;;
;; Creation-date: 28.10.2007.
;;
;; Time-stamp: <2015-04-22 11:33:48 drazen>
;;

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

;; Set library path
(setq load-path
      (append (list "~/.emacs.d/lisp")
	      load-path))

;; Backup settings
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      tramp-backup-directory-alist backup-directory-alist
      delete-old-versions t
      kept-new-versions 4
      version-control t)

;; builtin
;; cua
;; immediately remap default cua key map C-<return> to C-S-<return>
(setq cua-rectangle-mark-key (kbd "C-S-<return>"))
(cua-mode 1)
;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
;; recentf
(recentf-mode 1)
;; term
(unless window-system
  (xterm-mouse-mode t))
;; dired
(add-hook 'dired-mode-hook '(lambda () (hl-line-mode)))
;; custom
(elpy-enable)
(defalias 'workon 'pyvenv-workon)
;; mercurial
(require 'ahg)
(global-set-key (kbd "<f8>") 'ahg-status)
;; speedbar
(require 'sr-speedbar)
(setq speedbar-use-images nil)
;; autopair
(require 'autopair)
;; (autopair-global-mode)
(add-hook 'c-mode-common-hook '(lambda () (autopair-mode)))
(add-hook 'python-mode-hook '(lambda () (autopair-mode)))
(add-hook 'lisp-mode-hook '(lambda () (autopair-mode)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (autopair-mode)))

;; web
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(setq web-mode-comment-style 1)
(add-hook 'web-mode-hook
          (lambda ()
            (set-fill-column 120)))     ; use 120 column rule for html files

(require 'boxquote)
(require 'w3m-load)

(when window-system
  ;; (require 'bar-cursor)
  ;; (bar-cursor-mode nil)
  )

;; Indentation
(setq-default indent-tabs-mode nil)             ; use spaces everywhere
;; (setq-default python-indent-offset 4)

;; Saving buffers
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Visualization
(blink-cursor-mode)
(setq blink-cursor-blinks 2)
(scroll-bar-mode 0)
(tool-bar-mode)
(menu-bar-mode)
;; (ruler-mode)
(setq column-number-mode t)
(show-paren-mode t)
(setq visible-bell t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(transient-mark-mode t)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(windmove-default-keybindings 'meta)

;; Mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Keys
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; comint keys
;; comint is a minor-mode for dealing with interpreter commands in buffer
;; Default to cycle commands is M-p and M-n, this setup use up and down keys
(define-key comint-mode-map (kbd "M-n") 'comint-next-input)
(define-key comint-mode-map (kbd "M-p") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
;; More used in other programs
;; (global-set-key (kbd "C-s") 'save-buffer)
;; (global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-r") 'query-replace-regexp)
;; (global-set-key (kbd "<f2>") 'ido-find-file)
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-'") 'kill-this-buffer)
(global-set-key (kbd "C-w") 'er/expand-region)
(global-set-key (kbd "C-1") 'delete-other-windows)
;; comment-region is by default bounded to M-;
(global-set-key (kbd "M-w") 'mark-word)
;; reset C-RET key
(global-set-key [(control return)] 'hippie-expand)
(global-set-key (kbd "C-S-c") 'comment-or-uncomment-region)
;; reset elpy meta-<arrows> keys
;; (eval-after-load "elpy"
;;   '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
;;      (define-key elpy-mode-map (kbd key) nil)))
(defun set-newline-and-indent ()
  (local-set-key (kbd "C-m") 'newline-and-indent))
(defun set-hs-keys ()
  (local-set-key (kbd "<kp-add>") 'hs-show-block)
  (local-set-key (kbd "C-<kp-add>") 'hs-show-all)
  (local-set-key (kbd "<kp-subtract>") 'hs-hide-block)
  (local-set-key (kbd "C-<kp-subtract>") 'hs-hide-all))
(defun set-docstring-fill-column()
  (set-fill-column 72))
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'set-hs-keys)
(add-hook 'python-mode-hook 'set-docstring-fill-column)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Functions
;; Use the xterm color initialization code.
;; From http://www.emacswiki.org/emacs/GnuScreen
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

(defun insert-date-string()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;; From: http://stackoverflow.com/questions/435847/emacs-mode-to-edit-json
(defun beautify-json ()
  "Beautify JSON object in region if active, otherwise in whole buffer."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
	(e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region
     b e
     "python -mjson.tool" (current-buffer) t)))


(defun elpy-test-k2-runner (top file module test)
  "Test the project using the Django discover runner.

Use devmanage.py as main script for running the tests, rather
than global django-admin.py script.
This requires Django 1.6 or the django-discover-runner package."
  (interactive (elpy-test-at-point))
  (if module
      (elpy-test-run top
                     "python" "devmanage.py" "test" "--noinput"
                     (if test
                         (format "%s.%s" module test)
                       module))
    (elpy-test-run top
                   "python" "devmanage.py" "test" "--noinput")))

(global-set-key (kbd "<f5>") 'elpy-test-k2-runner)

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
  (previous-line)
  )
(global-set-key (kbd "<f7>") 'insert-python-doc-string)

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
(global-set-key (kbd "<f6>") 'buff-rename)

;; Display
;; custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; load theme
(when window-system
  ;(load-theme 'deeper-blue)
  ;(set-cursor-color "orange")
  )
;;
;; Dark theme
(set-foreground-color "gray90")
(set-background-color "black")
(set-cursor-color "gray80")

(add-to-list 'default-frame-alist '(foreground-color . "gray90"))
(add-to-list 'default-frame-alist '(background-color . "gray5"))
(add-to-list 'default-frame-alist '(cursor-color . "gray80"))
;;
;; Light theme
;; (set-foreground-color "gray10")
;; (set-background-color "gray95")
;; (set-cursor-color "ForestGreen")
;;
;; (add-to-list 'default-frame-alist '(foreground-color . "gray10"))
;; (add-to-list 'default-frame-alist '(background-color . "#f2f2f2"))
;; (add-to-list 'default-frame-alist '(cursor-color . "gray50"))
;;
;; (set-background-color "darkslategray")
;; (set-foreground-color "wheat")
;;

;; Calendar localization
(setq calendar-week-start-day 1
      calendar-day-name-array
      ["Nedjelja" "Ponedjeljak" "Utorak"
       "Srijeda" "Četvrtak" "Petak" "Subota"]
      calendar-month-name-array
      ["Siječanj" "Veljača" "Ožujak" "Travanj"
       "Svibanj" "Lipanj" "Srpanj" "Kolovoz"
       "Rujan" "Listopad" "Studeni" "Prosinac"])

;; Help and references
;;
;; Dired
;; ------------------------------------------------------------------
;;
;; Use same dired buffer for multiple dirs
;;
;; Key:
;; i
;;
;; :)
;;
;; Remove hook
;; ------------------------------------------------------------------
;;
;; e.g.
;; (add-hook 'web-mode-hook
;;          (lambda ()
;;            (set-fill-column 100)))
;; (remove-hook 'web-mode-hook (lambda nil (set-fill-column 120)))
;;
;;
;; Reload / Revert multiple buffers
;; ------------------------------------------------------------------
;; example reason:
;; Added new hook for some mode and all buffers in that mode need to
;; be updated.
;;
;; M-x ibuffer or C-x C-b
;;
;; In *Ibuffer* window:
;;
;; % m (mark all buffers by ther major mode, using regexp)
;; V (revert marked buffers to activate newly added hook :)
;;
;;
;; Load library
;; ------------------------------------------------------------------
;;
;; M-x load-library library
;;
;;
;; Rectangles
;; ------------------------------------------------------------------
;; Key  	Meaning
;; C-x r k 	Kill rectangle
;; C-x r d 	Delete rectangle
;; C-x r y 	Yank rectangle
;; C-x r c 	Clear rectangle
;;
;; Commands
;;
;; string-insert-rectangle
;; string-rectangle
;;
;;
;; YesOrNop
;; ------------------------------------------------------------------
;;
;; (defalias 'yes-or-no-p 'y-or-n-p)
;;
;;
;; Dired
;; -------------------------------------------------------------------
;; (setq dired-recursive-deletes 'top)
;; nil means no recursive deletes. (default)
;; 'top means ask for each directory at the top level
;; but delete subdirectories without asking.
;; 'always means delete recursively without asking.
;; Warning: The Surgeon General has determined that this
;; may be hazardous to your health.
;; Anything else means ask for each directory (including subdirectories).
;;
;;
;; Numbers in registers
;; -------------------------------------------------------------------
;; C-u 1000 C-x r n x
;; Store 1000 in register x
;; C-x r i x
;; Insert the number in register
;; C-x r + x
;; Increment the number by one
;;
;;
;; Registers
;; -------------------------------------------------------------------
;; Store current window configuration
;; C-x r w REGISTER
;; Restore window configuration
;; C-x r j REGISTER
;;
;;
;; What did I just do?
;; -------------------------------------------------------------------
;;
;; M-x view-lossage
;;
;;
;; Display character map
;; -------------------------------------------------------------------
;;
;; M-x list-charset-chars RET unicode-bmp
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "orange red" "forest green" "yellow" "cornflower blue" "medium orchid" "light sky blue" "white"])
 '(custom-safe-themes
   (quote
    ("efd849c804148b88536914ccdee08285fd7376e2e3334522c9afc00fd7e594da" "cd40ef6720ba9716743322b1684c622090e58d90d3c46e2b3eeec7b8ef5d5817" "0ae977e603e99d89c80d679377bfed4a904317968bd885ee063455cee01728d3" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "2588175e0f3591583582a72c465e6d38bd8c99b36daee949ab08f1e758052117" "6ed61522770067a63d7cfe797bede622fa69c975dd0882c7cb706e2ddb464a32" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "caa9a86ff9b85f733b424f520ec6ecff3499a36f20eb8d40e3096dbbe1884069" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(ido-separator nil)
 '(sql-sqlite-program "sqlite3")
 '(w3m-search-default-engine "duckduckgo")
 '(w3m-use-cookies t)
 '(w3m-use-favicon nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "unknown" :slant normal :weight normal :height 110 :width normal))))
 '(eshell-prompt ((t (:foreground "IndianRed" :weight bold))) t)
 '(eshell-prompt-face ((t (:foreground "IndianRed" :weight bold))) t)
 '(flymake-errline ((((class color)) (:underline "Sienna"))))
 '(flymake-warnline ((((class color)) (:underline "Peru"))))
 '(ido-subdir ((t (:foreground "IndianRed"))))
 '(term-color-blue ((t (:background "cornflower blue" :foreground "cornflower blue")))))
