;;
;; ___.dpz .emacs file
;;
;; Creation-date: 28.10.2007.
;;
;; Time-stamp: <2014-10-14 16:49:47 drazen>
;;

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
(cua-mode 1)
(ido-mode t)
(setq ido-enable-flex-matching t)
(recentf-mode 1)
(setq speedbar-use-images nil)
(unless window-system
  (xterm-mouse-mode t))
;; custom
(elpy-enable)
(defalias 'workon 'pyvenv-workon)
(require 'ahg)
(require 'sr-speedbar)
(require 'autopair)
;; (autopair-global-mode)
(add-hook 'c-mode-common-hook '(lambda () (autopair-mode)))
(add-hook 'python-mode-hook '(lambda () (autopair-mode)))
(add-hook 'lisp-mode-hook '(lambda () (autopair-mode)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (autopair-mode)))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(setq web-mode-comment-style 1)
(add-hook 'web-mode-hook
          (lambda ()
            (set-fill-column 100)))     ; use 100 column rule for html files
(require 'boxquote)
(require 'w3m-load)
(when window-system
  (require 'bar-cursor)
  (bar-cursor-mode t))

;; Indentation
(setq-default indent-tabs-mode nil)             ; use spaces everywhere
(setq-default python-indent-offset 4)

;; Saving buffers
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Visualization
(blink-cursor-mode t)
(set-scroll-bar-mode nil)
(tool-bar-mode 0)
(menu-bar-mode 1)
(setq column-number-mode t)
(show-paren-mode t)
(setq visible-bell nil)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-splash-screen nil)
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
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "<f2>") 'ido-find-file)
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-'") 'kill-this-buffer)
(global-set-key (kbd "C-w") 'er/expand-region)
(global-set-key (kbd "C-1") 'delete-other-windows)
;; comment-region is by default bounded to M-;
(global-set-key (kbd "M-w") 'mark-word)
(global-set-key (kbd "M-RET") 'hippie-expand)
;; reset elpy meta-<arrows> keys
(eval-after-load "elpy"
  '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))
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

(defun buff-rename ()
  "Rename buffer so that new buffer name is in form: dirname/filename.

Used mainly for Django projects where there are a lot of files with same names.
"
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
  (load-theme 'adwaita))
;;
;; Dark theme
;; (set-foreground-color "gray90")
;; (set-background-color "gray10")
;; (set-cursor-color "gray80")
;;
;; Light theme
;; (set-foreground-color "gray10")
;; (set-background-color "gray95")
;; (set-cursor-color "ForestGreen")
;;
;; (set-background-color "darkslategray")
;; (set-foreground-color "wheat")
;;
;; (add-to-list 'default-frame-alist '(foreground-color . "gray10"))
;; (add-to-list 'default-frame-alist '(background-color . "#f2f2f2"))
;; (add-to-list 'default-frame-alist '(cursor-color . "gray50"))

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
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes (quote ("39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "2588175e0f3591583582a72c465e6d38bd8c99b36daee949ab08f1e758052117" "6ed61522770067a63d7cfe797bede622fa69c975dd0882c7cb706e2ddb464a32" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "caa9a86ff9b85f733b424f520ec6ecff3499a36f20eb8d40e3096dbbe1884069" default)))
 '(elpy-mode-hook nil)
 '(elpy-modules (quote (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(ido-separator nil)
 '(menu-bar-mode nil)
 '(python-check-command "flake8")
 '(scroll-bar-mode nil)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 100 :width normal))))
 '(eshell-prompt ((t (:foreground "IndianRed" :weight bold))) t)
 '(eshell-prompt-face ((t (:foreground "IndianRed" :weight bold))) t)
 '(flymake-errline ((((class color)) (:underline "Sienna"))))
 '(flymake-warnline ((((class color)) (:underline "Peru"))))
 '(ido-subdir ((t (:foreground "IndianRed")))))
