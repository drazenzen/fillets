;;
;; ___.dpz .emacs file
;;
;; Creation-date: 28.10.2007.
;;
;; Time-stamp: <2014-09-19 23:08:17 drazen>
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
(require 'sr-speedbar)
(setq speedbar-use-images nil)
(unless window-system
  (xterm-mouse-mode t))
;; custom
(elpy-enable)
(defalias 'workon 'pyvenv-workon)
(require 'ahg)
(require 'autopair)
(autopair-global-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(require 'python-django)
(require 'boxquote)
(require 'w3m-load)
(when window-system
  (require 'bar-cursor)
  (bar-cursor-mode t))

;; Saving buffers
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Visualization
(blink-cursor-mode t)
(set-scroll-bar-mode nil)
(tool-bar-mode 0)
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
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "<f3>") 'isearch-forward)
(global-set-key (kbd "<f2>") 'ido-find-file)
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-w") 'ido-kill-buffer)
(global-set-key (kbd "C-1") 'delete-other-windows)
;; comment-region is by default bounded to M-;
(global-set-key (kbd "M-Č") 'comment-dwim)

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

;; Display
;;
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

;;
;; Help and references
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
 '(elpy-modules (quote (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(ido-separator nil)
 '(python-check-command "flake8"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((t (:foreground "IndianRed" :weight bold))))
 '(eshell-prompt-face ((t (:foreground "IndianRed" :weight bold))) t)
 '(ido-subdir ((t (:foreground "#cd5c5c")))))
