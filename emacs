;;
;; ___.dpz .emacs file
;;
;; Creation-date: 28.10.2007.
;;
;; Time-stamp: <2013-09-11 18:05:47 drazen>
;;

(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Use the xterm color initialization code.
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

;; Start server
;; (server-start)

;; Set library path
(setq load-path
      (append (list "~/.emacs.d/lisp")
	      load-path))

;; Add time stamp when saving files
(add-hook 'before-save-hook 'time-stamp)

;; Blinking cursor
(blink-cursor-mode t)

;; No scrollbar
(set-scroll-bar-mode nil)

;; Mouse scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; No toolbar
(tool-bar-mode 0)

;; Set column mode
(setq column-number-mode t)

;; Show matching parenthesis
(show-paren-mode t)

;; Iswitchb minor mode
(iswitchb-mode t)

;; ido mode
;(ido-mode t)

;; Icomplete minor mode
(icomplete-mode t)

;; Disable system bell
(setq visible-bell t)

;; Built-in backup settings
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      tramp-backup-directory-alist backup-directory-alist
      delete-old-versions t
      kept-new-versions 6
      kept-new-versions 2
      version-control t)

;; By default, use spaces
(setq indent-tabs-mode nil)

;; Clear trailing spaces when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Inhibit display of the initial message
(setq inhibit-splash-screen t)

;; Set transient mode (view rectangle between point and mark)
(transient-mark-mode t)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Enable upcase-region
(put 'upcase-region 'disabled nil)

;; Enable downcase-region
(put 'downcase-region 'disabled nil)

;; shell-mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; Start shell when starting emacs
;; (shell)

;; php-mode
;; (autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
;; (add-to-list 'auto-mode-alist
;; 	     '("\\.php[34]\\'\\|\\.php\\'\\|.phtml\\'" . php-mode))

;; python-mode
(add-hook 'python-mode-hook 'hs-minor-mode)
;; (add-hook 'python-mode-hook 'smart-tab-mode)
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; text-mode
;; (add-hook 'text-mode-hook
;; 	  (lambda ()
;; 	    (when (y-or-n-p "Auto Fill mode? ")
;; 	      (turn-on-auto-fill))))

;; javascript-mode (js2)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; auto-complete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/ac-dict")
;; (ac-config-default)

;; autopair
;; (require 'autopair)
;; (autopair-global-mode)

;; mail-mode
(add-to-list 'auto-mode-alist
	     '("\\.*mutt-*\\|.article\\|\\.followup"
	       . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

;; calendar localization
;; (croatian-calendar)
(setq calendar-week-start-day 1
      calendar-day-name-array
      ["Nedjelja" "Ponedjeljak" "Utorak"
       "Srijeda" "Četvrtak" "Petak" "Subota"]
      calendar-month-name-array
      ["Siječanj" "Veljača" "Ožujak" "Travanj"
       "Svibanj" "Lipanj" "Srpanj" "Kolovoz"
       "Rujan" "Listopad" "Studeni" "Prosinac"])

;; recentf-mode
(recentf-mode 1)

;; smart-tab
;; (require 'smart-tab)

;; boxquote
;; (require 'boxquote)

;; sr-speedbar
;; (require 'sr-speedbar)

;; tabbar
; (require 'tabbar)

;; w3m
;; (require 'w3m-load)

;; yasnippet
;(add-to-list 'load-path
;	     "~/.emacs.d/yasnippet")
;(require 'yasnippet)
;(yas/global-mode t)

;; E l i s p   f u n c t i o n s
(defun insert-date-string()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; K e y s
;; dabbrev-expand
(global-set-key (kbd "C-b") 'hippie-expand)
;; save-buffer
;; (global-set-key (kbd "C-s") 'save-buffer)
;; isearch-forward
;; (global-set-key (kbd "<XF86Search>") 'isearch-forward)
;; other-window
(global-set-key (kbd "M-<up>") 'other-window)
(global-set-key (kbd "M-<down>") (lambda () (interactive) (other-window -1)))
;; goto-line
(global-set-key (kbd "M-g") 'goto-line)

;; D i s p l a y
(set-foreground-color "gray10")

;; Dark theme
;; (set-foreground-color "gray90")
;; (set-background-color "gray10")
;; (set-cursor-color "gray80")

;;
;; H e l p   s e c t i o n
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
;; -------------------------------------------------------------------
;; string-insert-rectangle
;; string-rectangle
;;
;; YesOrNop
;;
;; (defalias 'yes-or-no-p 'y-or-n-p)
;;
;; Exiting
;; -------------------------------------------------------------------
;; kill-emacs
;; save-buffers-kill-emacs
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
;; Numbers in registers
;; -------------------------------------------------------------------
;; C-u 1000 C-x r n x
;; Store 1000 in register x
;; C-x r i x
;; Insert the number in register
;; C-x r + x
;; Increment the number by one
;;
;; Registers
;; -------------------------------------------------------------------
;; Store current window configuration
;; C-x r w REGISTER
;; Restore window configuration
;; C-x r j REGISTER
;;
;; What did I just do?
;; -------------------------------------------------------------------
;; M-x view-lossage
;;
;; Display character map
;; -------------------------------------------------------------------
;; M-x list-charset-chars RET unicode-bmp
;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(python-mode-hook (quote (python-setup-brm abbrev-mode turn-on-eldoc-mode imenu-add-menubar-index (lambda nil (define-key python-mode-map "" (quote newline-and-indent))) hs-minor-mode pylint-python-hook)) t)
 '(python-use-skeletons t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))