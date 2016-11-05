;;
;; drazenzen .emacs file
;;
;; Creation-date: 2007-10-28
;; Time-stamp: <2016-11-05 16:49:52 drazen>
;;

;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

;; Ref: http://stackoverflow.com/questions/10092322/
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (pack)
     (if (package-installed-p pack)
	 nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " pack))
	   (package-install pack)
	 pack)))
   packages))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(ensure-package-installed 'popwin 'company 'elpy 'web-mode 'ahg
			  'visual-regexp 'expand-region 'ag
			  'rainbow-delimiters
			  'smart-mode-line)

;; custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; library
(setq load-path
      (append (list "~/.emacs.d/lisp")
	      load-path))
(load-file (concat
	    user-emacs-directory
	    (convert-standard-filename "lisp/") "functions.el"))

;; common
(blink-cursor-mode -1)
(scroll-bar-mode)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq column-number-mode t)
(show-paren-mode t)
(setq visible-bell t)
(setq ring-bell-function		; reduce flashing
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort
			abort-recursive-edit
			exit-minibuffer
			keyboard-quit))
	  (ding))))
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq indicate-empty-lines t)
(setq inhibit-startup-screen t)
(transient-mark-mode t)
(delete-selection-mode t)

;; enable commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; backups
(setq backup-by-copying t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "saves")))
      tramp-backup-directory-alist backup-directory-alist
      delete-old-versions t
      kept-new-versions 4
      version-control t)

;; clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;; tramp
;; http://www.emacswiki.org/emacs/TrampMode
(setq tramp-default-method "ssh")

;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; term
(unless window-system
  (xterm-mouse-mode t))

;; dired
(add-hook 'dired-mode-hook
	  '(lambda () (hl-line-mode))
	  (lambda ()
	    (define-key dired-mode-map (kbd "<mouse-2>")
	      'dired-find-file)))

;; saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; python
(elpy-enable)
(defalias 'workon 'pyvenv-workon)
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'set-hs-keys)
(add-hook 'python-mode-hook
          (lambda () (add-hook 'before-save-hook 'delete-trailing-whitespace)))

;; sql
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; mercurial
(require 'ahg)
(global-set-key (kbd "M-4") 'ahg-status)

;; web
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'") ("django" . "\\.jinja\\'")))
(setq web-mode-comment-style 2)
(add-hook 'web-mode-hook
          (lambda ()
            (set-fill-column 120)))     ; use 120 column rule for html files
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2))) ; 2 spaces by default
;; (setq web-mode-enable-current-element-highlight nil)
;; (setq web-mode-enable-current-column-highlight t)

;; todo: javascript
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; visual-regexp
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

;; ag
(setq ag-highlight-search t)

;; popwin
(require 'popwin)
(popwin-mode 1)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; indentation
;; (setq-default indent-tabs-mode nil)             ; use spaces everywhere

;; saving buffers
(add-hook 'before-save-hook 'time-stamp)
(setq require-final-newline t)

;; prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq revert-without-query '(".*"))

;; mouse
(setq mouse-wheel-scroll-amount '(2 ((shift) . 4))
      mouse-wheel-progressive-speed nil ; nil = no acceleration in scrolling
      mouse-yank-at-point t)

;; keys
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [S-return] 'open-lines)
;; comint keys
;; comint is a minor-mode for dealing with interpreter commands in buffer
;; Default to cycle commands is M-p and M-n, this setup use up and down keys
(define-key comint-mode-map (kbd "M-n") 'comint-next-input)
(define-key comint-mode-map (kbd "M-p") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-1") 'ag)
(global-set-key (kbd "M-2") 'find-file-in-project)
(global-set-key (kbd "M-3") 'rgrep)
(global-set-key (kbd "<C-M-mouse-1>") 'hs-toggle-hiding)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C--") 'comment-or-uncomment-region)
(global-set-key (kbd "C-S-k") 'delete-line)
;; reset C-RET key
(global-set-key [(control return)] 'hippie-expand)
;; reset elpy meta-<arrows> keys
(eval-after-load "elpy"
  '(cl-dolist (key '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))
;; rebind pop-tag-mark
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<C-tab>") 'other-window)
;; django test runner
(global-set-key (kbd "<f5>") 'elpy-k2-django-runner)
;; recompile
(global-set-key (kbd "S-<f5>") 'recompile)
(global-set-key (kbd "<f7>") 'insert-python-doc-string)
;; speedbar
(global-set-key (kbd "<f6>") 'speedbar-get-focus)

(define-prefix-command 'launcher-map)
(define-key ctl-x-map "l" 'launcher-map)
(define-key launcher-map "c" #'calc)
(define-key launcher-map "G" #'rgrep)
(define-key launcher-map "s" #'shell)
(define-key launcher-map "a" #'ansi-term)

(require 'expand-region)
(global-set-key (kbd "C-d") 'er/expand-region)

;; theme
(my-light-frame)
;; smart mode line
(setq sml/theme 'dark)
(sml/setup)

;; calendar localization
(setq calendar-week-start-day 1
      calendar-day-name-array
      ["Nedjelja" "Ponedjeljak" "Utorak"
       "Srijeda" "Četvrtak" "Petak" "Subota"]
      calendar-month-name-array
      ["Siječanj" "Veljača" "Ožujak" "Travanj"
       "Svibanj" "Lipanj" "Srpanj" "Kolovoz"
       "Rujan" "Listopad" "Studeni" "Prosinac"])
