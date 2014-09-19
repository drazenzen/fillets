;; w3m init file

; (setq w3m-home-page "http://emacs-w3m.namazu.org/")
(add-to-list 'w3m-search-engine-alist
	     '("duckduckgo" "https://duckduckgo.com/?q=%s"))
(setq w3m-search-default-engine "duckduckgo")
