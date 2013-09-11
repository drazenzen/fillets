" Enable syntax and colors
syntax enable
set t_Co=256
" Highlight searches
set hls
" Incremental search
set incsearch
" Ignore case
set ignorecase
" Make mouse active
set mouse=a
" Show ruler
set ruler
" Show typed command
set showcmd
" Allow buffers to remain modified without writing
set hidden
" Set default internal encoding in M$ and
" Set backspace behaviour
if has("win32")
	set enc=utf-8
	set bs=2
endif
" UTF-8 default file encoding
set fenc=utf-8
" Shortens messages
set shortmess=aI
" Set visual bell, no beeping
set visualbell
" Strings to use in list mode
" set listchars=tab:»\ ,trail:·,extends:>,precedes:<,eol:¶
set listchars=tab:»\ ,trail:·,extends:>,precedes:<,eol:¶
" Statusline
set statusline=%t%m\ %r%w\ %Y\ %{&ff}\ %{&fenc}%=\%l,%v\ %p%%
" Always show statusline
set laststatus=2
" Show linenumbers
set nonu
" Set swap directory
set directory=~/.vim/tmp

" Filetypes:
" ==========
filetype indent on
filetype plugin on

" GUI Specific:
" =============
if has("gui_running")
	"set mousefocus
	if has("win32")
		set guifont=Fixedsys:h9
	endif
	if has("unix")
		set guifont=DeJavu\ Sans\ Mono\ 9,Monospace\ 9
		set linespace=1
		set guioptions-=T
	endif
	" hide scrollbars
	set guioptions-=r
	set guioptions-=R
	set guioptions-=l
	set guioptions-=L
	" no mouse blink
	set guicursor=a:blinkon0
endif

" Common code for encodings
" =========================
function! SetFileEncodings(encodings)
	let b:myfileencodingsbak=&fileencodings
	let &fileencodings=a:encodings
endfunction

function! RestoreFileEncodings()
	let &fileencodings=b:myfileencodingsbak
	unlet b:myfileencodingsbak
endfunction

" Removes trailing spaces in file
" ===============================
function! TrimWhiteSpace()
	normal mZ
	%s/\s\+$//e
	if line("'Z") != line(".")
		echo "Stripped whitespace!\n"
	endif
	normal `Z
endfunction

" Python Specific:
" ================
autocmd BufRead *.py set tabstop=8 softtabstop=4 shiftwidth=4 expandtab
autocmd BufRead *.py set textwidth=78
autocmd BufRead *.py set ai smarttab smartindent

" PHP Specific:
" =============
autocmd BufRead *.php set ts=4|set sw=4|set et|set sts=4
autocmd BufRead *.php set makeprg=php\ -l\ %
autocmd BufRead *.php let php_sql_query=1|let php_folding=1

" HTML Specific:
" ==============
autocmd BufRead *.htm set ts=4|set sw=4|set et|set sts=4
autocmd BufRead *.html set ts=4|set sw=4|set et|set sts=4

" CSS Specific:
" =============
autocmd BufRead *.css set ts=4|set sw=4|set et|set sts=4

" NFO Specific:
" =============
au BufReadPre *.nfo call SetFileEncodings('cp437')
au BufReadPost *.nfo call RestoreFileEncodings()

" Abbreviation:
" =============

" Completition:
" =============
set completeopt=longest,menuone

" Default Mappings:
" =================
nnoremap <F12> :Ex<CR>

nnoremap <C-Tab> :bn<CR>
nnoremap <C-S-Tab> :bp<CR>
nnoremap <silent> <A-Up> :wincmd k<CR>
nnoremap <silent> <A-Left> :wincmd h<CR>
nnoremap <silent> <A-Down> :wincmd j<CR>
nnoremap <silent> <A-Right> :wincmd l<CR>

" Functions:
" ==========
" function! ShowDoc(name)

" Reference:
" ==========
" Use all 256 colors if term support it
" -------------------------------------
" :set t_Co=256
" Or set alias in .bashrc
"
" Change colors :help highlight
"
" highlight Cursor guifg=NONE guibg=#656565 gui=NONE
"
" Long lines and wrapping tuned on, j and k move down/up to next visible line
" ---------------------------------------------------------------------------
nmap j gj
nmap k gk
"
" Make file directory current working directoy
" --------------------------------------------
" :cd %:p:h
"
" Change to directory of current file automatically, but not on other windows
" ---------------------------------------------------------------------------
" autocmd BufEnter * lcd %:p:h
"
" Search for word
" ---------------
" Search for word under cursor = '*'
" search for word under cursor backward = '#'
"
" Number of pixel lines inserted between characters
" -------------------------------------------------
" set linespace=0 or set lsp=0 for no pixel space between lines
" Depends on font which is in use
"
" Set command bar height
" ----------------------
" set cmdheight=1
"
" Characters to fill the statusline and vertical separators
" ---------------------------------------------------------
" :help fillchars
"
" Strings to use in 'list' mode
" -----------------------------
" :help listchars
"
" Minimal number of lines to keep below the cursor
" ------------------------------------------------
" set scrolloff=10
"
" Copy and move commands
" ----------------------
" :reg (sinonim) :dis
" :pu :put [registar]
"
" Deleting whitespace
" -------------------
" :%s/\s\+$//gc (at the end of each line)
" :%s/^\s\+// (at the begging of each line)
"
" Sorting a section
" -----------------
" either use marks or even better
" enter visual mode and select text to be sorted
" issue !sort command
"
" Remove blank lines from file
" ----------------------------
" :g/^$/d
" May need to get rid of trailing whitespace first
" :%s/[ ^I]*$/
" Also look at
" :%!cat -s
" Another solution
" :%s/^[\ \t]*\n//g
"
