set nocompatible
filetype off

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/vundle/
call vundle#begin()

" let Vundle manage Vundle
Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
Plugin 'tmhedberg/SimpylFold'
" Bundle 'Valloric/YouCompleteMe'
" Plugin 'scrooloose/syntastic'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'mhinz/vim-signify'
Plugin 'majutsushi/tagbar'
Plugin 'Lawrencium'
Plugin 'desert256.vim'
Plugin 'othree/html5.vim'
Plugin 'rking/ag.vim'
Plugin 'Zenburn'
Plugin 'nvie/vim-flake8'
Plugin 'junegunn/goyo.vim'
Plugin 'davidhalter/jedi-vim'
Plugin 'itchyny/lightline.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'ervandew/supertab'
Plugin 'junkblocker/patchreview-vim'

call vundle#end()

" Enable syntax and colors
syntax enable
set t_Co=256
" Dark background
set background=dark
" Colorscheme
colorscheme desert256
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
" Set backspace behaviour on M$
if has("win32")
	set bs=2
endif
" UTF-8 default file encoding
set enc=utf-8
set fenc=utf-8
" Shortens messages
set shortmess=aI
" Set no visual bell, no beeping
set novisualbell
" Strings to use in list mode
" set listchars=tab:»\ ,trail:·,extends:>,precedes:<,eol:¶
set listchars=tab:»\ ,trail:·,extends:>,precedes:<,eol:¶
" Statusline
set statusline=%t%m\ %r%w\ %Y\ %{&ff}\ %{&fenc}%=hex=\%02.2B\ %l:%v\ %p%%
" Always show statusline
set laststatus=2
" Show linenumbers
set nonu
" Set swap directory
set directory=~/.vim/tmp
" Set backup directory
set backup
set backupdir=~/.vim/saves
" Map leader
let mapleader=" "

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
		set guifont=Liberation\ Mono\ 8,Monospace\ 9
	endif
	set linespace=0
	" hide toolbar and menubar
	set guioptions-=T
	set guioptions-=m
	" hide scrollbars
	set guioptions-=r
	set guioptions-=R
	set guioptions-=l
	set guioptions-=L
	" no mouse blink
	set guicursor=a:blinkon0
	colorscheme navajo
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
autocmd BufRead,BufNewFile *.py set tabstop=8 softtabstop=4 shiftwidth=4 expandtab
autocmd BufRead,BufNewFile *.py set makeprg=flake8\ %
autocmd BufRead,BufNewFile *.py set textwidth=120
autocmd BufRead,BufNewFile *.py set ai smarttab smartindent
autocmd FileType python map <buffer> <F9> :call Flake8()<CR>
let python_highlight_all=1
" Virtualeenv support
py << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
    project_base_dir = os.environ['VIRTUAL_ENV']
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    execfile(activate_this, dict(__file__=activate_this))
EOF

" HTML Specific:
" ==============
autocmd BufRead *.htm set ts=2|set sw=2|set et|set sts=2
autocmd BufRead *.html set ts=2|set sw=2|set et|set sts=2

" CSS Specific:
" =============
autocmd BufRead *.css set ts=4|set sw=4|set et|set sts=4

" JS Specific:
" ============
autocmd BufNewFile,BufRead *.js set tabstop=2 softtabstop=2 shiftwidth=2

" PHP Specific:
" =============
autocmd BufRead *.php set ts=4|set sw=4|set et|set sts=4
autocmd BufRead *.php set makeprg=php\ -l\ %
autocmd BufRead *.php let php_sql_query=1|let php_folding=1

" NFO Specific:
" =============
au BufReadPre *.nfo call SetFileEncodings('cp437')
au BufReadPost *.nfo call RestoreFileEncodings()

" Abbreviation:
" =============

" Completition:
" =============
set completeopt=longest,menuone

" NERDTree:
" =========
let NERDTreeIgnore=['\.pyc$', '\~$']

" SimpylFold:
" ===========
let g:SimpylFold_fold_docstring = 0
let g:SimpylFold_docstring_preview = 1

" YouCompleteMe:
" ==============
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>

" Syntastic:
" ==========
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
" 
" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0
" 
" let g:syntastic_python_checkers = ['flake8']
" 
" let g:syntastic_error_symbol = "\u2717"     " ✗
" let g:syntastic_warning_symbol = "\u26A0"   " ⚠

" Ctrlp:
" ======
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'

" Airline:
" ========
" let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#tabline#left_sep = ' '
" let g:airline#extensions#tabline#left_alt_sep = '|'
" let g:airline_powerline_fonts = 1
" if !exists('g:airline_symbols')
" 	let g:airline_symbols = {}
" endif
" let g:airline_symbols.space = "\ua0"
"
" if !exists('g:airline_symbols')
"   let g:airline_symbols = {}
" endif
" 
" " unicode symbols
" let g:airline_left_sep = '»'
" let g:airline_left_sep = '▶'
" let g:airline_right_sep = '«'
" let g:airline_right_sep = '◀'
" let g:airline_symbols.crypt = '🔒'
" let g:airline_symbols.linenr = '␊'
" let g:airline_symbols.linenr = '␤'
" let g:airline_symbols.linenr = '¶'
" let g:airline_symbols.maxlinenr = '☰'
" let g:airline_symbols.maxlinenr = ''
" let g:airline_symbols.branch = '⎇'
" let g:airline_symbols.paste = 'ρ'
" let g:airline_symbols.paste = 'Þ'
" let g:airline_symbols.paste = '∥'
" let g:airline_symbols.spell = 'Ꞩ'
" let g:airline_symbols.notexists = '∄'
" let g:airline_symbols.whitespace = 'Ξ'
" 
" let g:airline_theme='distinguished'

" Lightline:
" ==========
let g:lightline = {
	\ 'colorscheme': 'jellybeans',
	\ 'component': {
	\	'readonly': '%{&readonly?"x":""}',
	\	},
	\ }
set noshowmode

" Tagbar:
" =======
nmap <F3> :TagbarToggle<CR>

" Ag:
" ===
let g:ag_working_path_mode='r'

" Flake8:
" =======
let g:flake8_show_quickfix=1
let g:flake8_quickfix_height=7
let g:flake8_show_in_gutter=1
let g:flake8_show_in_file=1
highlight link Flake8_Error Error
highlight link Flake8_Warning WarningMsg
highlight link Flake8_Complexity WarningMsg
highlight link Flake8_Naming WarningMsg
highlight link Flake8_PyFlake WarningMsg

" Default Mappings:
" =================
nnoremap <F12> :Ex<CR>
nmap <F2> :NERDTreeToggle<CR>

nnoremap <space> za

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
