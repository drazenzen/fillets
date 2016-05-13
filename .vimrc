set nocompatible
filetype off

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/vundle/
call vundle#begin()

" let Vundle manage Vundle
Plugin 'gmarik/vundle'

Plugin 'scrooloose/nerdtree'
Plugin 'tmhedberg/SimpylFold'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'mhinz/vim-signify'
Plugin 'majutsushi/tagbar'
Plugin 'Lawrencium'
Plugin 'othree/html5.vim'
Plugin 'rking/ag.vim'
Plugin 'nvie/vim-flake8'
Plugin 'junegunn/goyo.vim'
Plugin 'davidhalter/jedi-vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'ervandew/supertab'
Plugin 'junkblocker/patchreview-vim'
Plugin 'vim-scripts/bufexplorer.zip'
Plugin 'itchyny/lightline.vim'
Plugin 'nanotech/jellybeans.vim'
Plugin 'Zenburn'
Plugin 'desert256.vim'

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
set listchars=tab:»\ ,trail:·,extends:>,precedes:<,eol:¬ " ¶ ⯈
" Statusline
set statusline=%t%m\ %r%w\ %Y\ %{&ff}\ %{&fenc}%=hex=\%02.2B\ %l:%v\ %p%%
" Always show statusline
set laststatus=2
" Show linenumbers
set nu
" Set swap directory
set directory=~/.vim/tmp
" Set backup directory
set backup
set backupdir=~/.vim/saves
" Map leader
let mapleader=" "
" Wildmenu
set wildmenu
set wildmode=full
" Cmd history
set history=200

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

" Functions:
" ==========
"
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
function! <SID>TrimWhiteSpace()
	" Save last search and cursor position
	let _s = @/
	let l = line(".")
	let c = col(".")
	%s/\s\+$//e
	" clean up
	let @/ = _s
	call cursor(l, c)

	" previous version
	" normal mZ
	" %s/\s\+$//e
	" if line("'Z") != line(".")
	" 	echo "Stripped whitespace!\n"
	" endif
	" normal `Z
endfunction

" Preserve last search and cursor position
" ========================================
function! Preserve(command)
  " Preparation: save last search, and cursor position.
  let _s = @/
  let l = line(".")
  let c = col(".")
  " Do the business:
  execute a:command
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction

" Python:
" =======
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

" ReST:
" =====
autocmd FileType rst set tabstop=8 sts=4 sw=4 expandtab
autocmd FileType rst set makeprg=rst2html\ %

" HTML:
" =====
autocmd BufRead *.htm set ts=2|set sw=2|set et|set sts=2
autocmd BufRead *.html set ts=2|set sw=2|set et|set sts=2

" CSS:
" ====
autocmd BufRead *.css set ts=4|set sw=4|set et|set sts=4

" JS:
" ===
autocmd BufNewFile,BufRead *.js set tabstop=2 softtabstop=2 shiftwidth=2

" PHP:
" ====
autocmd BufRead *.php set ts=4|set sw=4|set et|set sts=4
autocmd BufRead *.php set makeprg=php\ -l\ %
autocmd BufRead *.php let php_sql_query=1|let php_folding=1

" NFO:
" ====
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

" Ctrlp:
" ======
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'

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

" Mappings:
" =========
nnoremap <F12> :Ex<CR>
nmap <F2> :NERDTreeToggle<CR>

nnoremap <space> za

nnoremap <C-Tab> :bn<CR>
nnoremap <C-S-Tab> :bp<CR>
nnoremap <silent> <A-Up> :wincmd k<CR>
nnoremap <silent> <A-Left> :wincmd h<CR>
nnoremap <silent> <A-Down> :wincmd j<CR>
nnoremap <silent> <A-Right> :wincmd l<CR>
nmap <leader>l :set list!<CR>
nmap <leader>$ :call Preserve("%s/\\s\\+$//e")<CR>
nmap <leader>= :call Preserve("normal gg=G")<CR>
nmap j gj
nmap k gk

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
"
" Make file directory current working directoy
" --------------------------------------------
" :cd %:p:h
"
" Change to directory of current file automatically, but not on other windows
" ---------------------------------------------------------------------------
" autocmd BufEnter * lcd %:p:h
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
