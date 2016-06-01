set nocompatible
filetype off

" Install Vundle
" $ git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle 

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/vundle/
call vundle#begin()

" let Vundle manage Vundle
Plugin 'gmarik/vundle'

Plugin 'tmhedberg/SimpylFold'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'mhinz/vim-signify'
Plugin 'majutsushi/tagbar'
Plugin 'othree/html5.vim'
Plugin 'nvie/vim-flake8'
Plugin 'junegunn/goyo.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'ervandew/supertab'
Plugin 'vim-scripts/bufexplorer.zip'

Plugin 'itchyny/lightline.vim'
Plugin 'nanotech/jellybeans.vim'

call vundle#end()

" Enable syntax and colors
set t_Co=256
syntax enable
colorscheme jellybeans
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
" Show linenumbers and relativenumbers
set nonu
set relativenumber
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
		set guifont=Liberation\ Mono\ 9,Monospace\ 9
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

" Open new tab with output of hg diff command
" ===========================================
function! HgDiff()
	tabnew
	setlocal buftype=nofile ft=diff bufhidden=hide noswapfile
	read ! hg diff
	0delete
endfunction

" Vim:
" ====
nnoremap <leader>v :edit $MYVIMRC<CR>
nnoremap <leader>s :source $MYVIMRC<CR>
let g:netrw_list_hide='^\.,\~$'
let g:netrw_liststyle= 3

" Python:
" =======
augroup ft_py
	autocmd!
	autocmd BufRead,BufNewFile *.py set tabstop=8 softtabstop=4 shiftwidth=4 expandtab
	autocmd BufRead,BufNewFile *.py set makeprg=flake8\ %
	autocmd BufRead,BufNewFile *.py set textwidth=120
	autocmd BufRead,BufNewFile *.py set ai smarttab smartindent
	autocmd BufRead,BufNewFile *.py set foldlevel=99
	autocmd FileType python noremap <buffer> <F9> :call Flake8()<CR>
augroup END
let python_highlight_all=1
let g:SimpylFold_fold_docstring = 0
" Virtualeenv support
" py << EOF
" import os
" import sys
" if 'VIRTUAL_ENV' in os.environ:
"     project_base_dir = os.environ['VIRTUAL_ENV']
"     activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
"     execfile(activate_this, dict(__file__=activate_this))
" EOF

" ReST:
" =====
augroup ft_rst
	autocmd!
	autocmd FileType rst set tabstop=8 sts=4 sw=4 expandtab
	autocmd FileType rst set makeprg=rst2html\ %
augroup END

" HTML:
" =====
augroup ft_html
	autocmd!
	autocmd BufRead,BufNewFile *.htm set ts=2|set sw=2|set et|set sts=2
	autocmd BufRead,BufNewFile *.html set ts=2|set sw=2|set et|set sts=2
augroup END

" CSS:
" ====
augroup ft_css
	autocmd!
	autocmd BufRead,BufNewFile *.css set ts=4|set sw=4|set et|set sts=4
augroup END

" JS:
" ===
augroup ft_js
	autocmd!
	autocmd BufNewFile,BufRead *.js set tabstop=4 softtabstop=4 shiftwidth=4
	autocmd FileType javascript set makeprg=jshint\ %
augroup END

" PHP:
" ====
augroup ft_php
	autocmd!
	autocmd BufRead *.php set ts=4|set sw=4|set et|set sts=4
	autocmd BufRead *.php set makeprg=php\ -l\ %
	autocmd BufRead *.php let php_sql_query=1|let php_folding=1
augroup END

" NFO:
" ====
augroup ft_nfo
	autocmd!
	au BufReadPre *.nfo call SetFileEncodings('cp437')
	au BufReadPost *.nfo call RestoreFileEncodings()
augroup END

" Abbreviations:
" ==============

" Completition:
" =============
set completeopt=longest,menuone

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
nnoremap <F3> :TagbarToggle<CR>

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
nnoremap <F2> :Ex<CR>
noremap <leader>q :bp<CR>
nnoremap <space> za
nnoremap <C-Tab> :bn<CR>
nnoremap <C-S-Tab> :bp<CR>
nnoremap <leader>l :set list!<CR>
nnoremap <leader>$ :call Preserve("%s/\\s\\+$//e")<CR>
nnoremap <leader>= :call Preserve("normal gg=G")<CR>
nnoremap <leader>hd :call HgDiff()<CR>
nnoremap j gj
nnoremap k gk
" Current word to uppercase
nnoremap <leader><c-u> viwUe<esc>
" Put word in quotes
nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>hbi'<esc>lel
" Operator pending
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap in" :<c-u>normal! f"vi"<cr>
onoremap in' :<c-u>normal! f'vi'<cr>
onoremap in[ :<c-u>normal! f[vi[<cr>


