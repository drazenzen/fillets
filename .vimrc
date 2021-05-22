" .vimrc

" Global:
" =======
if !has('nvim')
    unlet! skip_defaults_vim
    source $VIMRUNTIME/defaults.vim
endif
if has("termguicolors")
    set termguicolors
endif
syntax on
set hls
set incsearch
set ignorecase
set smartcase
set mouse=a
set ruler
set showcmd
set hidden
set backspace=indent,eol,start
set enc=utf-8
set fenc=utf-8
set shortmess=aI
set novisualbell
set listchars=tab:▸\ ,space:·,trail:·,extends:>,precedes:<,eol:¬ "¶
set statusline=
set statusline+=%-3.3n\                         " buffer number
set statusline+=%f\                             " filename
set statusline+=%h%m%r%w                        " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}]    " file type
set statusline+=%=                              " right align remainder
" set statusline+=0x%-8B                        " character value
set statusline+=%-14(%l,%c%V%)                  " line, character
set statusline+=%<%P                            " file position"
set laststatus=2
set number
set norelativenumber
set directory=~/.vim/tmp
set backup
set backupdir=~/.vim/saves
if !has('nvim')
    set cryptmethod=blowfish2
endif
" let mapleader=","
" let maplocalleader="_"
set wildmenu
set wildmode=full
set history=500
set foldmethod=indent
set foldlevel=99
set noequalalways   " after split or close do not make windows equal
set title

" Functions:
" ==========

" Open new tab with output of hg diff command
function! HgDiff()
    tabnew
    setlocal buftype=nofile ft=diff bufhidden=hide noswapfile
    read ! hg diff
    0delete
endfunction

" Open new tab with output of git diff command
function! GitDiff()
    tabnew
    setlocal buftype=nofile ft=diff bufhidden=hide noswapfile
    read ! git diff
    0delete
endfunction

" Do not close window when deleting buffer
function! BufferClose()
    let l:cur_buf_num = bufnr("%")
    let l:alt_buf_num = bufnr("#")
    if buflisted(l:alt_buf_num)
        buffer #
    else
        bnext
    endif

    if bufnr("%") == l:cur_buf_num
        new
    endif

    if buflisted(l:cur_buf_num)
        execute("bdelete! ".l:cur_buf_num)
    endif
endfunction

" Python:
" =======
augroup ft_py
    autocmd!
    autocmd BufRead,BufNewFile *.py set ts=8 sts=4 shiftwidth=4 expandtab
    autocmd BufRead,BufNewFile *.py set makeprg=flake8\ %
    autocmd BufRead,BufNewFile *.py set ai smarttab smartindent
    autocmd BufRead,BufNewFile *.py set foldlevel=99
    autocmd BufWritePre *.py :%s/\s\+$//e
    autocmd FileType python nnoremap <buffer> <f2> Oimport pudb; pudb.set_trace()<ESC>
    autocmd FileType python inoremap <buffer> <f2> import pudb; pudb.set_trace()
augroup END
let g:python_highlight_all=1

" Shell:
" ======
augroup ft_sh
    autocmd!
    autocmd BufRead,BufNewFile *.sh set ts=8 sts=4 shiftwidth=4 expandtab
    autocmd BufRead,BufNewFile *.sh set makeprg=shellcheck\ %
    autocmd BufRead,BufNewFile *.sh set ai smarttab smartindent
    autocmd BufRead,BufNewFile *.sh set foldlevel=99
augroup END

" Sql:
" ====
augroup ft_sql
    autocmd!
    autocmd BufRead,BufNewFile *.sql set ts=4 sts=4 shiftwidth=4 expandtab
    autocmd BufRead,BufNewFile *.sql set ai smarttab smartindent
augroup END

" ReST:
" =====
augroup ft_rst
    autocmd!
    autocmd FileType rst set tabstop=8 sts=4 sw=4 expandtab
    autocmd FileType rst set makeprg=rst2html\ %
augroup END

" Lisp:
" =====
augroup ft_lisp
    autocmd!
    autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END

" HTML:
" =====
augroup ft_html
    autocmd!
    autocmd BufRead,BufNewFile *.htm set ts=2|set sw=2|set et|set sts=2
    autocmd BufRead,BufNewFile *.html set ts=2|set sw=2|set et|set sts=2
    " autocmd BufWritePre *.html :normal gg=G
    " autocmd FileType html nnoremap <buffer> <localleader>f Vatzf
    autocmd BufNewFile,BufRead *.html setlocal filetype=htmldjango 
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
    autocmd BufNewFile,BufRead *.js set ts=4 sts=4 shiftwidth=4
    autocmd FileType javascript set makeprg=jshint\ %
    autocmd FileType javascript nnoremap <buffer> <f2> Oconsole.log();<ESC>hi
    autocmd FileType javascript inoremap <buffer> <f2> console.log();<ESC>hi
augroup END

" Ruby:
" =====
augroup ft_rb
    autocmd!
    autocmd BufNewFile,BufRead *.rb set ts=2|set sw=2|set et|set sts=2
    autocmd BufRead,BufNewFile *.rb set foldlevel=99
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
function! SetFileEncodings(encodings)
    let b:myfileencodingsbak=&fileencodings
    let &fileencodings=a:encodings
endfunction

function! RestoreFileEncodings()
    let &fileencodings=b:myfileencodingsbak
    unlet b:myfileencodingsbak
endfunction

augroup ft_nfo
    autocmd!
    au BufReadPre *.nfo call SetFileEncodings('cp437')
    au BufReadPost *.nfo call RestoreFileEncodings()
augroup END

" Diff:
" =====
if has("patch-8.1.0360")
    set diffopt+=internal,algorithm:patience
endif

" GUI Specific:
" =============
if has("gui_running")
    if has("win32")
        set guifont=Fixedsys:h9
    elseif has("gui_macvim")
        set guifont=Menlo:h14
    else
        set guifont=Fira\ Code\ Retina\ 9.5,Monospace\ 10
    endif
    set linespace=1
    set guioptions-=m
    set guioptions-=T
    set guioptions-=r
    set guioptions-=R
    set guioptions-=L
    set guicursor=a:blinkon0    " no mouse blink
endif

" Plugins:
" ========
" Load matchit.vim
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
    runtime! macros/matchit.vim
endif
" Load man.vim
if !exists(':Man')
    runtime! ftplugin/man.vim
endif

" Keymaps:
" ========
nnoremap <leader>v :edit $MYVIMRC<CR>
nnoremap <leader>s :source $MYVIMRC<CR>

map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>
noremap  <silent> <C-S> :update<CR>
vnoremap <silent> <C-S> <C-C>:update<CR>
nnoremap <leader>l :ls<CR>:b<space>
inoremap <silent> <C-S> <C-O>:update<CR>
nnoremap + ddkP
nnoremap _ ddp
nnoremap H ^
nnoremap L g_
vnoremap L g_
" Disable mapping example
" inoremap <left> <nop>

nnoremap <Up> gk
nnoremap <Down> gj

nnoremap <f5> :e!<cr>

" Commands:
" =========
" Create python tags in current working directory
command! MakeTags !ctags -R --fields=+l --languages=python --python-kinds=-iv --exclude=build --exclude=node_modules --exclude=backup -f ./tags .

" vim: expandtab:sw=4:sts=4
