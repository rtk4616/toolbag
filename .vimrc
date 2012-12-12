" ===============================
" BEGIN Misc settings
" ===============================
"set autochdir
set noautochdir
set expandtab
set number
set title
set nosmartindent
set nocindent
set autoindent
set wrap
set hlsearch
set ic
set is
set showtabline=4
set tabstop=4
set softtabstop=4
set shiftwidth=4
set bs=2
set tabpagemax=50
set showmode


" ===============================
" BEGIN Style settings
" ===============================
syntax on
set background=dark
colo monokai
set guifont=DejaVu\ Sans\ Mono\ 9
autocmd Syntax * syntax sync minlines=1000


" ===============================
" BEGIN Plugin settings
" ===============================
" filetype plugin on


" ===============================
" BEGIN Key bindings
" ===============================
" Easy switching of tabs.
nnoremap <S-h> gT
nnoremap <S-l> gt
vnoremap <S-h> gT
vnoremap <S-l> gt
nnoremap <S-l> gt

" See a tree of all files and directories starting with the current one.
nnoremap <F1> :tabnew <bar> :r!tree -f .<CR>dkggdd

" Open a new tab with the tree of the current working directory.
nnoremap <F2> :tabnew <bar> :r!find . \| grep -i ''<left>

" Easy binding to vimgrep in all files.
nnoremap <F3> :call MikeGrep()<CR>

" See the difference between the saved version of the current file, and the unsaved changes.
nnoremap <F4> :w !diff % -<CR>

" Allow toggling of number/nonumber mode with F5
nnoremap <F5> :set nonumber!<CR>

" Allow toggling of paste/nopaste mode with F6
nnoremap <F6> :set invpaste paste?<CR>

" Bind toggling paste mode on/off to F6
set pastetoggle=<F6>

" Not currently used, but here for possible future
let g:EasyMotion_leader_key = '<Leader>'


" ===============================
" BEGIN File-specific settings
" ===============================
autocmd FileType make setlocal noexpandtab
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
autocmd Filetype yaml setlocal ts=2 sts=2 sw=2
autocmd Filetype html setlocal ts=2 sts=2 sw=2
autocmd FileType python setlocal ts=4 sts=4 sw=4

" File extension-specific commands
autocmd BufRead,BufNewFile *.vert,*.frag set filetype=cpp
autocmd BufRead,BufNewFile *.sls set filetype=yaml
autocmd BufRead,BufNewFile Vagrantfile set ft=ruby

autocmd BufWritePre * :%s/\s\+$//e

" Show syntax highlighting groups for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

function! MikeGrep()
    call inputsave()
    let l:theQuery = input('Search for: ')
    call inputrestore()
    call inputsave()
    let l:theExtension = input('... in files: .')
    call inputrestore()
    exe "vimgrep /" . l:theQuery . "/j **/*." . l:theExtension
    exe "cope"
endfunc
