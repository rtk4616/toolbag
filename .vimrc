" ===============================
" BEGIN Misc settings
" ===============================
set noautochdir        " Keep the working directory Vim was opened from, and don't base it on the currently opened file.
set expandtab          " Use only spaces for tabs.
set number             " Enable line numbers.
set title              " Make the window title be the filename.
set nosmartindent      " Don't do that sassy smart-indenting.
set nocindent          " Don't do that sassy auto-indenting for C code.
set autoindent         " Indentation same as previous line when inserting a new line.
set wrap               " Wrap text at window boundary by default.
set hlsearch           " Highlight matches while searching.
set ic                 " Ignore case while searching, by default.
set is                 " Enable incremental search by default.
set showtabline=4      " Show the tab bar by default, even if we only have one tab open.
set tabstop=4          " Default tab stop to 4 chars.
set softtabstop=4      " Default soft tab stop to 4 chars.
set shiftwidth=4       " Default shiftwidth to 4 chars.
set bs=2               " Set backspacing mode 2. This allows backspacing with no restrictions.
set tabpagemax=50      " 50 tabs at any given time max. The rest get opened as buffers.
set showmode           " Show which mode we're currently in.
set cursorline         " Enable highlighting of the current line.


" ===============================
" BEGIN Style settings
" ===============================
syntax on

if has("gui_running")
    set guifont=menlo:h12
    set background=dark
    colo jellybeans
else
    set background=dark
    colo monokai
endif

autocmd Syntax * syntax sync minlines=1000


" ===============================
" BEGIN Key bindings
" ===============================

" Easy quit all.
nnoremap ZA :qa!<cr>

" Easy switching of tabs.
nnoremap <S-h> gT
nnoremap <S-l> gt

" See a tree of all files and directories starting with the current one.
nnoremap <F1> :tabnew <bar> :r!tree -f .<CR>dkggdd

" Open a new tab with the tree of the current working directory.
nnoremap <F2> :tabnew <bar> :r!find . \| grep -i ''<left>

" Easy binding to vimgrep in all files.
nnoremap <F3> :tabnew <bar> :call MikeGrep()<CR>

" See the difference between the saved version of the current file, and the unsaved changes.
nnoremap <F4> :w !diff % -<CR>

" Allow toggling of number/nonumber mode with F5
nnoremap <F5> :set nonumber!<CR>

" Allow toggling of paste/nopaste mode with F6
nnoremap <F6> :set invpaste paste?<CR>

" Bind toggling paste mode on/off to F6
set pastetoggle=<F6>

" Not currently used, but here for possible future use.
" let g:EasyMotion_leader_key = '<Leader>'


" -------------------------------------------
" Setting up indentation rules for filetypes.
" -------------------------------------------
autocmd FileType make setlocal noexpandtab
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
autocmd Filetype yaml setlocal ts=2 sts=2 sw=2
autocmd Filetype html setlocal ts=2 sts=2 sw=2
autocmd FileType python setlocal ts=4 sts=4 sw=4
autocmd FileType cpp setlocal ts=4 sts=4 sw=4


" -----------------------------------------------------------------
" Setting up which filetypes are assigned to which file extensions.
"
" File-specific rules should go after the *rc file rule, or else
" they will get superceeded.
" -----------------------------------------------------------------
autocmd BufRead,BufNewFile *rc set filetype=sh
autocmd BufRead,BufNewFile .vimrc set filetype=vim
autocmd BufRead,BufNewFile *.vert,*.frag set filetype=cpp
autocmd BufRead,BufNewFile *.cl set filetype=cpp
autocmd BufRead,BufNewFile *.sls set filetype=yaml
autocmd BufRead,BufNewFile Vagrantfile set ft=ruby

" Delete trailing whitespace on save.
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
