" ------------------------------------------------------------------------------
" vim-plug stuff.
" ------------------------------------------------------------------------------
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes.
Plug 'kien/ctrlp.vim'
Plug 'godlygeek/csapprox'
Plug 'w0ng/vim-hybrid'
Plug 'gabrielelana/vim-markdown'
Plug 'tomtom/tcomment_vim'

call plug#end()


" ------------------------------------------------------------------------------
" Set up any custom vars.
" ------------------------------------------------------------------------------
let g:ctrlp_show_hidden = 1
let g:ctrlp_custom_ignore = '\v[\/](\.git|\.hg|\.svn|/migrations/|/ve/|\.idea|node_modules)$'
let g:ctrlp_working_path_mode = 0
let g:netrw_list_hide= '.*\.swp$,.*\.sqlite$,.*\.pyc$'    " And when in the netrw browser.


" ------------------------------------------------------------------------------
" The Silver Searcher
" ------------------------------------------------------------------------------
if executable('ag')
    " Use ag over grep
    set grepprg=ag\ -i\ --hidden\ --nogroup\ --nocolor

    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s --hidden -l --nocolor -g ""'

    " ag is fast enough that CtrlP doesn't need to cache
    let g:ctrlp_use_caching = 0
endif


" ------------------------------------------------------------------------------
" Style settings
" ------------------------------------------------------------------------------
set background=dark
syntax on
let g:hybrid_use_iTerm_colors = 1

if has("gui_running")
    set guifont=menlo:h12
endif

autocmd Syntax * syntax sync minlines=1000
autocmd BufEnter * colo hybrid


" ------------------------------------------------------------------------------
" Configure Vim
" ------------------------------------------------------------------------------
set autoindent                                            " Indentation same as previous line when inserting a new line.
set background=dark                                       " Prefer dark background.
set bs=2                                                  " Set backspacing mode 2. This allows backspacing with no restrictions.
set completeopt=longest,menuone                           " Don't select first autocompletion, and show the menu even if there is only one result.
set cursorline                                            " Enable highlighting of the current line.
set expandtab                                             " Use only spaces for tabs.
set foldlevel=99                                          " Set the level we automatically fold at to 99 (essentially disabling it).
set foldmethod=indent                                     " Set folding options.
set formatoptions=ql
set hlsearch                                              " Highlight matches while searching.
set ic                                                    " Ignore case while searching, by default.
set is                                                    " Enable incremental search by default.
set linebreak                                             " Better word wrapping.
set noautochdir                                           " Keep the working directory Vim was opened from, and don't base it on the currently opened file.
set nobackup                                              " No backup file.
set nocindent                                             " Don't do that sassy auto-indenting for C code.
set nofoldenable                                          " Disable automatic folding.
set nosmartindent                                         " Don't do that sassy smart-indenting.
set noswapfile                                            " No swap file.
set number                                                " Enable line numbers.
set shiftwidth=4                                          " Default shiftwidth to 4 chars.
set showmode                                              " Show which mode we're currently in.
set showtabline=2                                         " Show the tab bar by default, even if we only have one tab open.
set softtabstop=4                                         " Default soft tab stop to 4 chars.
set tabpagemax=50                                         " 50 tabs at any given time max. The rest get opened as buffers.
set tabstop=4                                             " Default tab stop to 4 chars.
set title                                                 " Make the window title be the filename.
set viminfo=                                              " No viminfo file.
set wildignore+=*.o,*~,*.pyc,**/migrations/**,**/ve/**    " Set up ignored files when searching...
set wrap                                                  " Wrap text at window boundary by default.


" ------------------------------------------------------------------------------
" Keybindings
" ------------------------------------------------------------------------------
map <C-S-P> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

nnoremap <C-a> :pop<CR>
nnoremap <C-j> 6j
nnoremap <C-k> 6k
nnoremap <C-x> :exe "tag ". expand("<cword>")<CR>
nnoremap <F5> :set nonumber!<CR>
nnoremap <F7> :windo set scb!<CR>
nnoremap <F8> :w !diff % -<CR>
nnoremap <S-h> gT
nnoremap <S-l> gt
nnoremap <space> za
nnoremap ZA :qa!<cr>

vnoremap <C-j> 6j
vnoremap <C-k> 6k


" ------------------------------------------------------------------------------
" " Setting up indentation rules for filetypes.
" ------------------------------------------------------------------------------
autocmd FileType make setlocal noexpandtab
autocmd FileType go setlocal noexpandtab ts=4 sts=4 sw=4
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
autocmd Filetype scala setlocal ts=2 sts=2 sw=2
autocmd Filetype yaml setlocal ts=2 sts=2 sw=2
autocmd Filetype html setlocal ts=2 sts=2 sw=2
autocmd FileType python setlocal ts=4 sts=4 sw=4
autocmd FileType cpp setlocal ts=4 sts=4 sw=4
autocmd FileType rst setlocal ts=2 sts=2 sw=2
autocmd FileType markdown setlocal ts=4 sts=4 sw=4
autocmd FileType nginx setlocal ts=2 sts=2 sw=2


" ===================================================================================================
" Set up autocmds
" ===================================================================================================
autocmd BufRead,BufNewFile *.applescript set ft=applescript
autocmd BufRead,BufNewFile *.cl set ft=cpp                     " For OpenCL files.
autocmd BufRead,BufNewFile *.go set ft=go
autocmd BufRead,BufNewFile *.hbs set ft=html
autocmd BufRead,BufNewFile *.json set ft=javascript
autocmd BufRead,BufNewFile *.md set ft=markdown
autocmd BufRead,BufNewFile *.sbt set ft=scala
autocmd BufRead,BufNewFile *.scala set ft=scala
autocmd BufRead,BufNewFile *.sls set ft=yaml
autocmd BufRead,BufNewFile *.textile set ft=textile
autocmd BufRead,BufNewFile *.vert,*.frag set ft=cpp
autocmd BufRead,BufNewFile *rc set ft=sh
autocmd BufRead,BufNewFile .ideavimrc set ft=vim
autocmd BufRead,BufNewFile .tm_properties set ft=sh
autocmd BufRead,BufNewFile .vimrc set ft=vim
autocmd BufRead,BufNewFile /etc/nginx/*.conf set ft=nginx
autocmd BufRead,BufNewFile /etc/nginx/conf/* set ft=nginx
autocmd BufRead,BufNewFile Dockerfile set ft=sh                " Close enough.
autocmd BufRead,BufNewFile Vagrantfile set ft=ruby
autocmd BufRead,BufNewFile supervisord.conf set ft=dosini
autocmd BufWritePre * :%s/\s\+$//e                             " Delete all trailing whitespace on write.
autocmd FileType * setlocal formatoptions-=cro
