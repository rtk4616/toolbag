" ------------------------------------------------------------------------------
" vim-plug stuff.
" ------------------------------------------------------------------------------
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes.
Plug 'kien/ctrlp.vim'
Plug 'godlygeek/csapprox'
Plug 'evanmiller/nginx-vim-syntax'
Plug 'w0ng/vim-hybrid'
Plug 'gabrielelana/vim-markdown'
Plug 'tomtom/tcomment_vim'
Plug 'scrooloose/syntastic'
Plug 'ervandew/supertab'
Plug 'Raimondi/delimitMate'
Plug 'docunext/closetag.vim'
Plug 'ekalinin/Dockerfile.vim'
Plug 'tpope/vim-fugitive'
Plug 'fatih/vim-go'
Plug 'mxw/vim-jsx'
Plug 'stephpy/vim-yaml'
Plug 'scrooloose/nerdtree'

if (v:version > 703) || (v:version == 703 && has('patch584'))
    Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }
endif

call plug#end()
filetype plugin on


" ------------------------------------------------------------------------------
" Set up any custom vars.
" ------------------------------------------------------------------------------

if !exists('g:neocomplcache_omni_patterns')
    let g:neocomplcache_omni_patterns = {}
endif

let b:delimitMate_expand_cr = 1
let g:CSApprox_verbose_level = 0
let g:NERDTreeWinSize=40
let g:SuperTabDefaultCompletionType="<c-x><c-o>"
let g:SuperTabLongestEnhanced = 1
let g:ctrlp_custom_ignore = '\v[\/](\.git|\.hg|\.svn|/migrations/|/ve/|\.idea|node_modules)$'
let g:ctrlp_show_hidden = 1
let g:ctrlp_working_path_mode = 0
let g:jsx_ext_required = 0
let g:neocomplcache_auto_completion_start_length = 3
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_auto_select = 1
let g:neocomplcache_enable_fuzzy_completion = 1
let g:neocomplcache_fuzzy_completion_start_length = 1
let g:neocomplcache_max_list = 10
let g:neocomplcache_tags_caching_limit_file_size = 5000000
let g:netrw_list_hide= '.*\.swp$,.*\.sqlite$,.*\.pyc$'
let g:syntastic_javascript_checkers = ['eslint']


" ------------------------------------------------------------------------------
" The Silver Searcher
" ------------------------------------------------------------------------------
if executable('ag')
    " Use ag over grep
    set grepprg=ag\ -i\ --hidden\ --nogroup\ --nocolor\ -U

    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s --hidden -l -U --nocolor -g ""'

    " ag is fast enough that CtrlP doesn't need to cache
    let g:ctrlp_use_caching = 0
endif


" ------------------------------------------------------------------------------
" Style settings
" ------------------------------------------------------------------------------
syntax on
set background=dark
colo hybrid

if has("unix") && has("gui_running")
    let s:uname = system("uname")
    if s:uname == "Darwin\n"
        set guifont=menlo:h12
    endif
endif

autocmd Syntax * syntax sync minlines=1000


" ------------------------------------------------------------------------------
" Configure Vim
" ------------------------------------------------------------------------------
set autoindent                                            " Indentation same as previous line when inserting a new line.
set bs=2                                                  " Set backspacing mode 2. This allows backspacing with no restrictions.
set cursorline                                            " Enable highlighting of the current line.
set expandtab                                             " Use only spaces for tabs.
set foldlevel=99                                          " Set the level we automatically fold at to 99 (essentially disabling it).
set foldmethod=indent                                     " Set folding options.
set formatoptions=ql
set hlsearch                                              " Highlight matches while searching.
set completeopt-=preview                                  " Don't show the sassy preview window during autocomplete.
set ignorecase                                            " Ignore case while searching, by default...
set is                                                    " Enable incremental search by default.
set linebreak                                             " Better word wrapping.
set noautochdir                                           " Keep the working directory Vim was opened from, and don't base it on the currently opened file.
set nobackup                                              " No backup file.
set nocindent                                             " Don't do that sassy auto-indenting for C code.
set nofoldenable                                          " Disable automatic folding.
set nojs                                                  " Don't add spaces when joining lines.
set nosmartindent                                         " Don't do that sassy smart-indenting.
set noswapfile                                            " No swap file.
set number                                                " Enable line numbers.
set shiftwidth=4                                          " Default shiftwidth to 4 chars.
set showmode                                              " Show which mode we're currently in.
set showtabline=2                                         " Show the tab bar by default, even if we only have one tab open.
set smartcase                                             " Don't ignore case if the search string contains any capital letters.
set softtabstop=4                                         " Default soft tab stop to 4 chars.
set tabpagemax=50                                         " 50 tabs at any given time max. The rest get opened as buffers.
set tabstop=4                                             " Default tab stop to 4 chars.
set tags=tags                                             " Explicitly set the tags file so we don't get the same tags file twice.
set title                                                 " Make the window title be the filename.
set viminfo=                                              " No viminfo file.
set wildignore+=*.o,*~,*.pyc,**/migrations/**,**/ve/**    " Set up ignored files when searching...
set wrap                                                  " Wrap text at window boundary by default.


" ------------------------------------------------------------------------------
" Keybindings
" ------------------------------------------------------------------------------
map <C-S-P> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" inoremap <expr><silent> <CR> <SID>SpecialCRFunction()

" -----------------------------------------------------------------------------
" Not sure the best way to handle these bindings...
" Ideally, they would only be active for filetypes that have a YCM semantic
" completer.
" -----------------------------------------------------------------------------
" nnoremap <C-a> :pop<CR>
" nnoremap <C-x> :exe "tag ". expand("<cword>")<CR>
nnoremap <C-a> <C-O>
nnoremap <C-x> :YcmCompleter GoToDefinitionElseDeclaration<CR>

nnoremap <C-j> 6j
nnoremap <C-k> 6k
nnoremap <C-o> :CtrlPTag<cr>
nnoremap <F5> :set nonumber!<CR>
nnoremap <F6> :set paste!<CR>
nnoremap <F7> :windo set scb!<CR>
nnoremap <F8> :w !diff % -<CR>
nnoremap <leader>\ :NERDTreeToggle<CR>
nnoremap <S-h> gT
nnoremap <S-l> gt
nnoremap <leader>e :Errors<cr>
nnoremap <leader>f :call MikeGrep()<CR>
nnoremap <leader>l :call MikeFindAllOccurrencesInFile()<CR>
nnoremap <leader>r :SyntasticReset<cr>
nnoremap <space> za
nnoremap ZA :qa!<cr>
nnoremap <leader>m :call CreateMarkdownTOC()<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gl :call MikeFugitiveGitLog()<cr>

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
autocmd FileType python setlocal ts=4 sts=4 sw=4 indentexpr=
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


" ===================================================================================================
" Custom helper functions
" ===================================================================================================

" function! s:SpecialCRFunction()
"     return pumvisible() ? neocomplcache#close_popup() : "\<CR>"
" endfunction

function! CreateMarkdownTOC()
    normal ggyG
    tabnew
    normal p
    silent exe "%!markdown_toc.py"
    exe "1d"
endfunc

function! MikeFugitiveGitLog()
    call inputrestore()
    exe "silent Glog!"
    exe "cope"
    exe "redraw!"
endfunc

function! MikeGrep()
    call inputsave()
    let l:theQuery = input('Search for: ')
    call inputrestore()
    exe "silent grep! '" . l:theQuery . "'"
    exe "cope"
    exe "redraw!"
endfunc

function! MikeFindAllOccurrencesInFile()
    call inputsave()
    let l:thePattern = input('Pattern to find: ')
    call inputrestore()
    echo "\n\n"
    exe "g/".l:thePattern."/p"
    echo "\n\n"
endfunc
