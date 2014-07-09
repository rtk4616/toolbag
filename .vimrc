" ===================================================================================================
" Misc settings
" ===================================================================================================
set linebreak                                             " Better word wrapping.
set noautochdir                                           " Keep the working directory Vim was opened from, and don't base it on the currently opened file.
set expandtab                                             " Use only spaces for tabs.
set number                                                " Enable line numbers.
set title                                                 " Make the window title be the filename.
set nosmartindent                                         " Don't do that sassy smart-indenting.
set nocindent                                             " Don't do that sassy auto-indenting for C code.
set autoindent                                            " Indentation same as previous line when inserting a new line.
set wrap                                                  " Wrap text at window boundary by default.
set hlsearch                                              " Highlight matches while searching.
set ic                                                    " Ignore case while searching, by default.
set is                                                    " Enable incremental search by default.
set showtabline=2                                         " Show the tab bar by default, even if we only have one tab open.
set tabstop=4                                             " Default tab stop to 4 chars.
set softtabstop=4                                         " Default soft tab stop to 4 chars.
set shiftwidth=4                                          " Default shiftwidth to 4 chars.
set bs=2                                                  " Set backspacing mode 2. This allows backspacing with no restrictions.
set tabpagemax=50                                         " 50 tabs at any given time max. The rest get opened as buffers.
set showmode                                              " Show which mode we're currently in.
set cursorline                                            " Enable highlighting of the current line.
set completeopt=longest,menuone                           " Don't select first autocompletion, and show the menu even if there is only one result.
set wildignore+=*.o,*~,*.pyc,**/migrations/**,**/ve/**    " Set up ignored files when searching...
set foldmethod=indent                                     " Set folding options.
set foldlevel=99                                          " Set the level we automatically fold at to 99 (essentially disabling it).
set nofoldenable                                          " Disable automatic folding.
set viminfo=                                              " No viminfo file.
set nobackup                                              " No backup file.
set noswapfile                                            " No swap file.

" ===================================================================================================
" Some more settings
" ===================================================================================================
let g:ctrlp_show_hidden = 1
let g:ctrlp_custom_ignore = '\v[\/](\.git|\.hg|\.svn|migrations|ve)$'
let g:ctrlp_working_path_mode = 0
let g:netrw_list_hide= '.*\.swp$,.*\.sqlite$,.*\.pyc$'    " And when in the netrw browser.

" ===================================================================================================
" Style settings
" ===================================================================================================
syntax on

if has("gui_running")
    set guifont=menlo:h12
    " colo mikeokai
    colo tomorrow-night
else
    set background=dark
    " colo monokai
    colo tomorrow-night
endif

autocmd Syntax * syntax sync minlines=1000


" ===================================================================================================
" Key bindings
" ===================================================================================================

" Some Syntastic shortcuts.
nnoremap <leader>e :Errors<cr>
nnoremap <leader>c :SyntasticReset<cr>

" Some CtrlP shortcuts.
nnoremap <C-O> :CtrlPTag<cr>

" Use the enter key to select a completion candidate.
inoremap <expr> <CR> pumvisible() ? "\<space>\<bs>" : "\<C-g>u\<CR>"

" Go to tag under cursor.
nnoremap <C-x> :exe "tag ". expand("<cword>")<CR>
nnoremap <C-a> :pop<CR>

" Move up and down by 6 lines
nnoremap <C-k> 6k
nnoremap <C-j> 6j
vnoremap <C-k> 6k
vnoremap <C-j> 6j

" Testing setting ctrl-space and alt-space.
nnoremap <C-space> :echo "You just pressed ctrl-space"<CR>
nnoremap <M-space> :echo "You just pressed alt-space"<CR>

" Set space to open and close folds in normal mode.
nnoremap <space> za

" List all files in all subdirectories by partial name.
nnoremap <Leader>f :call MikeFindInFiles()<CR>

" List all files in all subdirectories by partial name.
" nnoremap <Leader>g :call MikeFileFinder()<CR>

" TODO: Delete this!
nnoremap <Leader>x :call SaveSymbolsToDict()<CR>

" Easy binding to vimgrep in all files.
" nnoremap <Leader>f :tabnew <bar> :call MikeGrep()<CR>

" Easy binding to vimgrep in all files.
" nnoremap <Leader>r :tabnew <bar> :call GrepForSymbol()<CR>

" Vimgrep for word under the cursor!
nnoremap <Leader>d :call MikeGrepForSymbolUnderCursor()<CR>

" Show all occurrences of a pattern in the current file.
nnoremap <Leader>l :call MikeFindAllOccurrencesInFile()<CR>

" Show all occurrences of a pattern in the current file.
nnoremap <Leader>s :call MikeFindAllSymbolsInFile()<CR>

" Easy quit all.
nnoremap ZA :qa!<cr>

" Easy switching of tabs.
nnoremap <S-h> gT
nnoremap <S-l> gt

" See a tree of all files and directories starting with the current one.
nnoremap <F1> :tabnew <bar> :r!tree -f .<CR>dkggdd

nnoremap <F4> :call MikeReplaceInFiles()<CR>

" Allow toggling of number/nonumber mode with F5
nnoremap <F5> :set nonumber!<CR>

" Allow toggling of paste/nopaste mode with F6
nnoremap <F6> :set paste! <bar> :set paste?<CR>

" Allow toggling of paste/nopaste mode with F6
nnoremap <F7> :windo set scb!<CR>

" See the difference between the saved version of the current file, and the unsaved changes.
nnoremap <F8> :w !diff % -<CR>

" Show syntax highlighting groups for word under cursor
map <C-S-P> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Not currently used, but here for possible future use.
" let g:EasyMotion_leader_key = '<Leader>'


" ===================================================================================================
" Setting up indentation rules for filetypes.
" ===================================================================================================
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
" Setting up which filetypes are assigned to which file extensions.
"
" File-specific rules should go after the *rc file rule, or else
" they will get superceeded.
" ===================================================================================================
autocmd BufRead,BufNewFile *rc set ft=sh
autocmd BufRead,BufNewFile Dockerfile set ft=sh
autocmd BufRead,BufNewFile .vimrc set ft=vim
autocmd BufRead,BufNewFile .ideavimrc set ft=vim
autocmd BufRead,BufNewFile *.vert,*.frag set ft=cpp
autocmd BufRead,BufNewFile *.cl set ft=cpp
autocmd BufRead,BufNewFile *.md set ft=markdown
autocmd BufRead,BufNewFile *.sls set ft=yaml
autocmd BufRead,BufNewFile Vagrantfile set ft=ruby
autocmd BufRead,BufNewFile .tm_properties set ft=sh
autocmd BufRead,BufNewFile *.go set ft=go
autocmd BufRead,BufNewFile *.hbs set ft=html
autocmd BufRead,BufNewFile *.json set ft=javascript
autocmd BufRead,BufNewFile *.scala set ft=scala
autocmd BufRead,BufNewFile *.sbt set ft=scala
autocmd BufRead,BufNewFile supervisord.conf set ft=dosini
autocmd BufRead,BufNewFile *.textile set ft=textile
autocmd BufRead,BufNewFile *.applescript set ft=applescript
autocmd BufRead,BufNewFile /etc/nginx/conf/* set ft=nginx
autocmd BufRead,BufNewFile /etc/nginx/*.conf set ft=nginx

" Delete trailing whitespace on save.
autocmd BufWritePre * :%s/\s\+$//e

function! MikeGrep()
    call inputsave()
    let l:theQuery = input('Search for: ')
    call inputrestore()
    call inputsave()
    let l:theExtension = input('... in files: ')
    call inputrestore()
    exe "vimgrep /" . l:theQuery . "/j **/*" . l:theExtension
    exe "cope"
    exe "on"
endfunc

function! GrepForSymbol()
    call inputsave()
    let l:theQuery = input('Search for symbol: ')
    call inputrestore()
    exe "noautocmd vimgrep /^\ *\\(class\\|def\\|func\\).*" . l:theQuery . "/j **/*"
    exe "cope"
    exe "on"
endfunc

function! MikeGrepForSymbolUnderCursor()
    exe "noautocmd vimgrep /^\ *\\(class\\|def\\|func\\).*" . expand("<cword>") . "/j **"
    exe "tabnew"
    exe "cope"
    exe "on"
endfunc

function! MikeListFilesByPartialName()
    call inputsave()
    let l:filePartial = input('Partial filename: ')
    call inputrestore()
    exe "tabnew"
    exe 'normal i'.globpath('.', '**/*' . filePartial . '*')
endfunc

function! MikeReplaceInFiles()
    call inputsave()
    let l:searchFor = input('Replace: ')
    call inputrestore()

    call inputsave()
    let l:replaceWith = input('...with: ')
    call inputrestore()

    call inputsave()
    let l:theExtension = input('... in files: .')
    call inputrestore()

    exe "args **/*" . l:theExtension
    exe "tab ba"
    exe "tabdo %s/" . searchFor . "/" . replaceWith . "/giec"
    exe "tabdo close"
endfunc

function! MikeFindAllOccurrencesInFile()
    call inputsave()
    let l:thePattern = input('Pattern to find: ')
    call inputrestore()
    echo "\n\n"
    exe "g/".l:thePattern."/p"
    echo "\n\n"
endfunc

function! MikeFindAllSymbolsInFile()
    call inputsave()
    let l:thePattern = input('Pattern to find: ')
    call inputrestore()
    echo "\n\n"
    exe "g/^ *\\(class\\|def\\|func!*\\| function!*\\) ".l:thePattern."/p"
    echo "\n\n"
endfunc

function! MDTOC(theHeader)
    let l:partOne = substitute(a:theHeader, ":$", "", "g")
    let l:partTwo = tolower(substitute(substitute(a:theHeader, "[[:punct:]]", "", "g"), " ", "-", "g"))
    return "[" . l:partOne . "](#" . l:partTwo . ")"
endfunc

function! PrefixLines() range
    call inputsave()
    let t = input('Prefix: ')
    call inputrestore()
    exe a:firstline.','.a:lastline 's/^/\=t'
endfunction
