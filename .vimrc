" ===================================================================================================
" BEGIN Misc settings
" ===================================================================================================

" Keep the working directory Vim was opened from, and don't base it on the currently opened file.
set noautochdir

" Use only spaces for tabs.
set expandtab

" Enable line numbers.
set number

" Make the window title be the filename.
set title

" Don't do that sassy smart-indenting.
set nosmartindent

" Don't do that sassy auto-indenting for C code.
set nocindent

" Indentation same as previous line when inserting a new line.
set autoindent

" Wrap text at window boundary by default.
set wrap

" Highlight matches while searching.
set hlsearch

" Ignore case while searching, by default.
set ic

" Enable incremental search by default.
set is

" Show the tab bar by default, even if we only have one tab open.
set showtabline=4

" Default tab stop to 4 chars.
set tabstop=4

" Default soft tab stop to 4 chars.
set softtabstop=4

" Default shiftwidth to 4 chars.
set shiftwidth=4

" Set backspacing mode 2. This allows backspacing with no restrictions.
set bs=2

" 50 tabs at any given time max. The rest get opened as buffers.
set tabpagemax=50

" Show which mode we're currently in.
set showmode

" Enable highlighting of the current line.
set cursorline

" Don't select first autocompletion, and show the menu even if there is only one result.
set completeopt=longest,menuone

" Set up ignored files when searching...
set wildignore+=*.o,*~,*.pyc,**/migrations/**,**/ve/**

" And when in the netrw browser.
let g:netrw_list_hide= '.*\.swp$,.*\.sqlite$,.*\.pyc$'
" let g:netrw_list_hide= '.*\.pyc$'


" ===================================================================================================
" BEGIN Style settings
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
" BEGIN Key bindings
" ===================================================================================================

" List all files in all subdirectories by partial name.
nnoremap <Leader>g :call MikeFileFinder()<CR>

" TODO: Delete this!
nnoremap <Leader>x :call SaveSymbolsToDict()<CR>

" Easy binding to vimgrep in all files.
nnoremap <Leader>f :tabnew <bar> :call MikeGrep()<CR>

" Easy binding to vimgrep in all files.
nnoremap <Leader>r :tabnew <bar> :call GrepForSymbol()<CR>

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
autocmd Filetype yaml setlocal ts=2 sts=2 sw=2
autocmd Filetype html setlocal ts=2 sts=2 sw=2
autocmd FileType python setlocal ts=4 sts=4 sw=4
autocmd FileType cpp setlocal ts=4 sts=4 sw=4
autocmd FileType rst setlocal ts=2 sts=2 sw=2
autocmd FileType markdown setlocal ts=4 sts=4 sw=4


" ===================================================================================================
" Setting up which filetypes are assigned to which file extensions.
"
" File-specific rules should go after the *rc file rule, or else
" they will get superceeded.
" ===================================================================================================
autocmd BufRead,BufNewFile *textile set ft=textile
autocmd BufRead,BufNewFile *rc set ft=sh
autocmd BufRead,BufNewFile .vimrc set ft=vim
autocmd BufRead,BufNewFile *.vert,*.frag set ft=cpp
autocmd BufRead,BufNewFile *.cl set ft=cpp
autocmd BufRead,BufNewFile *.md set ft=markdown
autocmd BufRead,BufNewFile *.sls set ft=yaml
autocmd BufRead,BufNewFile Vagrantfile set ft=ruby
autocmd BufRead,BufNewFile .tm_properties set ft=sh
autocmd BufRead,BufNewFile *.go set ft=go
autocmd BufRead,BufNewFile *.json set ft=javascript
autocmd BufRead,BufNewFile supervisord.conf set ft=dosini

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
