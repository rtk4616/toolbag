" ===============================
" BEGIN Misc settings
" ===============================
set noautochdir                  " Keep the working directory Vim was opened from, and don't base it on the currently opened file.
set expandtab                    " Use only spaces for tabs.
set number                       " Enable line numbers.
set title                        " Make the window title be the filename.
set nosmartindent                " Don't do that sassy smart-indenting.
set nocindent                    " Don't do that sassy auto-indenting for C code.
set autoindent                   " Indentation same as previous line when inserting a new line.
set wrap                         " Wrap text at window boundary by default.
set hlsearch                     " Highlight matches while searching.
set ic                           " Ignore case while searching, by default.
set is                           " Enable incremental search by default.
set showtabline=4                " Show the tab bar by default, even if we only have one tab open.
set tabstop=4                    " Default tab stop to 4 chars.
set softtabstop=4                " Default soft tab stop to 4 chars.
set shiftwidth=4                 " Default shiftwidth to 4 chars.
set bs=2                         " Set backspacing mode 2. This allows backspacing with no restrictions.
set tabpagemax=50                " 50 tabs at any given time max. The rest get opened as buffers.
set showmode                     " Show which mode we're currently in.
set cursorline                   " Enable highlighting of the current line.
" set complete=k**/*,i             " Make autocomplete pull candidate completeions recursively from all files in the working directory, as well as the current buffer.
set completeopt=longest,menuone  " Don't select first autocompletion, and show the menu even if there is only one result.


" ===============================
" BEGIN Style settings
" ===============================
syntax on

if has("gui_running")
    set guifont=menlo:h12
    colo proton_mike
    set background=light
else
    set background=dark
    colo monokai
endif

autocmd Syntax * syntax sync minlines=1000


" ===============================
" BEGIN Key bindings
" ===============================

" List all files in all subdirectories by partial name.
nnoremap <Leader>g :call MikeListFilesByPartialName()<CR>

" Easy binding to vimgrep in all files.
nnoremap <Leader>f :tabnew <bar> :call MikeGrep()<CR>

" Vimgrep for word under the cursor!
nnoremap <Leader>d :call MikeGrepForWordUnderCursor()<CR>

" Show all occurrences of a pattern in the current file.
nnoremap <Leader>l :call MikeFindAllOccurrencesInFile()<CR>

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
autocmd BufRead,BufNewFile .tm_properties set ft=sh

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
    exe "on"
endfunc

function! MikeGrepForWordUnderCursor()
    exe "vimgrep /" . expand("<cword>") . "/j **"
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


" =============================
" START THE NEOCOMPLCACHE STUFF
" =============================

let g:acp_enableAtStartup = 0
" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" Enable heavy features.
" Use camel case completion.
"let g:neocomplcache_enable_camel_case_completion = 1
" Use underbar completion.
"let g:neocomplcache_enable_underbar_completion = 1

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
    let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplcache#undo_completion()
inoremap <expr><C-l>     neocomplcache#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplcache#smart_close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplcache#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e>  neocomplcache#cancel_popup()
" Close popup by <Space>.
"inoremap <expr><Space> pumvisible() ? neocomplcache#close_popup() : "\<Space>"

" For cursor moving in insert mode(Not recommended)
"inoremap <expr><Left>  neocomplcache#close_popup() . "\<Left>"
"inoremap <expr><Right> neocomplcache#close_popup() . "\<Right>"
"inoremap <expr><Up>    neocomplcache#close_popup() . "\<Up>"
"inoremap <expr><Down>  neocomplcache#close_popup() . "\<Down>"
" Or set this.
"let g:neocomplcache_enable_cursor_hold_i = 1
" Or set this.
"let g:neocomplcache_enable_insert_char_pre = 1

" AutoComplPop like behavior.
"let g:neocomplcache_enable_auto_select = 1

" Shell like behavior(not recommended).
"set completeopt+=longest
"let g:neocomplcache_enable_auto_select = 1
"let g:neocomplcache_disable_auto_complete = 1
"inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplcache_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplcache_omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'

" Enable fuzzy completion.
let g:neocomplcache_enable_fuzzy_completion=1
let g:neocomplcache_auto_completion_start_length=2

" ===========================
" END THE NEOCOMPLCACHE STUFF
" ===========================
