" =============================================================================
" File:          autoload/ctrlp/gitlog.vim
" Description:   Git log extension for ctrlp.vim.
" Maintainer:    Mike Wilkerson <github.com/wilkystyle>
" =============================================================================

let g:loaded_ctrlp_gitlog = 1
let s:gitlog_commands = ['cmd1', 'cmd2', 'cmd3']
let s:gitlog_var = {
	\ 'init': 'ctrlp#gitlog#init()',
	\ 'accept': 'ctrlp#gitlog#accept',
	\ 'lname': 'gitlog',
	\ 'sname': 'glog',
	\ 'type': 'tabs',
	\ 'sort': 0,
	\ }


" Append s:gitlog_var to g:ctrlp_ext_vars
if exists('g:ctrlp_ext_vars') && !empty(g:ctrlp_ext_vars)
	let g:ctrlp_ext_vars = add(g:ctrlp_ext_vars, s:gitlog_var)
else
	let g:ctrlp_ext_vars = [s:gitlog_var]
endif


" This will be called by ctrlp to get the full list of elements
function! ctrlp#gitlog#init()
    let to_return = system("git log --oneline --no-color")
    return split(to_return, '\n')
endfunction


func! ctrlp#gitlog#accept(mode, str)
    call ctrlp#exit()
    redraw
    let selected_git_commit = system("git show " . split(a:str)[0])
    tabnew
    exe "set ft=git"
    put! =selected_git_commit
    exe "set nomodifiable"
    call feedkeys("gg")
endfunc


" Give the extension an ID
let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

" Allow it to be called later
function! ctrlp#gitlog#id()
    return s:id
endfunction
