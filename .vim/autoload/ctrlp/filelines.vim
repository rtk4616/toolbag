" =============================================================================
" File:          autoload/ctrlp/filelines.vim
" Description:   Extension for ctrlp to show lines in current buffer.
" Maintainer:    Mike Wilkerson <github.com/wilkystyle>
" =============================================================================

let g:loaded_ctrlp_filelines = 1
let s:filelines_var = {
	\ 'init': 'ctrlp#filelines#init(s:crfile)',
	\ 'accept': 'ctrlp#filelines#accept',
	\ 'lname': 'filelines',
	\ 'sname': 'glog',
	\ 'type': 'tabs',
	\ 'sort': 0,
	\ }


" Append s:filelines_var to g:ctrlp_ext_vars
if exists('g:ctrlp_ext_vars') && !empty(g:ctrlp_ext_vars)
	let g:ctrlp_ext_vars = add(g:ctrlp_ext_vars, s:filelines_var)
else
	let g:ctrlp_ext_vars = [s:filelines_var]
endif


" This will be called by ctrlp to get the full list of elements
function! ctrlp#filelines#init(bufferName)
    for bufferLine in getbufline(a:bufferName, 1, '$')
        echom bufferLine
    endfor
    return getbufline(a:bufferName, 1, '$')
endfunction


func! ctrlp#filelines#accept(mode, str)
    echom "You chose: " . a:str
    " call ctrlp#exit()
    " redraw
    " let selected_git_commit = system("git show " . split(a:str)[0])
    " tabnew
    " exe "set ft=git"
    " put! =selected_git_commit
    " exe "set nomodifiable"
    " call feedkeys("gg")
endfunc


" Give the extension an ID
let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

" Allow it to be called later
function! ctrlp#filelines#id()
    return s:id
endfunction
