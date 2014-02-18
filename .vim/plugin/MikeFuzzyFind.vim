" If the user's version of Vim doesn't support Python, disable the plugin.
if !has('python')
    echom "Python not supported in this instance of Vim... Disabling " . expand('<sfile>:.')
    finish
endif


" Determine the location of the Python files we need to call for this plugin. We
" need to do this at the script level, and not inside a function, or it gives
" weird results.
let s:MikeFuzzyFindScript = fnamemodify(resolve(expand('<sfile>:p')), ':h') . "/MikeFuzzyFind.py"
let s:MikeFindInFilesScript = fnamemodify(resolve(expand('<sfile>:p')), ':h') . "/MikeFindInFiles.py"


" Utility function for opening a file from the results list.
function! FindFileOpen()
    let l:file_path = getline(line('.'))
    wincmd k
    exec "edit ". l:file_path
    wincmd j
endfunction

" Utility function for opening a file from the results list.
function! FindFileOpenNewTab()
    let l:file_path = getline(line('.'))
    exec "tabe ". l:file_path
    exec "normal gT"
endfunction


" Utility function for opening a file and closing the results list.
function! FindFileOpenClose()
    call FindFileOpen()
    call FindFileClose()
endfunction


" Utility function to close the results list.
function! FindFileClose()
    bdelete "Find file (*"
endfunction


function! MikeFileFinder()
    " Run the Python file.
    execute "pyfile " . s:MikeFuzzyFindScript

    " Set up a new split window, 30 lines tall, at the bottom of the screen.
    setl splitbelow
    exec 'silent! 30 new "Search Results"'
    setl noshowcmd
    setl noswapfile
    setl nowrap
    setl nonumber
    setl nospell
    setl cursorline
    setl modifiable
    setl buftype=nofile
    setl bufhidden=delete
    for file_path in l:toReturn
        call append(line('$'), l:file_path)
    endfor
    norm gg"_dd
    setl nomodifiable
    setl noinsertmode

    noremap <silent> <buffer> o        :call FindFileOpenNewTab()<CR>
    map     <silent> <buffer> <CR>     :call FindFileOpenClose()<CR>
    map     <silent> <buffer> <C-c>    :call FindFileClose()<CR>
endfunc


function! MikeFindInFiles()
    " Run the Python file.
    execute "pyfile " . s:MikeFindInFilesScript

    " Set up a new split window, 30 lines tall, at the bottom of the screen.
    setl splitbelow
    exec 'silent! 30 new "Occurances in files"'
    setl noshowcmd
    setl noswapfile
    setl nowrap
    setl nonumber
    setl nospell
    setl cursorline
    setl modifiable
    setl buftype=nofile
    setl bufhidden=delete
    for file_path in l:toReturn
        call append(line('$'), l:file_path)
    endfor
    norm gg"_dd
    setl nomodifiable
    setl noinsertmode

    noremap <silent> <buffer> o        :call FindFileOpenNewTab()<CR>
    map     <silent> <buffer> <CR>     :call FindFileOpenClose()<CR>
    map     <silent> <buffer> <C-c>    :call FindFileClose()<CR>
endfunc
