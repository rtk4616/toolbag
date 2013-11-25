" If the user's version of Vim doesn't support Python, disable the plugin.
if !has('python')
    echom "Python not supported in this instance of Vim... Disabling plugin!"
    finish
endif


function! FindFileOpen()
    let l:file_path = getline(line('.'))
    wincmd k
    exec "edit ". l:file_path
    wincmd j
endfunction

function! FindFileOpenClose()
    call FindFileOpen()
    call FindFileClose()
endfunction

function! FindFileClose()
    bdelete "Find file (*"
endfunction

" Need to do this at the script level, and not inside a function, or it gives weird results.
let s:PythonScriptToImport = fnamemodify(resolve(expand('<sfile>:p')), ':h') . "/MikeFuzzyFind.py"

function! PluginTest()

    " python sys.argv = []
    execute "pyfile " . s:PythonScriptToImport

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

    noremap <silent> <buffer> o        :call FindFileOpen()<CR>
    map     <silent> <buffer> <CR>     :call FindFileOpenClose()<CR>
    map     <silent> <buffer> <C-c>    :call FindFileClose()<CR>

    " echom "Vim was initially opened at " . s:ProjectWorkingDirectory

    " Here's how you would see if there is a file in the current working directory.
    " if filereadable(s:ProjectWorkingDirectory . "/" . ".MikeTest")
    "     echom ".MikeTest exists in this directory!"
    " else
    "     echom "No .MikeTest file in this directory!"
    " endif

endfunc

" TODO: I don't think we need this... Delete when confirmed.
" command! MikePlugin call PluginTest()
