" ===================================
" Plugin stuff for smart completions.
" ===================================

" Table of completion specifications (a list of lists)...
let s:completions = []
" Function to add user-defined completions...
function! AddCompletion (left, right, completion, restore)
    call insert(s:completions, [a:left, a:right, a:completion, a:restore])
endfunction
let s:NONE = ""
" Table of completions...
"                    Left        Right       Complete with...                                                     Restore
"                    ==========  ==========  ===================================================================  =======
call AddCompletion(  '{',        s:NONE,     "}",                                                                  1    )
call AddCompletion(  '{',        '}',        "\<CR>\<C-D>\<ESC>O",                                                 0    )
call AddCompletion(  '\[',       s:NONE,     "]",                                                                  1    )
call AddCompletion(  '\[',       '\]',       "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '(',        s:NONE,     ")",                                                                  1    )
call AddCompletion(  '(',        ')',        "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '<',        s:NONE,     ">",                                                                  1    )
call AddCompletion(  '<a',       s:NONE,     " href=\"\"></a>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>",          0    )
call AddCompletion(  '<p',       s:NONE,     "></p>\<LEFT>\<LEFT>\<LEFT>\<LEFT>",                                  0    )
call AddCompletion(  '<p>',      '</p>',     "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '<li',      s:NONE,     "></li>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>",                          0    )
call AddCompletion(  '<li>',     '</li>',    "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '<ol',      s:NONE,     "></ol>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>",                          0    )
call AddCompletion(  '<ol>',     '</ol>',    "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '<ul',      s:NONE,     "></ul>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>",                          0    )
call AddCompletion(  '<ul>',     '</ul>',    "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '<html',    s:NONE,     "></html>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>",          0    )
call AddCompletion(  '<html>',   '</html>',  "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '<head',    s:NONE,     "></head>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>",          0    )
call AddCompletion(  '<head>',   '</head>',  "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '<title',   s:NONE,     "></title>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>",  0    )
call AddCompletion(  '<title>',  '</title>', "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '<body',    s:NONE,     "></body>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>\<LEFT>",          0    )
call AddCompletion(  '<body>',   '</body>',  "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '<',        '>',        "\<CR>\<ESC>O\<TAB>",                                                 0    )
call AddCompletion(  '"',        s:NONE,     '"',                                                                  1    )
call AddCompletion(  '"',        '"',        "\\n",                                                                1    )
call AddCompletion(  "'",        s:NONE,     "'",                                                                  1    )
call AddCompletion(  "'",        "'",        s:NONE,                                                               0    )

" Implement smart completion magic...
function! SmartComplete ()
    " Remember where we parked...
    let cursorpos = getpos('.')
    let cursorcol = cursorpos[2]
    let curr_line = getline('.')

    " Special subpattern to match only at cursor position...
    let curr_pos_pat = '\%' . cursorcol . 'c'

    " Tab as usual at the left margin...
    if curr_line =~ '^\s*' . curr_pos_pat
        return "\<TAB>"
    endif

    " How to restore the cursor position...
    let cursor_back = "\<C-O>:call setpos('.'," . string(cursorpos) . ")\<CR>"

    " If a matching smart completion has been specified, use that...
    for [left, right, completion, restore] in s:completions
        let pattern = left . curr_pos_pat . right
        if curr_line =~ pattern
            " Code around bug in setpos() when used at EOL...
            if cursorcol == strlen(curr_line)+1 && strlen(completion)==1
                let cursor_back = "\<LEFT>"
            endif

            " Return the completion...
            return completion . (restore ? cursor_back : "")
        endif
    endfor

    " If no contextual match and after an identifier, do keyword completion...
    if curr_line =~ '\k' . curr_pos_pat
        return "\<C-N>"

    " Otherwise, just be a <TAB>...
    else
        return "\<TAB>"
    endif
endfunction

" Remap <TAB> for smart completion on various characters...
inoremap <silent> <TAB>   <C-R>=SmartComplete()<CR>


" ==============================================================
" Plugin stuff for using a local project completions dictionary.
" ==============================================================

" The directory the user was in when they initially opened Vim.
let s:ProjectWorkingDirectory = getcwd() . "/"
" The name of the completions dict file we'll use.
let s:ProjectCompletionsFileName = ".project_completions"
" The full path to the completions dict file.
let s:ProjectCompletionsFile = s:ProjectWorkingDirectory . s:ProjectCompletionsFileName

" If there is a .project_completions file in the current working directory...
if filereadable(s:ProjectWorkingDirectory . s:ProjectCompletionsFileName)
    " ...use dictionaries in completions...
    execute "set complete -=k complete+=k"
    " .. and use the .project_completions file as a completion dictionary.
    execute "set dictionary=" . s:ProjectCompletionsFile
else
    " No .project_completions file in the current working directory.
    " TODO: Do we need to do anything to handle this?
    " echom "Could not find " . s:ProjectCompletionsFile
endif


" ==========================================
" Plugin stuff in an external Python script.
" ==========================================

" Be kind. If the user's version of Vim doesn't support Python, disable the plugin.
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
let s:PythonScriptToImport = fnamemodify(resolve(expand('<sfile>:p')), ':h') . "/plugin_test.py"

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
command! MikePlugin call PluginTest()
