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
