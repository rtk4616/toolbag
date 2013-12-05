" Table of completion specifications (a list of lists)...
let s:completions = []
" Function to add user-defined completions...
function! AddCompletion (left, right, completion, restore)
    call insert(s:completions, [a:left, a:right, a:completion, a:restore])
endfunction
let s:NONE = ""
" Table of completions...
"                    Left            Right            Complete with...              Move left this many spaces when done
"                    ==============  ===============  ============================  ====================================
call AddCompletion(  '{',            s:NONE,          "}",                           1    )
call AddCompletion(  '{',            '}',             "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '\[',           s:NONE,          "]",                           1    )
call AddCompletion(  '\[',           '\]',            "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '(',            s:NONE,          ")",                           1    )
call AddCompletion(  '(',            ')',             "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<',            s:NONE,          ">",                           1    )
call AddCompletion(  '<a',           s:NONE,          " href=\"\"></a>",             6    )
call AddCompletion(  '<p',           s:NONE,          "></p>",                       4    )
call AddCompletion(  '<p>',          '</p>',          "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<li',          s:NONE,          "></li>",                      5    )
call AddCompletion(  '<li>',         '</li>',         "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<ol',          s:NONE,          "></ol>",                      5    )
call AddCompletion(  '<ol>',         '</ol>',         "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<ul',          s:NONE,          "></ul>",                      5    )
call AddCompletion(  '<ul>',         '</ul>',         "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<pre',         s:NONE,          "></pre>",                     6    )
call AddCompletion(  '<pre>',        '</pre>',        "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<html',        s:NONE,          "></html>",                    7    )
call AddCompletion(  '<html>',       '</html>',       "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<head',        s:NONE,          "></head>",                    7    )
call AddCompletion(  '<head>',       '</head>',       "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<style',       s:NONE,          ' type="text/css"></style>',   8    )
call AddCompletion(  '>',            '</style>',      "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<blockquote',  s:NONE,          "></blockquote>",              13   )
call AddCompletion(  '<blockquote>', '</blockquote>', "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<title',       s:NONE,          "></title>",                   8    )
call AddCompletion(  '<title>',      '</title>',      "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<body',        s:NONE,          "></body>",                    7    )
call AddCompletion(  '<body>',       '</body>',       "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '<',            '>',             "\<CR>\<ESC>O\<TAB>",          0    )
call AddCompletion(  '"',            s:NONE,          '"',                           1    )
call AddCompletion(  "'",            s:NONE,          "'",                           1    )
call AddCompletion(  "'",            "'",             s:NONE,                        0    )

" TODO: I dont' think I need this one...
" call AddCompletion(  '"',            '"',             "\\n",                  2    )


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

    " If a matching smart completion has been specified, use that...
    for [left, right, completion, restore] in s:completions
        let pattern = left . curr_pos_pat . right
        if curr_line =~ pattern
            if restore
                return completion . repeat("\<LEFT>", restore)
            endif
            return completion
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
