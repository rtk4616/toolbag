" If the user's version of Vim doesn't support Python, disable the plugin.
if !has('python')
    echom "Python not supported in this instance of Vim... Disabling " . expand('<sfile>:.')
    finish
endif

" The directory the user was in when they initially opened Vim.
let g:MikeProjectWorkingDirectory = getcwd() . "/"
" The name of the completions dict file we'll use.
let g:MikeProjectCompletionsFileName = ".project_completions"
" The full path to the completions dict file.
let s:ProjectCompletionsFile = g:MikeProjectWorkingDirectory . g:MikeProjectCompletionsFileName

" If there is a .project_completions file in the current working directory...
if filereadable(g:MikeProjectWorkingDirectory . g:MikeProjectCompletionsFileName)
    " ...use dictionaries in completions...
    execute "set complete -=k complete+=k"
    " .. and use the .project_completions file as a completion dictionary.
    execute "set dictionary=" . s:ProjectCompletionsFile
else
    " No .project_completions file in the current working directory.
    " TODO: Do we need to do anything to handle this?
    " echom "Could not find " . s:ProjectCompletionsFile
endif

" Determine the location of the Python file we need to call for this plugin. We
" need to do this at the script level, and not inside a function, or it gives
" weird results.
let s:PythonScriptToImport = fnamemodify(resolve(expand('<sfile>:p')), ':h') . "/MikeSaveSymbolsToDict.py"


function! SaveSymbolsToDict()
    " Run the Python file.
    echom "about to execute " . s:PythonScriptToImport
    execute "pyfile " . s:PythonScriptToImport
endfunc
