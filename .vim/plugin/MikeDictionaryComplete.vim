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
