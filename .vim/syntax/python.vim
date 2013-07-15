" Vim syntax file
" Language:     Python
" Maintainer:   Mike Wilkerson
" Last Change:  July 15, 2013
" Version:      0.1.0

if exists("b:current_syntax")
  finish
endif

hi link pythonVariable Identifier
syn match pythonVariable /[a-zA-Z0-9\._-]* *=/he=e-1

setlocal iskeyword+=:
syn case ignore
