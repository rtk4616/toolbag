" ansible-inventory: Syntax definition for Ansible inventory files
" ================================================================
"
" Download ansible-inventory.vim and put it in ~/.vim/syntax
"
" Copyright (c) 2015 Mike Wilkerson
"
" MIT License
"
" Permission is hereby granted, free of charge, to any person obtaining
" a copy of this software and associated documentation files (the
" "Software"), to deal in the Software without restriction, including
" without limitation the rights to use, copy, modify, merge, publish,
" distribute, sublicense, and/or sell copies of the Software, and to
" permit persons to whom the Software is furnished to do so, subject to
" the following conditions:
"
" The above copyright notice and this permission notice shall be
" included in all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
" EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
" MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
" NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
" LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
" OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
" WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

if exists("b:current_syntax")
    finish
endif

syntax match ansibleinventoryOperator "\v\="
syntax match ansibleinventoryComment "\v#.*$"
syntax match ansibleinventoryGroup "\v^\[.*\]$"
syntax match ansibleinventoryVariable "\v[A-Za-z0-9_.-]+\ze\="
syntax match ansibleinventoryValue "\v\=\S+( |$)" contains=ansibleinventoryOperator
syntax match ansibleinventoryHostname "\v^[A-Za-z0-9_.-]+\ze( |$)"

highlight link ansibleinventoryOperator Operator
highlight link ansibleinventoryComment Comment
highlight link ansibleinventoryGroup Function
highlight link ansibleinventoryVariable Character
highlight link ansibleinventoryValue String
highlight link ansibleinventoryHostname Define

let b:current_syntax = "ansibleinventory"