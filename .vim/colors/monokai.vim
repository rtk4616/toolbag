" Vim color file
"
" Author: Tomas Restrepo <tomas@winterdom.com>
"
" Note: Based on the monokai theme for textmate
" by Wimer Hazenberg and its darker variant
" by Hamish Stuart Macpherson
"
" Modified: Oct. 2012, by Mike Wilkerson
" Note: To ensure a proper background color, I modified the following lines,
" setting ctermbg to none. This way I can set the terminal background color,
" which is #272822 and just use that for the theme:
" ---------------------------------------------------------
" hi CursorLine                   ctermbg=none   cterm=none
" hi CursorColumn                 ctermbg=none
" hi LineNr          ctermfg=250  ctermbg=none
" hi NonText         ctermfg=none ctermbg=none
" ---------------------------------------------------------


hi clear

set background=dark
if version > 580
    " no guarantees for version 5.8 and below, but this makes it stop
    " complaining
    hi clear
    if exists("syntax_on")
        syntax reset
    endif
endif
let g:colors_name="molokai"

if exists("g:molokai_original")
    let s:molokai_original = g:molokai_original
else
    let s:molokai_original = 0
endif


"hi  Boolean      ctermfg=135
hi   Boolean      ctermfg=99
hi   Character    ctermfg=144
"hi  Number       ctermfg=135
hi   Number       ctermfg=99
"hi  String       ctermfg=144
hi   String       ctermfg=221
hi   Conditional  ctermfg=161  cterm=bold
"hi  Constant     ctermfg=135  cterm=bold
hi   Constant     ctermfg=99   cterm=bold
hi   Cursor       ctermfg=16   ctermbg=253
hi   Debug        ctermfg=225  cterm=bold
hi   Define       ctermfg=81
hi   Delimiter    ctermfg=241

hi   DiffAdd         ctermbg=24
hi   DiffChange      ctermfg=181   ctermbg=239
hi   DiffDelete      ctermfg=161   ctermbg=53
hi   DiffText        ctermbg=102   cterm=bold

hi   Directory       ctermfg=118   cterm=bold
hi   Error           ctermfg=219   ctermbg=89
hi   ErrorMsg        ctermfg=199   ctermbg=16   cterm=bold
"hi  Exception       ctermfg=118   cterm=bold
hi   Exception       ctermfg=161   cterm=bold
hi   Float           ctermfg=135
hi   FoldColumn      ctermfg=67    ctermbg=16
hi   Folded          ctermfg=67    ctermbg=16
hi   Function        ctermfg=118
"hi  Function        ctermfg=117
hi   Identifier      ctermfg=208
hi   Ignore          ctermfg=244   ctermbg=232
hi   IncSearch       ctermfg=193   ctermbg=16

hi   Keyword         ctermfg=161   cterm=bold
hi   Label           ctermfg=229   cterm=none
hi   Macro           ctermfg=193
hi   SpecialKey      ctermfg=81

hi   MatchParen      ctermfg=16    ctermbg=208  cterm=bold
hi   ModeMsg         ctermfg=229
hi   MoreMsg         ctermfg=229
hi   Operator        ctermfg=161

" complete menu
hi   Pmenu           ctermfg=81    ctermbg=16
hi   PmenuSel        ctermbg=244
hi   PmenuSbar       ctermbg=232
hi   PmenuThumb      ctermfg=81

hi   PreCondit       ctermfg=161   cterm=bold
hi   PreProc         ctermfg=161
hi   Question        ctermfg=81
hi   Repeat          ctermfg=161   cterm=bold
hi   Search          ctermfg=232   ctermbg=221

" marks column
hi   SignColumn      ctermfg=118   ctermbg=235
hi   SpecialChar     ctermfg=161   cterm=bold
hi   SpecialComment  ctermfg=237   cterm=bold
hi   Special         ctermfg=81    ctermbg=232
hi   SpecialKey      ctermfg=245

hi   Statement       ctermfg=161   cterm=bold
hi   StatusLine      ctermfg=238   ctermbg=253
hi   StatusLineNC    ctermfg=244   ctermbg=232
hi   StorageClass    ctermfg=208
hi   Structure       ctermfg=81
hi   Tag             ctermfg=161
hi   Title           ctermfg=166
" hi  Todo            ctermfg=231   ctermbg=232  cterm=bold
" hi   Todo            ctermfg=232   ctermbg=196  cterm=bold
hi   Todo            ctermbg=none ctermfg=196  cterm=bold

hi   Typedef         ctermfg=81
hi   Type            ctermfg=81    cterm=none
hi   Underlined      ctermfg=244   cterm=underline

hi   VertSplit       ctermfg=244   ctermbg=232  cterm=bold
hi   VisualNOS       ctermbg=241
hi   Visual          ctermbg=241
hi   WarningMsg      ctermfg=238   cterm=bold
hi   WildMenu        ctermfg=81    ctermbg=16

" Commented out so there's no color background
"hi  Normal          ctermfg=252   ctermbg=233
hi   Normal          guibg=#272822

hi   Comment         ctermfg=239
"hi  Comment         ctermfg=59

hi   CursorLine      ctermbg=234   cterm=none
hi   CursorColumn    ctermbg=236
hi   LineNr          ctermfg=238   ctermbg=none
hi   NonText         ctermfg=none  ctermbg=none
