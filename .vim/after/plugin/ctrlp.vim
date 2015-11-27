if !exists('g:loaded_ctrlp') || !g:loaded_ctrlp
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

command! CtrlPGitLog call ctrlp#init(ctrlp#gitlog#id())
command! CtrlPFileLines call ctrlp#init(ctrlp#filelines#id())

let &cpo = s:save_cpo
unlet s:save_cpo
