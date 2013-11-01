if !has('python')
    echomsg "Python not supported in this instance of Vim... Disabling plugin!"
    finish
endif

function! PluginTest()
    pyfile plugin_test.py
endfunc

command! MikePlugin call PluginTest()
