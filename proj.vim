

if !exists("s:didScript") 
  let s:didScript = 1
  let g:cfile_path = g:projectScriptDir."/errout"
  exec "lcd ".g:projectScriptDir
  silent !hasktags -c --ignore-close-implementation */**.hs
endif

