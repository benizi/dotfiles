fun! MarkDownHeaderFold(...)
   let lnum = a:0 ? a:1 : v:lnum
   let pound = strlen(matchstr(getline(lnum), '^#\+'))
   return pound ? '>'.pound : '='
endfun

setl fdm=expr fde=MarkDownHeaderFold()
