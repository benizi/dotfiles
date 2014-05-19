fun! MarkDownHeaderFold(...)
   let lnum = a:0 ? a:1 : v:lnum
   let pound = strlen(matchstr(getline(lnum), '^#\+'))
   if pound
      return '>'.pound
   end
   if getline(lnum + 1) =~ '^=\+\s*$'
      return '>1'
   end
   if getline(lnum + 1) =~ '^-\+\s*$'
      return '>2'
   end
   return '='
endfun

setl fdm=expr fde=MarkDownHeaderFold()
