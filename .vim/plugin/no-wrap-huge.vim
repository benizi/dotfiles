" if any lines are longer than 180 chars, set nowrap
" (probably viewing a fixed-width file)
fun! NoWrapReallyLongLines()
   if line('$') < 2
      return
   endif
   for i in range(1,min([10, line('$')]))
      if strlen(getline(i)) > 180
         se nowrap
      endif
   endfor
endfun

aug NoWrapReallyLongLines
   au!
   au BufReadPost * call NoWrapReallyLongLines()
aug END
