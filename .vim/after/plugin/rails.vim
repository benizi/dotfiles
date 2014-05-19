fun! s:LoadOrNew(cmd)
  let v:errmsg = ''
  sil! exe a:cmd
  let err = v:errmsg
  if !strlen(err)
    return
  endif
  let file = matchstr(err, '^E345:.*"\zs[^"]\+\ze".*$')
  if strlen(file)
    exe 'new' fnameescape(file)
  else
    echomsg "Couldn't find file in:" err
    let g:lasterr = err
  endif
endf

com! RSorNew :call <SID>LoadOrNew('RS')
