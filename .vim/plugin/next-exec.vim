let s:verbose = 0

fun! s:NextExec(...)
  let f = a:0 ? a:1 : fnamemodify(expand('%'), ':p')
  let nextexec = GetNextExecutable(f)
  if strlen(nextexec)
    let cmd = ':e '.fnameescape(nextexec)
    if s:verbose | echomsg 'exe' cmd | endif
    exe cmd
    return
  endif
  echoerr 'Failed to find next file for: '.f
endfun

fun! GetNextExecutable(fullpath)
  let dir = fnamemodify(a:fullpath, ':p:h')
  let file = fnamemodify(a:fullpath, ':t')
  let seen = 0
  for f in split(globpath(join(split($PATH,':'),','), file), "\n")
    if !seen && f == a:fullpath
      let seen = 1
      continue
    endif
    if seen
      return f
    endif
  endfor
  return ''
endfun

com NextExec call <SID>NextExec()
