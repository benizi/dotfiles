" Run a file test portably
fun! posix#test(op, file)
  let arg = a:op =~ '^-' ? a:op : '-'.a:op
  cal system('test '.arg.' '.shellescape(expand(a:file)))
  return !v:shell_error
endf

" Print a file's owner's username portably
fun! posix#owner(file)
  let e = 'chomp($_ = <>); print +(getpwuid((stat)[4]))[0]'
  return split(system('perl -lwe '.shellescape(e), expand(a:file)), "\n")[0]
endf

" Simulate `mkdir -p` using internal version (if supported) or shell
fun! posix#mkdirp(path, ...)
  let dir = expand(a:path)
  if isdirectory(dir) > 0
    return 1
  end
  let mode = a:0 ? a:1 : 0700
  if exists('*mkdir')
    return mkdir(dir, 'p', mode)
  end
  call system('mkdir -m '.mode.' -p '.shellescape(dir))
  return !v:shell_error
endf
