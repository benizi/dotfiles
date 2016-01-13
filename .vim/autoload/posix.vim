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
