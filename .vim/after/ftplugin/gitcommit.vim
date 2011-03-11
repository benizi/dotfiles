fun! GitDiff(...)
   let rev = (a:0 && a:1) ? a:1 : 'HEAD~1'
   let force = a:0 > 1 ? a:2 : 1
   let lines = map(split(system('git diff '.shellescape(rev)), "\n"), '"# ".v:val')
   let noncomment = filter(range(1,line('$')), 'getline(v:val) !~ "^#"')
   let where = len(noncomment) ? noncomment[-1] : 0
   let b:testing = 1
   if len(lines) > 100 && !force
      return
   endif
   call append(where, lines)
endfun

nmap <Leader>d :call GitDiff()<CR>
aug GitDiff
   au!
   au FileType gitcommit :call GitDiff(0,0)
aug END
