fun! GitFindCommit(...)
   let line = a:0 && a:1 ? a:1 : line('.')
   " syn name = gitrebaseCommit
   let syntax = map(range(1,len(getline(line))), '{ "col": v:val, "syn": synIDattr(synID(line("."), v:val, 0),"name") }')
   let commit_cols = map(filter(syntax, 'v:val.syn == "gitrebaseCommit"'),'v:val.col')
   let beg = commit_cols[0]
   let end = commit_cols[-1]
   let commit = strpart(getline(line), beg-1, end-beg+1)
   return commit
endfun

fun! GitRebaseLog(...)
   let line = a:0 && a:1 ? a:1 : line('.')
   let extra = a:0 && a:2 ? '--name-status ' : ''
   let commit = GitFindCommit(line)
   let log = map(split(system('git log -1 '.extra.shellescape(commit)), "\n"), '"# ".v:val')
   let log[0] = substitute(log[0], '^# ', '#{{{1 ', '')
   let log[-1] = substitute(log[-1], '^# ', '#}}}1 ', '')
   call append(line, log)
endfun

nmap <buffer> <Leader>a :call GitRebaseLog(0,0)<CR>
nmap <buffer> <Leader>A :call GitRebaseLog(0,1)<CR>
