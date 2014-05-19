fun! BraceExpandWord(...)
  let sep = a:0 ? a:1 : ' '
  exe 'norm! "xdiW'
  let expanded = system("printf '%s\\n' ".@x)
  let @x = join(split(expanded, "\n"), sep)
  exe "norm! a\<C-R>=@x\<Esc>"
endf

nm <C-w>x :call BraceExpandWord()<CR>
nm <C-w>X :call BraceExpandWord("\n")<CR>
