fun! TrimTrailingWhitespace(type, ...)
  if a:0  " from Visual mode
    let range = '`<' . a:type . '`>'
  elseif a:type == 'line'
    let range = "'[V']"
  elseif a:type == 'block'
    let range = "`[\<C-V>`]"
  else
    let range = '`[v`]'
  endif
  sil exe "normal! ".range."=\<CR>"
  let cmd = "normal! ".range.":s/\\s\\+$//e\<CR>"
  sil exe cmd
endf

nnoremap <silent> = :set opfunc=TrimTrailingWhitespace<CR>g@
vnoremap <silent> = :<C-U>call TrimTrailingWhitespace(visualmode(), 1)<CR>
nmap <silent> == =$
