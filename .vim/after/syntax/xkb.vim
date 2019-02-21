" add folds to xkb sections
redir => xkbBrace
sil! syn list xkbBrace
redir END

for line in split(xkbBrace, "\n")[1:] " skip '--- Syntax items ---' line
  let region = substitute(substitute(line, ' *xxx', '', ''), ' *$', '', '')
  let region = 'syn region ' . region . ' fold keepend extend'
  exe region
endfor

syn sync clear
syn sync fromstart
se fdl=1
