fun! ScriptNumber(name)
  redir => names
    sil! scriptnames
  redir END
  for line in split(names, '\n')
    let line = substitute(line, '^\s\+', '', '')
    let [number, filename] = split(line, ':\s\+')
    if strlen(filename) < strlen(a:name)
      continue
    end
    if filename[-(strlen(a:name)):-1] == a:name
      return str2nr(number)
    end
  endfo
  return ''
endf
