" Vim compatibility wrappers

" Simulate `systemlist()` if unavailable
fun! vimcompat#systemlist(...)
  if exists('*systemlist')
    return call('systemlist', a:000)
  end
  let raw = call('system', a:000)
  return map(split(raw, "\n"), 'join(split(v:val,"\x01"),"\n")')
endf
