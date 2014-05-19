let s:sup = split("⁰¹²³⁴⁵⁶⁷⁸⁹", '\zs')

fun! SuperscriptToNumber(sup)
  let tot = 0
  for digit in split(copy(a:sup), '\zs')
    let tot = tot * 10
    let tot = tot + index(s:sup, digit)
  endfo
  return tot
endf

fun! NumberToSuperscript(num)
  let str = ''
  for digit in split(copy(a:num), '\zs')
    let str .= s:sup[str2nr(digit)]
  endfo
  return str
endf

fun! SuperscriptIncrement(str)
  return NumberToSuperscript(SuperscriptToNumber(a:str) + 1)
endfu
