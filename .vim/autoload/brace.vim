let s:specials = { '{': 1, '}': -1, ',': 0 }

let s:lit = 'literal'
let s:ctrl = 'control'

fun! brace#expand(txt, ...)
  let keep_escapes = a:0 && a:1
  let input = a:txt

  " tokenize
  let tokens = [[s:lit, '']]
  while strlen(input)
    let char = input[0:0]
    let escaped = (char == '\') ? 1 : 0
    let val = input[(keep_escapes ? 0 : escaped):escaped]
    let idx = stridx('},{', char)
    let typ = (idx < 0) ? s:lit : s:ctrl
    if typ == s:ctrl
      let val = idx - 1
    end
    cal add(tokens, [typ, val])
    let input = strpart(input, 1 + escaped)
  endwhile

  " find matching 'control' pairs
  let ntoks = len(tokens)
  let endpos = {}
  let choicepoints = {}
  let validcomma = {}
  let validclose = {}
  let stack = []
  let to_check = range(ntoks)
  while len(to_check)
    let to_check = filter(copy(to_check), 'tokens[v:val] == [s:ctrl, 1]')
    let to_check = filter(copy(to_check), '!has_key(endpos, v:val)')
    for spos in to_check
      let commas = []
      let epos = spos+1
      while epos < ntoks
        let token = tokens[epos]
        let [typ, val] = token
        if typ == s:lit
          let epos += 1
          continue
        end
        if val == 0
          cal add(commas, epos)
          let epos += 1
          continue
        end
        if val == 1
          if has_key(endpos, epos)
            let epos = 1 + endpos[epos]
            continue
          end
          break " don't know yet
        end
        " found end
        if len(commas) " valid alternation
          let choicepoints[spos] = copy(commas)
          for cpos in commas
            let validcomma[cpos] = 1
          endfor
          let endpos[spos] = epos
          let validclose[epos] = 1
        else
          let tokens[spos] = [s:lit, '{']
          let tokens[epos] = [s:lit, '}']
        end
        break
      endwhile
      if epos == ntoks " no match found, must be a literal
        let tokens[spos] = [s:lit, '{']
      end
    endfor
  endwhile

  " turn commas and closing brackets not used in alternations back to literals
  for pos in range(ntoks)
    if tokens[pos] == [s:ctrl, 0] && !has_key(validcomma, pos)
      let tokens[pos] = [s:lit, ',']
    elseif tokens[pos] == [s:ctrl, -1] && !has_key(validclose, pos)
      let tokens[pos] = [s:lit, '}']
    end
  endfor

  let links = []
  for pos in range(ntoks)
    cal add(links, {})
    let [typ, val] = tokens[pos]
    if typ == s:lit
      let links[pos] = {(pos+1): val}
    end
  endfor

  for [spos, choices] in items(choicepoints)
    let epos = endpos[spos]
    let links[spos][spos+1] = ''
    for cpos in [spos] + copy(choices)
      let links[spos][cpos+1] = ''
    endfor
    for cpos in copy(choices) + [epos]
      let links[cpos][epos+1] = ''
    endfor
  endfor

  let routes = [[[0], '']]
  let found_new = 1
  while found_new
    let found_new = 0
    let nextroutes = []
    for [path, word] in routes
      let pos = path[-1]
      if pos >= len(links)
        cal add(nextroutes, [path, word])
        continue
      end
      let jumps = sort(map(keys(links[pos]), 'str2nr(v:val)'), {a, b -> a - b})
      for j in jumps
        cal add(nextroutes, [path + [j], word . links[pos][j]])
        let found_new = 1
      endfor
    endfor
    let routes = nextroutes
  endwhile

  return map(routes, 'v:val[-1]')
endf

fun! brace#tests(...)
  let keep = a:0 ? a:1 : 0
  for testcase in [
    \ '{a,b{c{,{d}}e}f',
    \ 'asdf{fd,sa}{,/and{,/more}}',
    \ 'a{{{bcd}e,j}}',
    \ 'a{\{{bcd}e,j}}',
    \ '~/{Downloads,Pictures}/*.{jpg,gif,png}',
    \ 'It{{em,alic}iz,erat}e{d,}, please.',
    \ '{,{,gotta have{ ,\, again\, }}more }cowbell!',
    \ '{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}',
    \ 'a{b{1,2}c',
    \ 'ab{c,d\,e{f,g\h},i\,j{k,l\,m}n,o\,p}qr']
    echo "<< " . testcase . " >>"
    let tokens = brace#expand(testcase, keep)
    for i in range(len(tokens))
      echo "  [" . i . "]:" . string(tokens[i])
    endfor
  endfor
endf
