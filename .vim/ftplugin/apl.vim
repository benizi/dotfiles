fun! APLPipe()
  return get(b:, 'apl_pipe', '/tmp/apl-stdin')
endf

fun! SendToAPL(...)
  cal writefile(a:000, APLPipe())
  redraw
endf

fun! GetVisualSelection()
  let s = getpos("'<")
  let e = getpos("'>")
  let m = visualmode()
  let vis = m == 'v'
  let line = m == 'V'
  let block = m == "\<C-v>"
  let ret = []
  echom 'vis' vis 'line' line 'block' block
  let sv = virtcol(s[1:3])
  echom 'sv' sv
  for i in range(s[1], e[1])
    let sc = (((i == s[1] && vis) || block) ? col([i, sv]) : 1) - 1
    " col([i, ((i == s[1] && vis) || block) ? s[2] - 1 : 1]) - 1
    let ec = col([i, 1 + ((i == e[1] && vis) || block) ? e[2] + 1 : '$'])
    echom 'sc' sc 'ec' ec 'i' i 's[2]' s[2] 'e[2]' e[2]
"¯1↓[1]Mat
    cal add(ret, strpart(getline(i), sc, ec - sc))
  endfor
  return ret
endf

nn ! :.w >> /tmp/apl-stdin<CR>
nn % :w >> /tmp/apl-stdin<CR>
vn ! :cal SendToAPL(GetVisualSelection())<CR>
vn ! :w >> /tmp/apl-stdin<CR>
nn <silent> ? :cal SendToAPL(expand('<cword>'))<CR>

fun! ShowCalc()
  let p = getpos("'<")
  let i = p[1]
  let c = p[2]
  let l = getline(i)
  let v = virtcol([i,c])
  return join(map([string(p), v, strpart(l, c-1)] + GetVisualSelection(), '"{".v:val."}"'), " ")
endf
let &stl='%!ShowCalc()'
