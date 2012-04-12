" let s:extra='%{(exists("g:stl_extra")&&len(g:stl_extra))?":".join(map(copy(g:stl_extra),"v:val=~\"^!\"?eval(strpart(v:val,1)):v:val"),","):""}'

fun! StatusLine(actual_curbuf)

  try
  let current = bufnr('') == a:actual_curbuf

  " show tabstop/shiftwidth/et graphically
  let tabstops = '('
  let [ sw, sts, ts ] = &l:et
    \ ? [ '·', '¬', '|' ]
    \ : [ '»', '·', '¬' ]
  let tabstops .= repeat(sw, &l:sw)
  let tabstops .= repeat(sts, (&l:sts != &l:ts) * &l:sts)
  let tabstops .= repeat(sts, (&l:sw != &l:ts) * &l:ts)
  let tabstops .= ')'

  " show fileformat/encoding/eol status
  let ff = ''
  if &l:ff != 'unix'
    let ff .= ' ['.&l:ff.']'
  en
  if !&l:eol
    let ff .= ' [noeol]'
  en
  let enc = strlen(&l:fenc) ? &l:fenc : &l:enc
  if enc != 'utf-8'
    let ff .= ' ['.enc.']'
  en

  " show filetype if not what's expected
  let typ = expand('%:e') == &l:ft ? '' : ' <'.&l:ft.'>'

  " show keymap
  let kmp = strlen(&l:kmp) ? ' ['.&l:kmp.']' : ''

  " show conceal info
  let cole = (exists('&cole') && &l:cole) ? '[cole:'.&l:cole.']' : ''

  let fn = expand('%')
  if !strlen(fn)
    let fn = '[No Name]'
  en
  if !current
    let fn = '--'.fn
  en

  let help = &l:bt == 'help' ? '[Help]' : ''
  let mod = &l:mod ? &l:ma ? '[+]' : '[-]' : ''
  let ro = &l:ro ? '[RO]' : ''
  let [buf,line,col,off] = getpos('.')
  let lines = line('$')
  let wline = winline()
  let top = line - wline
  let bot = top + winheight(0)
  let wwid = winwidth(0)
  if top <= 1 && bot >= lines
    let P = 'All'
  elseif top <= 1
    let P = 'Top'
  elseif bot >= lines
    let P = 'Bot'
  else
    let P = (100 * line / lines).'%'
  en
  let left = fn.typ.' '.help.mod.ro.ff.kmp
  let mid = cole.tabstops
  let right = '('.line.','.col.(off ? '-'.off : '').') '.P.'/'.lines
  let right = &ru ? ' '.right : ''
  while 1
    let stl = left.mid.right
    let size = exists('*strdisplaywidth') ? strdisplaywidth(stl) : strlen(stl)
    if size < wwid
      let mid = repeat(' ', wwid - size).mid
    else
      break
    en
  endw
  return stl
  catch
    return 'Failed to set statusline: err:'.v:errmsg.' ex:'.v:exception
  endt
endf

" default statusline
let &stl='%{StatusLine(actual_curbuf)}'
" %-5.(%{AwkFields#GetVar()}%)'

" default statusline
let s:tabstops='(%{repeat(&l:et?"·":"»",&l:sw)}%{&l:sts==&l:ts?"":repeat(&l:et?"¬":"·",&l:sts)}%{&l:sw==&l:ts?"":repeat(&l:et?"|":"¬",&l:ts)})'
let s:ff='%{&l:ff=="unix"?"":" [".&l:ff."]"}'
let s:ff.='%{&l:eol?"":" [noeol]"}'
let s:enc='strlen(&l:fenc)?&l:fenc:&l:enc'
let s:ff.='%{('.s:enc.')=="utf-8"?"":" [".('.s:enc.')."]"}'
let s:km='%{strlen(&l:kmp)?" [".&l:kmp."]":""}'
let s:typ='%{(expand("%:e")==&l:ft)?"":" <".&l:ft.">"}'
let s:extra='%{(exists("g:stl_extra")&&len(g:stl_extra))?":".join(map(copy(g:stl_extra),"v:val=~\"^!\"?eval(strpart(v:val,1)):v:val"),","):""}'
let s:cole = exists('&cole') ? '%{(!&l:cole)?"":"[cole:".&l:cole."]"}' : ''
"let &stl='%<%f'.s:typ.s:extra.' %h%m%r'.s:ff.s:km.'%='.s:cole.s:tabstops.(&ruler?' ':'').(&ruler ? '%-14.(%l,%c%V%) %P' : '').'/%{line("$")}'
